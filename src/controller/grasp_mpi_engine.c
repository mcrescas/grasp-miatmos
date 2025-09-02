/*
 *  Copyright 2020 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* grasp_mpi_engine.c */

/* Fabrice Ducos <fabrice.ducos@univ-lille1.fr> 2014, 2015, 2020 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <grasp/utils.h>
#include <unistd.h> /* getpid, getppid, alarm, sleep (for simulating processing) */
#include <assert.h>

/* this is for interruptible_task (and also <unistd.h>, already included) */
#include <signal.h>
#include <setjmp.h>
#include <stdbool.h>

#ifdef USE_MPI
#include <mpi.h>
#endif

#ifdef DEBUG_MPI
static int g_debug_level = 1; // set to 2 for more verbose output
#else
static int g_debug_level = 0;
#endif

#include "grasp_mpi_engine.h"

static char g_appname[255 + 1];
static int g_order_payload_max_size;
static int g_result_payload_max_size;

#ifdef USE_MPI
/* 
 * This variables are only used if USE_MPI.
 * They are commented to avoid unused-variable warnings.
 */
static bool g_no_more_order = false;
static int g_num_orders_sent = 0;
static int g_num_orders_performed = 0;
static int g_available_workers = 0;
#endif /* USE_MPI */

static order_callback_t prepare_order_callback; /* routine set by the user for preparing orders */
static task_callback_t perform_task_callback; /* routine set by the user for performing a task */
static collect_callback_t collect_result_callback = NULL; /* routine set by the user for collecting results (will be ignored if set to NULL) */
static progress_info_callback_t progress_info_callback = NULL;

/* The labels in order_t and result_t are not necessary for the processing (only the payload is).
 * They contain strings with readable information about the content of an order or
 * a result. They are useful for debugging.
 */

struct order_t_ {
  unsigned char *message; /* this pointer references a sequence of bytes holding the metadata and the payload to be passed through MPI */
  char *label; /* a reference to message[ORDER_OFFSET_LABEL], for ease of use. */
  void *payload; /* a reference to message[ORDER_OFFSET_PAYLOAD], for ease of use. */
  bool *ptr_no_more_job; /* a reference to message[ORDER_OFFSET_NO_MORE_JOB] (its address, not its value) for ease of use.  */
  size_t *ptr_payload_size; /* a reference to message[ORDER_OFFSET_PAYLOAD_SIZE], for ease of use */
  size_t *ptr_message_size; /* a reference to message[ORDER_OFFSET_MESSAGE_SIZE], for ease of use */
};

struct result_t_ {
  unsigned char *message; /* this pointer references a sequence of bytes holding the metadata and the payload to be passed through MPI */
  char *label; /* a reference to message[RESULT_OFFSET_LABEL] for ease of use */
  void *payload; /* a reference to message[RESULT_OFFSET_PAYLOAD] for ease of use */
  size_t *ptr_payload_size; /* a reference to message[RESULT_OFFSET_PAYLOAD_SIZE], for ease of use */
  size_t *ptr_message_size; /* a reference to message[RESULT_OFFSET_MESSAGE_SIZE], for ease of use */
};

/* const variables (const int or const size_t) can't be used for declaring sizes of global arrays in plain C, so one uses enums (#define's would work too) */
enum {
  ORDER_LABEL_MAX_LEN = 255 /* this does not include the ending null character */
};

enum {
  RESULT_LABEL_MAX_LEN = 255 /* this does not include the ending null character */
};

enum {
  ORDER_WRAPPER_SIZE = 1024
};

enum {
  RESULT_WRAPPER_SIZE = 1024
};

enum {
  ORDER_OFFSET_NO_MORE_JOB = 0,
  ORDER_OFFSET_MESSAGE_SIZE = 8,
  ORDER_OFFSET_PAYLOAD_SIZE = 16,
  ORDER_OFFSET_LABEL = 24, /* the (ORDER_LABEL_MAX_LEN + 1) bytes starting from this offset are reserved for the order label */
  ORDER_OFFSET_PAYLOAD = ORDER_WRAPPER_SIZE
};

enum {
  RESULT_OFFSET_MESSAGE_SIZE = 0,
  RESULT_OFFSET_PAYLOAD_SIZE = 8,
  RESULT_OFFSET_LABEL = 16, /* the (RESULT_LABEL_MAX_LEN + 1) bytes starting from this offset are reserved for the result label */
  RESULT_OFFSET_PAYLOAD = RESULT_WRAPPER_SIZE
};

enum {
  WORKER_STATUS_TAG,
  ORDER_TAG,
  RESULT_IS_READY_TAG
};

enum {
  NO_AVAILABLE_WORKER = -1
};

typedef enum worker_status_t_ {
  WORKER_STATUS_BUSY, /* the worker has received an order and is busy on its task */
  WORKER_STATUS_AVAILABLE /* the worker is available for a new task. */
} worker_status_t;

typedef struct process_info_t_ {
#ifdef USE_MPI
  MPI_Comm communicator;
#endif
  char label[255 + 1];
  int pid;
  int ppid;
  int master_rank;
  int my_rank;
  int num_processes;
  int num_workers;
} process_info_t;

#ifdef USE_MPI

#define print_status(label, status) print_status_aux(__FILE__, __LINE__, label, status)
static void print_status_aux(const char *file, int line, const char *label, const MPI_Status *status) {
  assert(status != NULL);
  int source = status->MPI_SOURCE;
  int tag = status->MPI_TAG;
  int error = status->MPI_ERROR;
  char error_message[MPI_MAX_ERROR_STRING + 1];
  int error_len;
  MPI_Error_string(error, error_message, &error_len);
  if (label != NULL) {
    fprintf(stderr, "%s:%d: %s source: %d tag: %d error: %s\n", file, line, label, source, tag, error_message);
  }
  else {
    fprintf(stderr, "%s:%d: source: %d tag: %d error: %s\n", file, line, source, tag, error_message);
  }
}

static void print_order_aux(const char *file, int line, FILE *stream, const char *print_label, const order_t *order, const process_info_t *process_info) {
  const char *process_info_label;

  assert(stream != NULL);
  assert(order != NULL);

  if (process_info == NULL) {
    process_info_label = "unknown";
  }
  else {
    process_info_label = process_info->label;
  }

  if (print_label == NULL) {
    print_label = "order";
  }

  fprintf(stream, "%s:%d: %s: %s->payload_size: %lu\n", file, line, process_info_label, print_label, (long unsigned) *(order->ptr_payload_size));
  fprintf(stream, "%s:%d: %s: %s->message_size: %lu\n", file, line, process_info_label, print_label, (long unsigned) *(order->ptr_message_size));
  fprintf(stream, "%s:%d: %s: %s->label: %s\n", file, line, process_info_label, print_label, order->label);
  fprintf(stream, "%s:%d: %s: %s->no_more_job: %s\n\n", file, line, process_info_label, print_label, *(order->ptr_no_more_job) ? "true" : "false");
  
}

#define print_order(stream, print_label, order, process_info) print_order_aux(__FILE__, __LINE__, stream, print_label, order, process_info)

static void print_result_aux(const char *file, int line, FILE *stream, const char *print_label, const result_t *result, const process_info_t *process_info) {
  const char *process_info_label;

  assert(stream != NULL);
  assert(result != NULL);

  if (process_info == NULL) {
    process_info_label = "unknown";
  }
  else {
    process_info_label = process_info->label;
  }

  if (print_label == NULL) {
    print_label = "result";
  }

  fprintf(stream, "%s:%d: %s: %s->payload_size: %lu\n", file, line, process_info_label, print_label, (long unsigned) *(result->ptr_payload_size));
  fprintf(stream, "%s:%d: %s: %s->message_size: %lu\n", file, line, process_info_label, print_label, (long unsigned) *(result->ptr_message_size));
  fprintf(stream, "%s:%d: %s: %s->label: %s\n", file, line, process_info_label, print_label, result->label);
}

#define print_result(stream, print_label, result, process_info) print_result_aux(__FILE__, __LINE__, stream, print_label, result, process_info)
#endif /* USE_MPI */

/* this internal routine sets derived fields from an order object to their correct value (derived fields are all the fields but message);
 * to be called each time order is updated
 */
static void set_order_references(order_t *order) {
  assert(order != NULL);
  assert(order->message != NULL);
  
  order->label = (char *) &order->message[ORDER_OFFSET_LABEL];
  order->payload = &order->message[ORDER_OFFSET_PAYLOAD];
  order->ptr_payload_size = (size_t *) &order->message[ORDER_OFFSET_PAYLOAD_SIZE];
  order->ptr_message_size = (size_t *) &order->message[ORDER_OFFSET_MESSAGE_SIZE];
  order->ptr_no_more_job = (bool *) &order->message[ORDER_OFFSET_NO_MORE_JOB];
}

/* this internal routine sets derived fields from a result object to their correct value (derived fields are all the fields but message);
 * to be called each time result is updated
 */
static void set_result_references(result_t *result) {
  assert(result != NULL);
  assert(result->message != NULL);
  
  result->label = (char *) &result->message[RESULT_OFFSET_LABEL];
  result->payload = &result->message[RESULT_OFFSET_PAYLOAD];
  result->ptr_payload_size = (size_t *) &result->message[RESULT_OFFSET_PAYLOAD_SIZE];
  result->ptr_message_size = (size_t *) &result->message[RESULT_OFFSET_MESSAGE_SIZE];
  
}

order_t *grasp_mpi_engine_new_order(const char *label, size_t payload_size, const void *payload) {
  order_t *order;
  size_t message_size = ORDER_WRAPPER_SIZE + payload_size;

  order = calloc(1, sizeof(order_t));
  assert(order != NULL); /* if the memory is exhausted, it's useless to continue */
  
  if (! (payload_size <= g_order_payload_max_size)) {
    fprintf(stderr, "%s:%d: %s: the argument payload_size (%d) is out of range [0..%d]. Note that you can change the upper bound with grasp_mpi_engine_set_order_callback()\n",
	    __FILE__, __LINE__, __func__, (int) payload_size, (int) g_order_payload_max_size);
  }
  assert(payload_size <= g_order_payload_max_size);
  order->message = calloc(1, message_size);
  assert(order->message != NULL);
  set_order_references(order);
  *(order->ptr_no_more_job) = false;
  *(order->ptr_payload_size) = payload_size;
  *(order->ptr_message_size) = message_size;
  
  if (label != NULL) {
    strncpy(order->label, label, ORDER_LABEL_MAX_LEN);
  }
  
  if (payload_size > 0 && payload != NULL) {
    memcpy(order->payload, payload, payload_size);
  }
  
  return order;
}

result_t *grasp_mpi_engine_new_result(const char *label, size_t payload_size, const void *payload) {
  result_t *result;
  size_t message_size = RESULT_WRAPPER_SIZE + payload_size;
  
  result = calloc(1, sizeof(result_t));
  assert(result != NULL);  /* if the memory is exhausted, it's useless to continue */
  
  if (! (payload_size <= g_result_payload_max_size)) {
    fprintf(stderr, "%s:%d: %s: the argument payload_size (%d) is out of range [0..%d]. Note that you can change the upper bound with grasp_mpi_engine_set_task_callback()\n",
	    __FILE__, __LINE__, __func__, (int) payload_size, (int) g_result_payload_max_size);
  }
  
  assert(payload_size <= g_result_payload_max_size);
  result->message = calloc(1, message_size);
  assert(result->message != NULL);
  set_result_references(result);
  *(result->ptr_payload_size) = payload_size;
  *(result->ptr_message_size) = message_size;

  if (label != NULL) {
    strncpy(result->label, label, RESULT_LABEL_MAX_LEN);
  }

  if (payload_size > 0 && payload != NULL) {
    memcpy(result->payload, payload, payload_size);
  }

  return result;
}

void grasp_mpi_engine_delete_order(order_t *order) {
  if (order == NULL) return;
  
  trackmem_free(order->message);
  trackmem_free(order);
}

void grasp_mpi_engine_delete_result(result_t *result) {
  if (result == NULL) return;

  trackmem_free(result->message);
  trackmem_free(result);
}

void grasp_mpi_engine_get_order_label(const order_t *order, size_t label_max_size, char *label) {
  assert(order != NULL);
  assert(label != NULL);
  strncpy(label, (const char *) &order->message[ORDER_OFFSET_LABEL], label_max_size);
}

void grasp_mpi_engine_get_result_label(const result_t *result, size_t label_max_size, char *label) {
  assert(result != NULL);
  assert(label != NULL);
  strncpy(label, (const char *) &result->message[RESULT_OFFSET_LABEL], label_max_size);
}

#ifdef USE_MPI
static void get_process_info(process_info_t *process_info) {
  MPI_Comm communicator = MPI_COMM_WORLD;
  assert(process_info != NULL);
  
  memset(process_info, 0, sizeof(process_info_t));
  process_info->pid = getpid();
  process_info->ppid = getppid();
  MPI_Comm_size(communicator, &process_info->num_processes);
  MPI_Comm_rank(communicator, &process_info->my_rank);

  process_info->communicator = communicator;
  process_info->num_workers = process_info->num_processes - 1;
  process_info->master_rank = process_info->num_processes - 1;

  if (process_info->my_rank == process_info->master_rank) {
    strcpy(process_info->label, "master");
  }
  else {
    sprintf(process_info->label, "worker #%d", process_info->my_rank);
  }
}

static void print_process_info_aux(const char *file, int line, FILE *stream, const char *print_label, const process_info_t *process_info) {
  assert(stream != NULL);
  assert(process_info != NULL);

  if (print_label == NULL) {
    print_label = "process_info";
  }
  
  fprintf(stream, "%s:%d: %s: %s->label:         %s\n", file, line, process_info->label, print_label, process_info->label);
  fprintf(stream, "%s:%d: %s: %s->pid:           %d\n", file, line, process_info->label, print_label, process_info->pid);
  fprintf(stream, "%s:%d: %s: %s->ppid:          %d\n", file, line, process_info->label, print_label, process_info->ppid); 
  fprintf(stream, "%s:%d: %s: %s->master_rank:   %d\n", file, line, process_info->label, print_label, process_info->master_rank);
  fprintf(stream, "%s:%d: %s: %s->my_rank:       %d\n", file, line, process_info->label, print_label, process_info->my_rank);
  fprintf(stream, "%s:%d: %s: %s->num_processes: %d\n", file, line, process_info->label, print_label, process_info->num_processes);
  fprintf(stream, "%s:%d: %s: %s->num_workers:   %d\n\n", file, line, process_info->label, print_label, process_info->num_workers);
}

#define print_process_info(stream, print_label, process_info) print_process_info_aux(__FILE__, __LINE__, stream, print_label, process_info)

static bool I_am_a_worker(const process_info_t *process_info) {
  assert(process_info != NULL);
  return process_info->my_rank != process_info->master_rank;
}

static bool I_am_the_master(const process_info_t *process_info) {
  assert(process_info != NULL);
  return process_info->my_rank == process_info->master_rank;
}

/********************** Worker's workflow *********************/
// this is a blocking call (will block until an order is received)
static order_t *wait_for_next_order(const process_info_t *process_info) {
  order_t *order;
  unsigned char *recv_buffer;
  const size_t recv_buffer_size = ORDER_WRAPPER_SIZE + g_order_payload_max_size;
  const char *label;
  void *payload;
  bool no_more_job, *ptr_no_more_job;
  size_t payload_size, *ptr_payload_size;
  size_t message_size, *ptr_message_size;
  
  assert(process_info != NULL);
  assert(I_am_a_worker(process_info) == true);
  assert(recv_buffer_size > 0);
  
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: I'm waiting for a new order\n", __FILE__, __LINE__, process_info->label);
  }
  
  recv_buffer = calloc(1, recv_buffer_size);
  assert(recv_buffer != NULL);
  
  MPI_Recv(recv_buffer, recv_buffer_size, MPI_BYTE, process_info->master_rank, ORDER_TAG, process_info->communicator, MPI_STATUS_IGNORE);
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: I've just received a new order\n", __FILE__, __LINE__, process_info->label);
  }
  
  label = (const char *) &recv_buffer[ORDER_OFFSET_LABEL];
  payload = &recv_buffer[ORDER_OFFSET_PAYLOAD];
  ptr_no_more_job  = (bool *) &recv_buffer[ORDER_OFFSET_NO_MORE_JOB];
  ptr_payload_size = (size_t *) &recv_buffer[ORDER_OFFSET_PAYLOAD_SIZE];
  ptr_message_size = (size_t *) &recv_buffer[ORDER_OFFSET_MESSAGE_SIZE];
  no_more_job  = *ptr_no_more_job;
  payload_size = *ptr_payload_size;
  message_size = *ptr_message_size;
  order = grasp_mpi_engine_new_order(label, payload_size, payload);
  set_order_references(order);
  *(order->ptr_no_more_job) = no_more_job;
  *(order->ptr_payload_size) = payload_size;
  *(order->ptr_message_size) = message_size;

  trackmem_free(recv_buffer);

  if (g_debug_level > 1) {
    print_order(stderr, "order[received from master]", order, process_info);
  }
  
  return order;
}

/* DEPRECATED (TO BE REMOVED) */
static int g_maximum_job_time = 0; // 30*60; /* Maximal time allowed for a task before interruption, in seconds (0 means no interruption) */
void grasp_mpi_engine_set_maximum_job_time(int maximum_job_time) {
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s(maximum_job_time = %d) called\n", __FILE__, __LINE__, __func__, maximum_job_time);
  }

  if (maximum_job_time < 0) {
    fprintf(stderr, "%s: unexpected value for the maximum job time (%d s). Should be positive or null. Will be set to 0.\n", 
	    g_appname, maximum_job_time);
    maximum_job_time = 0;
  }

  g_maximum_job_time = maximum_job_time;
}

/* this routine should always return a valid result (even empty) */
static result_t *execute_order(const process_info_t *process_info, const void *worker_info, const order_t *order) {
  result_t *result;

  assert(process_info != NULL);
  assert(order != NULL);
  assert(I_am_a_worker(process_info) == true);
  
  result = perform_task_callback(order, worker_info);

  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: I've finished my work on %s\n", __FILE__, __LINE__, process_info->label, order->label);
  }

  assert(result != NULL);
  return result;
}

static void send_result_to_master(const process_info_t *process_info, result_t *result) {
  int count;

  assert(process_info != NULL);
  assert(result != NULL);
  assert(I_am_a_worker(process_info) == true);
  
  if (g_debug_level > 0) {
    print_result(stderr, "result[I'm about to send]", result, process_info);
  }

  count = *(result->ptr_message_size);
  assert(count > 0);
  assert(result->message != NULL);
  assert(process_info->communicator == MPI_COMM_WORLD);

  MPI_Send(result->message, count, MPI_BYTE, process_info->master_rank, RESULT_IS_READY_TAG, process_info->communicator);

  if (g_debug_level > 0) {
    print_result(stderr, "result[just sent]", result, process_info);
  }
}

static void worker_main_loop(const process_info_t *process_info, const void *worker_info) {
  bool done = false;
  order_t *order = NULL;
  result_t *result = NULL;

  assert(process_info != NULL);
  assert(I_am_a_worker(process_info) == true);

  while (! done) {
    order = wait_for_next_order(process_info);
    done = *(order->ptr_no_more_job);
    if (done) {
      grasp_mpi_engine_delete_order(order);
      break;
    }
    
    result = execute_order(process_info, worker_info, order);
    send_result_to_master(process_info, result);
    grasp_mpi_engine_delete_result(result);
    grasp_mpi_engine_delete_order(order);
  }

  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: I'm stopping now\n", __FILE__, __LINE__, process_info->label);
  }
}

/********************** Master's workflow *********************/
static void send_order_to_worker(const process_info_t *process_info, const order_t *order, int worker_rank) {
  assert(process_info != NULL);
  assert(order != NULL);
  
  assert(I_am_the_master(process_info));
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: I'm sending an order to worker #%d\n", __FILE__, __LINE__, process_info->label, worker_rank);
  }
  assert(0 <= worker_rank && worker_rank < process_info->num_workers);
  if (g_debug_level > 0) {
    char label[255 + 1];
    sprintf(label, "order[sent to worker #%d]", worker_rank);
    print_order(stderr, label, order, process_info);
  }
  
  MPI_Send((void *) order->message, *(order->ptr_message_size), MPI_BYTE, worker_rank, ORDER_TAG, process_info->communicator);
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s: order has been sent to worker #%d\n", __FILE__, __LINE__, process_info->label, worker_rank);
  }
}

static void print_worker_status_aux(const char *file, int line, FILE *stream, const char *label, 
				    int num_workers, const int *worker_status, const process_info_t *process_info) {
  int worker_rank;
  
  assert(file != NULL);
  assert(stream != NULL);
  assert(num_workers > 0);
  assert(worker_status != NULL);
  assert(process_info != NULL);
  
  fprintf(stream, "%s:%d: %s: ", file, line, process_info->label);
  if (label == NULL) {
    label = "worker_status";
  }

  fprintf(stream, "%s (num_workers: %d): [", label, num_workers);
  for (worker_rank = 0 ; worker_rank < num_workers ; worker_rank++) {
    fprintf(stream, " #%d(%d) ", worker_rank, worker_status[worker_rank]);
  }
  fprintf(stream, "]\n");
}

#define print_worker_status(stream, label, num_workers, worker_status, process_info) \
  print_worker_status_aux(__FILE__, __LINE__, stream, label, num_workers, worker_status, process_info)

static int g_polling_time = 1; /* the master process will sleep during g_polling_time seconds before checking for available workers.
				* This was intended to avoid spinning (busy waiting) of the application. The default polling time
				* was initially one second and considered long enough for reducing considerably the system overhead without degrading
				* the performances of the application.
				*
				* Now, this feature is deprecated and disabled by default (set to 0).
				*/

/* DEPRECATED (TO BE REMOVED) */
void grasp_mpi_engine_set_polling_time(int polling_time) {
  if (g_debug_level > 0) {
    fprintf(stderr, "%s:%d: %s(polling_time = %d) called\n", __FILE__, __LINE__, __func__, polling_time);
  }

  if (polling_time < 0) {
    fprintf(stderr, "%s: unexpected value for the polling time (%d s). Should be positive or null. Will be set to 0.\n", 
	    g_appname, polling_time);
    polling_time = 0;
  }

  g_polling_time = polling_time;
}

static void print_requests(const process_info_t *process_info, const MPI_Request *requests) {
  int i;
  fprintf(stderr, "%s:%d: process_info->num_workers: %d\n", __FILE__, __LINE__, process_info->num_workers); 
  for (i = 0 ; i < process_info->num_workers ; i++) {
    int flag;
    MPI_Status status;
    MPI_Request_get_status(requests[i], &flag, &status);
    int source = status.MPI_SOURCE;
    int tag = status.MPI_TAG;
    int error = status.MPI_ERROR;
    fprintf(stderr, "%s:%d: requests_for_available_results[%d]: flag: %d source: %d tag: %d error: %d\n", __FILE__, __LINE__, i, flag, source, tag, error);
  } 
}

/* the master sends stop orders for asking workers to stop themselves */
static order_t *new_stop_order(int worker_rank) {
  order_t *order;
  char label[ORDER_LABEL_MAX_LEN + 1];

  sprintf(label, "stop order for worker #%d", worker_rank);
  order = grasp_mpi_engine_new_order(label, /* payload_size */ 0, /* payload */ NULL);
  assert(order != NULL);
  order->message[ORDER_OFFSET_NO_MORE_JOB] = true;
  
  set_order_references(order);

  return order;
}

static void send_next_orders(const process_info_t *process_info, const void *master_info, int to_worker) {
  const int num_workers = process_info->num_workers;
  
  while (g_no_more_order == false && g_available_workers > 0) {
    order_t *order = prepare_order_callback(to_worker, master_info, &g_no_more_order, &g_num_orders_sent);
    if (order != NULL) {
      send_order_to_worker(process_info, order, to_worker);
      g_available_workers--;
      grasp_mpi_engine_delete_order(order);
      to_worker++;
      to_worker %= num_workers;
    }
    else {
      /* empty orders are counted as performed orders */
      if (g_no_more_order == false) g_num_orders_performed++;
    }
  }
}

result_t *wait_for_next_result(const process_info_t *process_info, const void *master_info, int *from_worker) {
  result_t *result = grasp_mpi_engine_new_result(/* label */ NULL, g_result_payload_max_size, /* payload */ NULL);
  MPI_Status status;

  assert(process_info != NULL);
  assert(I_am_the_master(process_info));

  int count = *(result->ptr_message_size);
  MPI_Recv(result->message, count, MPI_BYTE, MPI_ANY_SOURCE, RESULT_IS_READY_TAG, process_info->communicator, &status);
  char label[255 + 1];
  int worker_rank = status.MPI_SOURCE;

  if (g_debug_level > 0) {
    sprintf(label, "result[from worker #%d]", worker_rank);
    print_result(stderr, label, result, process_info);
  }
    
  *from_worker = worker_rank;
  return result;
}

static void stop_workers(const process_info_t *process_info, const void *master_info) {
  int worker_rank;
  for (worker_rank = 0 ; worker_rank < process_info->num_workers ; worker_rank++) {
    order_t *order = new_stop_order(worker_rank);
    assert(order != NULL);
    send_order_to_worker(process_info, order, worker_rank);
    if (g_debug_level > 0) {
      fprintf(stderr, "%s:%d: %s: the worker #%d has been told to stop its work\n", __FILE__, __LINE__, process_info->label, worker_rank);
    }
    grasp_mpi_engine_delete_order(order);
  } /* for (worker_rank) */
}

static void master_main_loop(const process_info_t *process_info, const void *master_info) {
  int to_worker = 0;
  assert(process_info != NULL);
  assert(I_am_the_master(process_info));
  assert(prepare_order_callback != NULL);

  const int num_workers = process_info->num_workers;
  if (num_workers < 1) {
    fprintf(stderr, "%s:%d: no worker found. Please run the code with mpiexec -n <num_processes>, where <num_processes> == <num_workers> + 1\n",
	    __FILE__, __LINE__);
    exit (EXIT_FAILURE);
  }
  
  g_available_workers = num_workers;
  send_next_orders(process_info, master_info, to_worker);

  while (g_no_more_order == false || g_available_workers < num_workers) {
    int from_worker;
    
    // block until one worker sends a result; collects the result
    result_t *result = wait_for_next_result(process_info, master_info, &from_worker);
    g_available_workers++;
    g_num_orders_performed++;
    if (collect_result_callback != NULL) {
      collect_result_callback(result, master_info);
    }
    if (progress_info_callback != NULL) {
      progress_info_callback(g_num_orders_performed, result, master_info);
    }
    grasp_mpi_engine_delete_result(result);
    
    to_worker = from_worker;
    send_next_orders(process_info, master_info, to_worker);
  }
  
  stop_workers(process_info, master_info);
}

#endif /* #ifdef USE_MPI */

/* implementation of the interface */

void grasp_mpi_engine_set_debug_level(int debug_level) {
  g_debug_level = debug_level;
}

void grasp_mpi_engine_set_appname(const char *appname) {
  strncpy(g_appname, appname, sizeof(g_appname) - 1);
}

void *grasp_mpi_engine_get_order_payload(const order_t *order) {
  assert(order != NULL);
  return order->payload;
}

void *grasp_mpi_engine_get_result_payload(const result_t *result) {
  assert(result != NULL);
  return result->payload;
}

order_t *grasp_mpi_engine_dummy_order_callback(int worker_rank, const void *master_info, bool *no_more_order, int *num_orders) {
  order_t *order;
  char label[255 + 1];
  const void *payload = NULL;
  size_t payload_size = 0;
  const int NUM_ORDERS_TOTAL = 3; /* number of orders to issue */
  static int num_orders_sent = 0;
  
  if (num_orders_sent < NUM_ORDERS_TOTAL) {
    sprintf(label, "order for worker #%d", worker_rank);
    order = grasp_mpi_engine_new_order(label, payload_size, payload);
    assert(order != NULL);
    num_orders_sent++;
    *no_more_order = false;
    *num_orders = num_orders_sent;
    return order;
  }
  else {
    /* no more order will be issued */
    *no_more_order = true;
    *num_orders = num_orders_sent;
    return NULL;
  }
}

result_t *grasp_mpi_engine_dummy_task_callback(const order_t *order, const void *worker_info) {
  result_t *result;
  const void *payload = NULL;
  size_t payload_size = 0;
  char order_label[255 + 1];
  char result_label[255 + 1];
  
  assert(order != NULL);
  
  /* SIMULATE THE EXECUTION OF AN ORDER */
  sleep(5);
  
  grasp_mpi_engine_get_order_label(order, sizeof(order_label) - 1, order_label);
  snprintf(result_label, sizeof(result_label) - 1, "%s (completed)", order_label);
  result = grasp_mpi_engine_new_result(result_label, payload_size, payload);
  assert(result != NULL);
  return result;
}

void grasp_mpi_engine_set_order_callback(order_callback_t order_callback, int order_payload_max_size) {
  assert(order_payload_max_size >= 0);
  
  if (order_callback != NULL) {
    g_order_payload_max_size = order_payload_max_size;
    prepare_order_callback = order_callback;
  }
  else {
    g_order_payload_max_size = 0;
    prepare_order_callback = grasp_mpi_engine_dummy_order_callback;
  }
}

void grasp_mpi_engine_set_task_callback(task_callback_t task_callback, int result_payload_max_size) {
  assert(result_payload_max_size >= 0);
  
  if (task_callback != NULL) {
    g_result_payload_max_size = result_payload_max_size;
    perform_task_callback = task_callback;
    
  }
  else {
    g_result_payload_max_size = 0;
    perform_task_callback = grasp_mpi_engine_dummy_task_callback;
  }
}

void grasp_mpi_engine_set_collect_callback(collect_callback_t collect_callback) {
  collect_result_callback = collect_callback; /* will be ignored if set to NULL */
}

void grasp_mpi_engine_set_progress_info_callback(progress_info_callback_t progress_callback) {
  progress_info_callback = progress_callback;
  
}

#ifdef USE_MPI
bool grasp_mpi_engine_is_master(void) {
  process_info_t process_info;

  get_process_info(&process_info);
  return I_am_the_master(&process_info);
}

bool grasp_mpi_engine_is_worker(void) {
  process_info_t process_info;

  get_process_info(&process_info);
  return I_am_a_worker(&process_info);
}

int grasp_mpi_engine_main_loop(const void *master_info, const void *worker_info) {
  process_info_t process_info;
  
  get_process_info(&process_info);
  if (g_debug_level > 0) {
    char label[255 + 1];
    sprintf(label, "process_info[#%d]", process_info.my_rank);
    print_process_info(stderr, label, &process_info);
  }
  
  if (I_am_the_master(&process_info)) {
    master_main_loop(&process_info, master_info);
  }
  else {
    worker_main_loop(&process_info, worker_info);
  }
  
  return EXIT_SUCCESS;
}
#else /* #ifdef USE_MPI is false */
static void grasp_mpi_engine_print_error(void) {
  fprintf(stderr, "%s: error: not an MPI-enabled application (please use BUILD=mpi|mpi-debug|mpi-prod)\n", g_appname);
}

bool grasp_mpi_engine_is_master(void) {
  return false;
}

bool grasp_mpi_engine_is_worker(void) {
  return false;
}

int grasp_mpi_engine_main_loop(const void *master_info, const void *worker_info) {
  grasp_mpi_engine_print_error();

  return EXIT_FAILURE;
}

#endif /* #ifdef USE_MPI */
