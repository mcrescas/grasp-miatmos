#ifndef PYTHON_GRASP_STATIC_H
#define	PYTHON_GRASP_STATIC_H

#ifdef	__cplusplus
extern "C" {
#endif

#include "../input/grasp_input.h"
#include "../settings/grasp_settings.h"

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef void (*input_driver_init_callback)(char*, size_t, size_t, char*);
void register_input_driver_init_callback(input_driver_init_callback callback);

typedef void (*input_driver_process_segment_callback)(char*, size_t, size_t, int, int, int);
void register_input_driver_process_segment_callback(input_driver_process_segment_callback callback);

typedef void (*input_driver_finalize_callback)(char*);
void register_input_driver_finalize_callback(input_driver_finalize_callback callback);

void initialize_py_interpreter();
void add_python_module_path(int index, char*path);

void python_callback_init(grasp_settings *settings, grasp_tile_description_t *input_information, char* py_module);

void python_callback_process_segment(char* py_module, grasp_segment_t* segment, int col, int row, int itime);

void python_callback_finalize(char* py_module);

PyObject* get_python_function(char* py_module, char* function);


#ifdef	__cplusplus
}
#endif

#endif	/* PYTHON_GRASP_STATIC_H */

