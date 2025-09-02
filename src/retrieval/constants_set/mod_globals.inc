/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */


/* File Path Length */

#define _GBL_FILE_PATH_LEN 511

/* g_error_aux defined in mod_stop_report.f90 */

#define G_ERROR(msg) call g_error_aux(msg, __FILE__, __LINE__); return
