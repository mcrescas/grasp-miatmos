/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   grasp_input_driver_sdata_error_codes.h
 * Author: david
 *
 * Created on July 3, 2018, 8:59 AM
 */

#ifndef GRASP_INPUT_DRIVER_SDATA_ERROR_CODES_H
#define GRASP_INPUT_DRIVER_SDATA_ERROR_CODES_H

// Ok status is 0

// This is not an error it self, it is an status. 
// but if the EOF arrives before it is expected, then
// It'll be an error
#define GRASP_SDATA_EOF 1

// This is the list of errors that can be returned
#define GRASP_ERROR_SDATA_DRIVER_ONLY_ONE_FILE_ALLOWED -1
#define GRASP_ERROR_SDATA_DRIVER_FILE_CANNOT_OPEN -2
#define GRASP_ERROR_SDATA_READ_UNEXPECTED_END -3
#define GRASP_ERROR_SDATA_MALFORMED -4
#define GRASP_ERROR_SDATA_MALFORMED_TIMESTAMP -5
#define GRASP_ERROR_SDATA_MALFORMED_LINE -6
#define GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS -7
#define GRASP_ERROR_SDATA_READING_PROBLEM -8
#define GRASP_ERROR_SDATA_CLOSING_PROBLEM -9
#define GRASP_ERROR_SDATA_OPEN_FAILED -10
#define GRASP_ERROR_SDATA_MISSING_VERSION -11
#define GRASP_ERROR_SDATA_VERSION_PATTERN_BADFORMED -12
#define GRASP_ERROR_SDATA_VERSION_UNSUPPORTED -13
#define GRASP_ERROR_SDATA_NOT_ENOUGH_MEM_DECLARED -14
#define GRASP_ERROR_SDATA_DUMP_ERROR -15
#define GRASP_ERROR_SDATA_SEGMENT_SIZE -16
#define GRASP_ERROR_SDATA_INVALID_VALUE -17




#endif /* GRASP_INPUT_DRIVER_SDATA_ERROR_CODES_H */

