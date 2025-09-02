# Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
# Licensed under the GRASP Open Source License V1.0 (see LICENSE file)

add_library(output_segment_function_csv
	grasp_output_segment_csv.c
)

find_package(ZLIB)
if (ZLIB_FOUND)
     add_definitions(-DZLIB_FOUND)
     include_directories(${ZLIB_INCLUDE_DIRS})
     target_link_libraries(output_segment_function_csv grasp_settings grasp_output ${ZLIB_LIBRARIES})
else()
    target_link_libraries(output_segment_function_csv grasp_settings grasp_output)
endif()

