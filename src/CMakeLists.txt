# Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
# Licensed under the GRASP Open Source License V1.0 (see LICENSE file)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(retrieval)
add_subdirectory(controller)
add_subdirectory(input)
add_subdirectory(output)
if( EXISTS ${CMAKE_SOURCE_DIR}/src/python/python_grasp.c ) 
    add_subdirectory(python)

    execute_process(WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/src/python"
            COMMAND /usr/bin/env git rev-parse --short HEAD
            RESULT_VARIABLE result
            OUTPUT_VARIABLE commit_ref)
    if(NOT ${result} EQUAL 0)
            set_property(GLOBAL PROPERTY GRASP_PYTHON "[undefined]")
    else()
            string(STRIP ${commit_ref} commit_ref)
            set_property(GLOBAL PROPERTY GRASP_PYTHON ${commit_ref})
    endif()
else()
    set_property(GLOBAL PROPERTY GRASP_PYTHON "no")
endif()
add_subdirectory(global)
add_subdirectory(settings)
add_subdirectory(test)
