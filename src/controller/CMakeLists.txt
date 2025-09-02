# Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
# Licensed under the GRASP Open Source License V1.0 (see LICENSE file)

include_directories(${CMAKE_CURRENT_BINARY_DIR} ${INPUT_BIN} ${INPUT_SRC}
	${OUTPUT_SRC} ${OUTPUT_BIN})

add_definitions(-DSPHEROID -DOSH -DGARRLIC)

if(${USE_MPI})
	find_package(MPI REQUIRED)
	add_definitions(-DUSE_MPI)
        include_directories(SYSTEM ${MPI_INCLUDE_PATH})
endif()

if(${DEBUG_MPI})
        add_definitions(-DDEBUG_MPI)
endif()

list(APPEND GRASP_CONTROLLER_SOURCES
        grasp_mpi_engine.c grasp_mpi_engine.h
        grasp_controller.c
        grasp_controller_iteration_callback.c)
list(APPEND GRASP_CONTROLLER_LINK_LIBRARIES
        grasp_retrieval_lib
	grasp_settings
	grasp_input
	grasp_global
	grasp_output
	grasputils)
if (${USE_PYTHON})
	find_package(Python 3 COMPONENTS Development Interpreter REQUIRED)
	include_directories(${Python_INCLUDE_DIRS})
	list(APPEND GRASP_CONTROLLER_SOURCES python_grasp_static.c)
endif()
add_library(grasp_controller ${GRASP_CONTROLLER_SOURCES})
target_link_libraries(grasp_controller ${GRASP_CONTROLLER_LINK_LIBRARIES})

if(${USE_MPI})
        target_link_libraries(grasp_controller ${MPI_C_LIBRARIES})
endif()

# CPack ignores the RENAME parameter in install()
set(GRASP_EXECUTABLE_NAME "grasp_app${INSTALL_TARGET_SUFFIX}")
add_executable("${GRASP_EXECUTABLE_NAME}" grasp_app.c grasp_main.c)
target_link_libraries("${GRASP_EXECUTABLE_NAME}" grasp_controller)
set_property(TARGET "${GRASP_EXECUTABLE_NAME}" PROPERTY INSTALL_RPATH_USE_LINK_PATH TRUE)
install(TARGETS "${GRASP_EXECUTABLE_NAME}"
	DESTINATION bin)

set(GRASP_WRAPPER_PATH "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/grasp${INSTALL_TARGET_SUFFIX}")
configure_file(grasp.in "${GRASP_WRAPPER_PATH}" @ONLY)
install(PROGRAMS "${GRASP_WRAPPER_PATH}"
  DESTINATION bin)

# API for Python wrapper
if (${USE_PYTHON})
        set(CMAKE_MACOSX_RPATH TRUE)
        set(LIBPYTHON_GRASP_NAME "python_grasp${INSTALL_TARGET_SUFFIX}")
        add_library("${LIBPYTHON_GRASP_NAME}" SHARED
            python_grasp.c
            python_grasp_static.c
            grasp_main.c
	    ../retrieval/forward_model/external_interface.c            
        )
        target_link_libraries("${LIBPYTHON_GRASP_NAME}"
            grasp_controller
            grasp_retrieval_lib
            grasp_settings
            grasp_input
            grasp_global
            grasp_output
            grasputils
            "${Python_LIBRARIES}"
        )
        install(TARGETS "${LIBPYTHON_GRASP_NAME}" DESTINATION lib)
endif()
