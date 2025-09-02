cmake_minimum_required(VERSION 3.10)

project(grasp-examples LANGUAGES NONE)

set(CMAKE_MODULE_PATH "../cmake/")

set(RESOURCE_PREFIX share/grasp/examples)
install(FILES README.md DESTINATION ${RESOURCE_PREFIX})
install(DIRECTORY aod lidar_and_sunphotometer polder sunphotometer
    DESTINATION ${RESOURCE_PREFIX})

# variable name matching one defined in root CMakeLists.txt
option(APPEND_CONSTANTS_SET_TO_TARGETS "Append -[CONSTANTS_SET] to name of required grasp package" OFF)
if(APPEND_CONSTANTS_SET_TO_TARGETS)
    set(INSTALL_TARGET_SUFFIX "-generic")
else()
    set(INSTALL_TARGET_SUFFIX "")
endif()

set(CPACK_GENERATOR "DEB")
include(GitUtils)
get_debian_package_version(CPACK_DEBIAN_PACKAGE_VERSION)
set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "all")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "GRASP SAS <office@grasp-sas.com>")
set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://code.grasp-open.com/grasp-internal/grasp/tree/master/examples")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Examples for GRASP applications.")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "grasp${INSTALL_TARGET_SUFFIX} (${CPACK_DEBIAN_PACKAGE_VERSION}+generic)")
set(CPACK_DEBIAN_FILE_NAME "DEB-DEFAULT")
include(CPack)
