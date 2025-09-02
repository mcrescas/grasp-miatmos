set(GIT_RELEASE_TAG_GLOB_PATTERN "v*")

function(sanitize_debian_package_version_string CLEAN_VERSION_STRING_VARNAME VERSION_STRING)
    string(REGEX REPLACE "^v" "" CLEAN_VERSION_STRING ${VERSION_STRING})
    set(${CLEAN_VERSION_STRING_VARNAME} "${CLEAN_VERSION_STRING}" PARENT_SCOPE)
endfunction()

function(get_git_release_tag GIT_RELEASE_TAG_VARNAME)
    execute_process(
        COMMAND git describe --tags --match ${GIT_RELEASE_TAG_GLOB_PATTERN} --dirty --exact-match
        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
        # hide expected "fatal: no tag exactly matches '[SHA1]'
        ERROR_QUIET
    )
    if(GIT_DESCRIBE_OUTPUT)
        set(${GIT_RELEASE_TAG_VARNAME} "${GIT_DESCRIBE_OUTPUT}" PARENT_SCOPE)
    else()
        unset(${GIT_RELEASE_TAG_VARNAME} PARENT_SCOPE)
    endif()
endfunction()

function(get_git_version GIT_VERSION_VARNAME)
    execute_process(
        COMMAND git describe --tags --match ${GIT_RELEASE_TAG_GLOB_PATTERN} --dirty
        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(GIT_DESCRIBE_OUTPUT)
        set(${GIT_VERSION_VARNAME} "${GIT_DESCRIBE_OUTPUT}" PARENT_SCOPE)
    else()
        unset(${GIT_VERSION_VARNAME} PARENT_SCOPE)
    endif()
endfunction()

function(get_git_branch_name GIT_BRANCH_VARNAME)
    execute_process(
        COMMAND git rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE GIT_BRANCH
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(GIT_BRANCH)
        set(${GIT_BRANCH_VARNAME} "${GIT_BRANCH}" PARENT_SCOPE)
    else()
        unset(${GIT_BRANCH_VARNAME} PARENT_SCOPE)
    endif()
endfunction()

# name inspired by gitlab-ci's $CI_COMMIT_REF_SLUG
# https://docs.gitlab.com/ee/ci/environments.html#example-configuration
function(get_git_branch_slug GIT_BRANCH_SLUG_VARNAME)
    get_git_branch_name(GIT_BRANCH_NAME)
    if(GIT_BRANCH_NAME)
        string(REGEX REPLACE "[^A-Za-z0-9]+" "-" GIT_BRANCH_SLUG ${GIT_BRANCH_NAME})
        set(${GIT_BRANCH_SLUG_VARNAME} ${GIT_BRANCH_SLUG} PARENT_SCOPE)
    else()
        unset(${GIT_BRANCH_SLUG_VARNAME})
    endif()
endfunction()

function(get_debian_package_version PACKAGE_VERSION_VARNAME)
    get_git_release_tag(GIT_RELEASE_TAG)
    if(GIT_RELEASE_TAG)
        set(PACKAGE_VERSION "${GIT_RELEASE_TAG}")
    else()
        get_git_version(GIT_VERSION_STRING)
        get_git_branch_slug(GIT_BRANCH_SLUG)
        if(NOT GIT_VERSION_STRING)
            set(PACKAGE_VERSION "unknown")
        elseif(${GIT_BRANCH_SLUG} STREQUAL "master")
            set(PACKAGE_VERSION "${GIT_VERSION_STRING}")
        else()
            set(PACKAGE_VERSION "${GIT_VERSION_STRING}+${GIT_BRANCH_SLUG}")
        endif()
    endif()
    sanitize_debian_package_version_string(PACKAGE_VERSION_CLEAN ${PACKAGE_VERSION})
    set(${PACKAGE_VERSION_VARNAME} "${PACKAGE_VERSION_CLEAN}" PARENT_SCOPE)
endfunction()

# base kernels rarely change.
# use version string based on last commit changing kernels folder (instead of repo-wide git describe)
# to avoid duplicate grasp-kernels-base packages in our apt archive (approx 150MiB/package).
# potential alternative approach in future: move kernels to a git submodule
function(get_base_kernels_debian_package_version PACKAGE_VERSION_VARNAME)
    execute_process(
        COMMAND git rev-parse --show-toplevel
        OUTPUT_VARIABLE GIT_WORKTREE_ROOT_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
        COMMAND git log --max-count=1 --format=%as-%H -- ./src/retrieval/internal_files/
        WORKING_DIRECTORY "${GIT_WORKTREE_ROOT_PATH}"
        OUTPUT_VARIABLE GIT_LOG_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(GIT_LOG_OUTPUT)
        set(${PACKAGE_VERSION_VARNAME} "${GIT_LOG_OUTPUT}" PARENT_SCOPE)
    else()
        unset(${PACKAGE_VERSION_VARNAME} PARENT_SCOPE)
    endif()
endfunction()
