#!/bin/bash
set -ex

GRASP_BINARY="$1"

# Set up temporary directory for output
OUTPUT_DIR=$(mktemp -d)
trap 'rm -rf "${OUTPUT_DIR}"' EXIT

# Prepare paths for output and reference output files^
BASE_PATH="$(dirname $BASH_SOURCE)"
OUTPUT_FILE="${OUTPUT_DIR}/output.h5"
REFERENCE_FILE="${BASE_PATH}/hdf5/expected_output.h5"

# Run the input driver through GRASP
${GRASP_BINARY} "${BASE_PATH}/sdata/settings_sdata.yml" \
                "output.tile.function=hdf5" \
                "output.tile.stream=${OUTPUT_FILE}"

# Compare the dumped SDATA file with the reference SDATA file
h5diff "${OUTPUT_FILE}" "${REFERENCE_FILE}"
