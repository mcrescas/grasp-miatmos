#!/bin/bash
set -ex

GRASP_BINARY="$1"

# Set up temporary directory for output
OUTPUT_DIR=$(mktemp -d)
trap 'rm -rf "${OUTPUT_DIR}"' EXIT

# Prepare paths for output and reference output files^
BASE_PATH="$(dirname $BASH_SOURCE)"
OUTPUT_FILE="${OUTPUT_DIR}/output.dat"
REFERENCE_FILE="${BASE_PATH}/segmenter/expected_output.dat"

# Run the input driver through GRASP
${GRASP_BINARY} "${BASE_PATH}/segmenter/settings_segmenter.yml" \
                "input.sdata.dump=${OUTPUT_FILE}" \
                "input.driver_settings.segmenter.used_files_out_name=${OUTPUT_DIR}/used_files.txt"

# Compare the dumped SDATA file with the reference SDATA file
diff "${OUTPUT_FILE}" "${REFERENCE_FILE}"
