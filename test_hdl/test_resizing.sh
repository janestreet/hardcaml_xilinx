#!/bin/bash
set -euo pipefail

jenga -pr .fpga-runtest

TEST_MODULE=test_ram_with_resizing
HERE="$(readlink -f "$(dirname "$0")")"
XSIM_PROJECT_DIR=$(mktemp -d "/tmp/xsim-XXXXX")

cd "$XSIM_PROJECT_DIR"
TCL_BATCH_FILE="cmd.tcl"

cp "$HERE"/resizing.v ./
cp "$HERE"/test_ram_with_resizing.v ./

FILES="\
  resizing.v \
  test_ram_with_resizing.v"

xvlog -relax -work xil_defaultlib -sv $FILES 2>&1 | tee compile.log

# The gui generates a glbl file, which i am not sure of its significance.
# I removed it and it seems to work. But if need it, remember to add a
# [-L xil_defaultlib.glbl] flag to xelab

xelab --relax --debug typical --mt auto \
  -L xil_defaultlib \
  -L unisims_ver \
  -L xpm \
  --snapshot $TEST_MODULE \
  "xil_defaultlib.$TEST_MODULE" \
  -log elaborate.log

function gui() {
  cat > $TCL_BATCH_FILE << EOF
add_wave *
run 10000 ns
EOF
  xsim $TEST_MODULE --gui \
    -tclbatch "$TCL_BATCH_FILE" \
    -log simulate.log
}

function terminal() {
  cat > $TCL_BATCH_FILE << EOF
run 200 ns
quit
EOF
  xsim $TEST_MODULE \
    -tclbatch "$TCL_BATCH_FILE" \
    -log simulate.log
}

gui

cat run-raw.log \
  | grep -v "Time:" \
  | grep -v "Info:" \
    > "$HERE"/run.log
