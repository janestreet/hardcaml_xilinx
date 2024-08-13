#!/bin/bash
set -euo pipefail

HERE="$(readlink -f "$(dirname "$0")")"
XSIM_PROJECT_DIR=$(mktemp -d "/tmp/xsim-XXXXX")

cd "$XSIM_PROJECT_DIR"
TCL_BATCH_FILE="cmd.tcl"

cp "$HERE"/xram.v ./
cp "$HERE"/test_xram.sv ./

FILES="\
  xram.v \
  test_xram.sv"

xvlog -relax -work xil_defaultlib -sv $FILES 2>&1 | tee compile.log

# The gui generates a glbl file, which i am not sure of its significance.
# I removed it and it seems to work. But if need it, remember to add a
# [-L xil_defaultlib.glbl] flag to xelab

xelab --relax --debug typical --mt auto \
  -L xil_defaultlib \
  -L unisims_ver \
  -L xpm \
  --snapshot test_xram_collision \
  xil_defaultlib.test_xram_collision \
  -log elaborate.log

function gui() {
  cat > $TCL_BATCH_FILE << EOF
add_wave *
run 200 ns
EOF
  xsim test_xram_collision --gui \
    -tclbatch "$TCL_BATCH_FILE" \
    -log simulate.log
}

function terminal() {
  cat > $TCL_BATCH_FILE << EOF
run 200 ns
quit
EOF
  xsim test_xram_collision \
    -tclbatch "$TCL_BATCH_FILE" \
    -log simulate.log
}

gui

cat run-raw.log \
  | grep -v "Time:" \
  | grep -v "Info:" \
    > "$HERE"/run.log
