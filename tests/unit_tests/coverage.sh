#!/bin/sh

set -e

alr build --profiles=*=validation

alr exec -- gnatcov setup --prefix=gnatcov_rts

# Ensure gprbuild can find the gnatcov RTS project file and libraries
export GPR_PROJECT_PATH=$(realpath gnatcov_rts)/share/gpr
export LD_LIBRARY_PATH=$(realpath gnatcov_rts)/lib

alr exec -- gnatcov instrument \
    -P unit_tests.gpr \
    --level=stmt+mcdc \
    --dump-trigger=atexit \
    --projects libsap.gpr

alr build --profiles=*=validation -- \
    --src-subdirs=gnatcov-instr \
    --implicit-with=gnatcov_rts \
    -gnatec=instrument-spark.adc

bin/unit_tests

alr exec -- gnatcov coverage \
    -P unit_tests.gpr \
    --annotate=html+ \
    --output-dir gnatcov_out \
    --level=stmt+mcdc \
    --projects libsap.gpr \
    *.srctrace

alr exec -- gnatcov coverage \
    -P unit_tests.gpr \
    --annotate=xcov+ \
    --output-dir gnatcov_out \
    --level=stmt+mcdc \
    --projects libsap.gpr \
    *.srctrace
