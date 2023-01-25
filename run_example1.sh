#!/bin/bash
#
# Run as: sbatch run_example1.sh <n=400> <seed=1> <save_figs=1>

Rscript R/example1.R $1 $2 $3
