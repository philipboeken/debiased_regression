#!/bin/bash
#
#SBATCH --job-name=example1
#SBATCH --open-mode=append
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_example1.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_example1.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch run_example1.sh <n=400> <seed=1> <save_figs=1>

Rscript R/example1.R $1 $2 $3
