#!/bin/bash
#
#SBATCH --job-name=exp2
#SBATCH --open-mode=append
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_exp2.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_exp2.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch run_experiment2.sh <m=100> <seed=1>

Rscript R/exp2.R $1 $2