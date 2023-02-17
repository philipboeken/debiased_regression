#!/bin/bash
#
#SBATCH --job-name=exp1a_find_valid_graphs
#SBATCH --open-mode=append
#SBATCH --output=/home/<NAME>/debiased_regression/output/log/log_exp1a_find_valid_graphs.stdout
#SBATCH --error=/home/<NAME>/debiased_regression/output/log/error_exp1a_find_valid_graphs.stderr
#SBATCH --workdir=/home/<NAME>/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch exp1a_find_valid_graphs.sh

Rscript R/exp1a_find_valid_graphs.R
