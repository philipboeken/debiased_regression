#!/bin/bash
#
#SBATCH --job-name=exp1a_find_valid_graphs
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch exp1a_find_valid_graphs.sh

Rscript R/exp1a_find_valid_graphs.R
