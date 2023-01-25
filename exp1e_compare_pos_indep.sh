#!/bin/bash
#
#SBATCH --job-name=exp1e_compare_pos_indep
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_exp1e_compare_pos_indep.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_exp1e_compare_pos_indep.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch exp1e_compare_pos_indep.sh <graph_nr> <iter> <n_obs> <graph_known>

Rscript R/exp1e_compare_pos_indep.R $1 $2 $3 $4
