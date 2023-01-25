#!/bin/bash
#
#SBATCH --job-name=exp1c_process_results
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_exp1c_process_results.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_exp1c_process_results.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch exp1c_process_results.sh <n_iter> <n_obs> <pos_mode=pos,wpos,npos> <indep_mode=indep,wdep,dep> <graph_known=0>

Rscript R/exp1c_process_results.R $1 $2 $3 $4 $5
