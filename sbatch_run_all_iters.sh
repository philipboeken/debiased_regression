#!/bin/bash
#
#SBATCH --job-name=sbatch_run_all_iters
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_mse_iter_%a.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_mse_iter_%a.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80
#

# Run as: sbatch --array=1-<n_iter> sbatch_run_all_iters.sh <n_obs> <pos_mode=pos,wpos,npos> <indep_mode=indep,wdep,dep>

Rscript run_iter.R $SLURM_ARRAY_TASK_ID $1 $2 $3
