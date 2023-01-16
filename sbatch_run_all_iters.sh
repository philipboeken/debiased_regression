#!/bin/bash
#
#SBATCH --job-name=sbatch_run_all_iters
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_mse.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_mse.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80
#
#SBATCH --array=1-500

# Run as: sbatch sbatch_run_all_iters.sh <n_obs>

Rscript run_iter.R $SLURM_ARRAY_TASK_ID $1
