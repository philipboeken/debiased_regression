#!/bin/bash
#
#SBATCH --job-name=exp1b_simulate_sbatch
#SBATCH --open-mode=append
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_exp1b_simulate_sbatch.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_exp1b_simulate_sbatch.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80

# Run as: sbatch --array=1-<n_iter> exp1b_simulate_sbatch.sh <n_obs> <pos_mode=pos,wpos,npos> <indep_mode=indep,wdep,dep> <graph_known=0>

Rscript R/exp1b_simulate.R $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_MAX $1 $2 $3 $4
