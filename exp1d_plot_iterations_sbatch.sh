#!/bin/bash
#
#SBATCH --job-name=exp1d_plot_iterations_sbatch
#SBATCH --open-mode=append
#SBATCH --output=/home/<NAME>/debiased_regression/output/log/log_exp1d_plot_iterations_sbatch.stdout
#SBATCH --error=/home/<NAME>/debiased_regression/output/log/error_exp1d_plot_iterations_sbatch.stderr
#SBATCH --workdir=/home/<NAME>/debiased_regression
#
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80
#
#SBATCH --array=1-20

# Run as: sbatch exp1d_plot_iterations_sbatch.sh <n_inter> <n_obs> <pos_mode> <indep_mode> <graph_known>

Rscript R/exp1d_plot_iterations.R $SLURM_ARRAY_TASK_ID $1 $2 $3 $4 $5
