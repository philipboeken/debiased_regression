#!/bin/bash
#
#SBATCH --job-name=exp1d_plot_iterations_sbatch
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_figures_graph_%a.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_figures_graph_%a.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80
#
#SBATCH --array=1-51

# Run as: sbatch exp1d_plot_iterations_sbatch.sh <n_inter> <n_obs> <pos_mode> <indep_mode>

Rscript R/exp1d_plot_iterations.R $SLURM_ARRAY_TASK_ID $1 $2 $3 $4
