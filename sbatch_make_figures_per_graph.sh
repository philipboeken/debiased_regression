#!/bin/bash
#
#SBATCH --job-name=sbatch_make_figures_per_graph
#SBATCH --output=/home/pboeken/debiased_regression/output/log/log_figures_graph_%a.stdout
#SBATCH --error=/home/pboeken/debiased_regression/output/log/error_figures_graph_%a.stderr
#SBATCH --workdir=/home/pboeken/debiased_regression
#
#SBATCH --time=10000:00
#SBATCH --mem-per-cpu=1000
#
#SBATCH --mail-type=END,FAIL,REQUEUE,TIME_LIMIT_80
#
#SBATCH --array=1-27

# Run as: sbatch sbatch_make_figures_per_graph.sh <n_inter> <n_obs>

Rscript make_figures_per_graph.R $SLURM_ARRAY_TASK_ID $1 $2
