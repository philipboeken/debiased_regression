#!/bin/bash
#
# Run as: bash exp1d_plot_iterations_bash.sh <n_inter> <n_obs> <pos_mode> <indep_mode> <graph_known>

for IDX in $(seq 1 51)
do
	Rscript R/exp1d_plot_iterations.R $IDX $1 $2 $3 $4 $5
done
