#!/bin/bash
#
# Run as: bash exp1b_simulate_bash.sh <n_iter> <n_obs> <pos_mode=pos,wpos,npos> <indep_mode=indep,wdep,dep> <graph_known=0>

for IDX in $(seq 1 $1)
do
	Rscript R/exp1b_simulate.R $IDX $1 $2 $3 $4 $5
done
