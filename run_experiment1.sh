#!/bin/bash
#
# Run as: bash run_experiment1.sh 50 1000

NITER=${1:-$NITER}
NOBS=${2:-$NOBS}

# 1a: Find, save and plot all valid DAGs:
JOBID0=$(sbatch --parsable exp1a_find_valid_graphs.sh)

# 1b: Run experiments: n_iter=$NITER, n_obs=$NOBS
JOBID1=$(sbatch --parsable --dependency=afterok:$JOBID0 --array=1-$NITER exp1b_simulate_sbatch.sh $NOBS npos indep)

# 1c: Combine MSE results for results with n=$NOBS: -->
sbatch --dependency=afterok:$JOBID0,afterany:$JOBID1 exp1c_process_results.sh $NITER $NOBS npos indep
