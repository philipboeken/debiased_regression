#!/bin/bash
#
# Run as: bash run_experiment1.sh <n_iter=500> <n_obs=1000>

NITER=${1:-$NITER}
NOBS=${2:-$NOBS}

# 1a: Find, save and plot all valid DAGs:
JOBID0=$(sbatch --parsable exp1a_find_valid_graphs.sh)

# 1b: Run experiments: n_iter=$NITER, n_obs=$NOBS, with positivity (P(S=1 | X, Z) > 0) and independence Y _||_S | X, Z:
JOBID1=$(sbatch --parsable --dependency=afterok:$JOBID0 --array=1-$NITER exp1b_simulate_sbatch.sh $NOBS npos indep)
# JOBID2=$(sbatch --parsable --dependency=afterok:$JOBID0 --array=1-$NITER exp1b_simulate_sbatch.sh $NOBS pos indep)
# JOBID3=$(sbatch --parsable --dependency=afterok:$JOBID0 --array=1-$NITER exp1b_simulate_sbatch.sh $NOBS npos dep)
# JOBID4=$(sbatch --parsable --dependency=afterok:$JOBID0 --array=1-$NITER exp1b_simulate_sbatch.sh $NOBS pos dep)

# 1c: Combine MSE results for results with n=$NOBS: -->
sbatch --dependency=afterok:$JOBID0,afterany:$JOBID1 exp1c_process_results.sh $NITER $NOBS npos indep
# sbatch --dependency=afterok:$JOBID0,afterany:$JOBID2 exp1c_process_results.sh $NITER $NOBS pos indep
# sbatch --dependency=afterok:$JOBID0,afterany:$JOBID3 exp1c_process_results.sh $NITER $NOBS npos dep
# sbatch --dependency=afterok:$JOBID0,afterany:$JOBID4 exp1c_process_results.sh $NITER $NOBS pos dep
# sbatch --dependency=afterok:$JOBID0 exp1c_process_results.sh $NITER $NOBS npos indep
# sbatch --dependency=afterok:$JOBID0 exp1c_process_results.sh $NITER $NOBS pos indep
# sbatch --dependency=afterok:$JOBID0 exp1c_process_results.sh $NITER $NOBS npos dep
# sbatch --dependency=afterok:$JOBID0 exp1c_process_results.sh $NITER $NOBS pos dep

# 1d: Make for each graph 25 plots with n=$NOBS:
# sbatch --dependency=afterok:$JOBID0 exp1d_plot_iterations_sbatch.sh 5 $NOBS pos indep
sbatch --dependency=afterok:$JOBID0 exp1d_plot_iterations_sbatch.sh 5 $NOBS npos indep
# sbatch --dependency=afterok:$JOBID0 exp1d_plot_iterations_sbatch.sh 5 $NOBS pos dep
# sbatch --dependency=afterok:$JOBID0 exp1d_plot_iterations_sbatch.sh 5 $NOBS npos dep

# For graph_nr=1, plot results of a single iteration for all pos_mode and indep_mode values
# sbatch --dependency=afterok:$JOBID0 exp1e_compare_pos_indep.sh 3 1 $NOBS
# sbatch --dependency=afterok:$JOBID0 exp1e_compare_pos_indep.sh 19 1 $NOBS
# sbatch --dependency=afterok:$JOBID0 exp1e_compare_pos_indep.sh 41 1 $NOBS