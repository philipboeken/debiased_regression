
# Find, save and plot all valid DAGs:
Rscript plot_valid_graphs.R

# For graph_nr=17, plot results of a single iteration for all pos_mode and indep_mode values
sbatch --wrap "Rscript plot_pos_vs_indep.R 1 1 1000"

# At hactar, make for each graph 25 plots with n=1000:
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos dep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos dep

# Run experiments: n_iter=500, n_obs=1000, with positivity (P(S=1 | X, Z) > 0) and independence Y _||_S | X, Z:
JOBID1=$(sbatch --parsable --array=1-500 sbatch_run_all_iters.sh 1000 npos indep)
JOBID2=$(sbatch --parsable --array=1-500 sbatch_run_all_iters.sh 1000 pos indep)
JOBID3=$(sbatch --parsable --array=1-500 sbatch_run_all_iters.sh 1000 npos dep)
JOBID4=$(sbatch --parsable --array=1-500 sbatch_run_all_iters.sh 1000 pos dep)

# Combine MSE results for results with n=1000: -->
sbatch --dependency=afterok:$JOBID1 --wrap "Rscript format_mse_results.R 500 1000 npos indep"
sbatch --dependency=afterok:$JOBID2 --wrap "Rscript format_mse_results.R 500 1000 pos indep"
sbatch --dependency=afterok:$JOBID3 --wrap "Rscript format_mse_results.R 500 1000 npos dep"
sbatch --dependency=afterok:$JOBID4 --wrap "Rscript format_mse_results.R 500 1000 pos dep"