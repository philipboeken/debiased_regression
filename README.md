
Find all valid DAGs: (run locally! doesn't work on hactar)
```
# Find, save and plot all valid DAGs:
Rscript plot_valid_graphs.R
```

Create figures on hactar:
```
# For graph_nr=17, plot results of a single iteration for all pos_mode and indep_mode values
sbatch --wrap "Rscript plot_pos_vs_indep.R 17 6 1000"

# At hactar, make for each graph 25 plots with n=1000:
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos dep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos dep

sbatch sbatch_make_figures_per_graph.sh 25 1000 npos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos dep
```


Run mse experiments on hactar:
```
# Run experiments: n_iter=500, n_obs=1000, with positivity (P(S=1 | X, Z) > 0) and independence Y _||_S | X, Z:
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos dep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos dep

sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos dep
```

```
# Locally combine MSE results for results with n=1000: -->
Rscript format_mse_results.R 1000 npos indep
Rscript format_mse_results.R 1000 pos indep
Rscript format_mse_results.R 1000 npos dep
Rscript format_mse_results.R 1000 pos dep

Rscript format_mse_results.R 1000 npos wdep
Rscript format_mse_results.R 1000 pos wdep
Rscript format_mse_results.R 1000 wpos wdep
Rscript format_mse_results.R 1000 wpos indep
Rscript format_mse_results.R 1000 wpos dep
```

Copying files from hactar to local:
```
# Copy RData files from hactar to local:
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/mse_results/*.RData' ~/Documents/PhD/debiased_regression/output/mse_results

# Copy txt files from hactar to local:
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/mse_results/*.txt' ~/Documents/PhD/debiased_regression/output/mse_results

# Copy figures from hactar to local:
scp -rp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/figures' ~/Documents/PhD/debiased_regression/output

# Copy log and error files from hactar to local:
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/log/*' ~/Documents/PhD/debiased_regression/output/log
```