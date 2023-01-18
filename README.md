
Find, save and plot all valid DAGs:
```
Rscript plot_valid_graphs.R
```

For `graph_nr=17`, plot results of a single iteration for all `pos_mode` and `indep_mode` values
```
Rscript plot_pos_vs_indep.R 17 6 1000
```

Run experiments: `n_iter=500`, `n_obs=1000`, with positivity (P(S=1 | X, Z) > 0) and independence Y _||_S | X, Z:
```
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 npos dep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos dep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos indep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos wdep
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 wpos dep
```

Copy RData files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/mse_results/*.RData' ~/Documents/PhD/Missingness_regression/debiased_regression/output/mse_results
```

Copy txt files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/mse_results/*.txt' ~/Documents/PhD/Missingness_regression/debiased_regression/output/mse_results
```

Locally combine MSE results for results with n=2000:
```
Rscript format_mse_results.R 2000 pos indep
```

At hactar, make for each graph 25 plots with n=1000:
```
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 pos dep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 wpos dep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos indep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos wdep
sbatch sbatch_make_figures_per_graph.sh 25 1000 npos dep
```

Copy figures from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/figures/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/figures
```

Copy log and error files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/log/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/log
```