
Find, save and plot all valid DAGs:
```
Rscript plot_valid_graphs.R
```

For a particular `graph_nr`, plot results of a single iteration for all `pos_mode` and `indep_mode` values
```
Rscript plot_pos_vs_indep.R <graph_nr> <iter> <n_obs>
```

Run experiments: `n_iter=500`, `n_obs=1000`, with positivity (P(S=1 | X, Z) > 0) and independence Y _||_S | X, Z:
```
sbatch --array=1-500 sbatch_run_all_iters.sh 1000 pos indep
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

At hactar, make for each graph 20 plots with n=1000:
```
sbatch sbatch_make_figures_per_graph.sh 20 1000 pos indep
```

Copy pdf files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/figures/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/figures
```

Copy log and error files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/log/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/log
```