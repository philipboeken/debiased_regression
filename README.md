
Find, save and plot all valid DAGs:
```
Rscript plot_valid_graphs.R
```

Run experiments: n_iter=500, n_obs=1000:
```
sbatch sbatch_run_all_iters 1000
```

Copy RData files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/mse_results/*.RData' ~/Documents/PhD/Missingness_regression/debiased_regression/output/mse_results
```

Locally combine MSE results:
```
Rscript format_mse_results.R
```

At hactar, make for each graph 20 plots with n=1000:
```
sbatch sbatch_make_figures_per_graph 20 1000
```

Copy pdf files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/figures/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/figures
```

Copy log and error files from hactar to local:
```
scp 'pboeken@hactar.science.uva.nl:/home/pboeken/debiased_regression/output/log/*' ~/Documents/PhD/Missingness_regression/debiased_regression/output/log
```