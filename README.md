
Run example 1:
```
Rscript R/example1.R 400 7 TRUE 500
```

Run experiment 1 parallel:
```
bash run_experiment1.sh 50 1000
```

or on a single thread:
```
NITER = 50
NOBS = 1000
POSMODE = npos
INDEPMODE = indep
GRAPHKNOWN = 0
bash exp1a_find_valid_graphs.sh
bash exp1b_simulate_bash.sh $NITER $NOBS $POSMODE $INDEPMODE $GRAPHKNOWN
bash exp1c_process_results.sh $NITER $NOBS $POSMODE $INDEPMODE $GRAPHKNOWN
bash exp1d_plot_iterations_bash.sh $NITER $NOBS $POSMODE $INDEPMODE $GRAPHKNOWN
bash exp1e_compare_pos_indep.sh 1 1 $NOBS $GRAPHKNOWN
```

...and output results:
```
Rscript R/exp1f_tex.R
```

Run experiment 2:
```
Rscript R/exp2.R 500 1
```
