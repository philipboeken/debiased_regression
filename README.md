
Run example 1:
```
# Find, save and plot all valid DAGs:
bash run_example1.sh <n=400> <seed=1> <save_figs=1>
```

Run experiment 1 parallel:
```
bash run_experiment1.sh <n_iter=500> <n_obs=1000>
```

or locally (only for small n_iter, otherwise it will take forever):
```
NITER = 10
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

Run experiment 2:
```
bash run_experiment2.sh <m=1000> <seed=1>
```
