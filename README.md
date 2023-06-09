# Correcting for Selection Bias and Missing Response in Regression using Privileged Information

Source code for the UAI 2023 paper 

P. Boeken, N. de Kroon, M. de Jong, J.M. Mooij, O. Zoeter, Correcting for Selection Bias and Missing Response in Regression using Privileged Information, *The 39th Conference on Uncertainty in Artificial Intelligence*, 2023

## Running the experiments:

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
```

...and output results:
```
Rscript R/exp1d_tex.R
```

Run experiment 2:
```
Rscript R/exp2.R 500 1
```
