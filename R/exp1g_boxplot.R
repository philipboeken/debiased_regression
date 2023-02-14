rows <- c("yhat_naive", "yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped", "yhat_dr_true_clipped", "yhat_dr_est_clipped")
columns <- c("yhat_true", "y", "yhat_imputed", "y_weighted_true", "y_weighted_est")
labels <- c("Naive", "RR", "IW-t", "IW-e", "DR-t", "DR-e")

load("~/Documents/PhD/debiased_regression/output/tables/exp1/results_data_transformed_2_1000_npos_indep_FALSE.RData")
for(i in 1:length(transformed_results)) {
  cat(names(transformed_results)[i], ":\n")
  results <- transformed_results[[i]]
  boxplot(results[rows, "y"], names = labels, log="y")
  cat("p-value that MSE naive > MSE repeated:",
      t.test(results[["yhat_naive", "y"]], results[["yhat_repeated", "y"]], alternative="greater")$p.value,
      "\n")
  cat("p-value that MSE iw > MSE repeated:",
      t.test(results[["yhat_iw_true", "y"]], results[["yhat_repeated", "y"]], alternative="greater")$p.value,
      "\n")
  cat("\n")
}

# library(vioplot)
# vioplot(results[["yhat_naive", "y"]], results[["yhat_repeated", "y"]], results[["yhat_iw_true_clipped", "y"]])
