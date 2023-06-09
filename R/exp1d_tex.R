source("R/utils.R")

load("output/tables/exp1/results_data_transformed_50_1000_npos_indep_FALSE_gam.RData")
# load("output/tables/exp1/results_data_transformed_50_1000_npos_indep_FALSE_rpart.RData")
all_results <- transformed_results[[1]]
graph_range <- names(transformed_results)[1]
cat(graph_range, ":\n")
output_table(all_results)
cat("\n")

qqnorm(log(all_results[["yhat_iw_true", "y"]]))
qqnorm(log(all_results[["yhat_repeated", "y"]]))
qqnorm(log(all_results[["yhat_naive", "y"]]))
cat(
  "p-value for H1 MSE RR < MSE naive:",
  wilcox.test(all_results[["yhat_repeated", "y"]],
    all_results[["yhat_naive", "y"]],
    alternative = "less"
  )$p.value,
  "\n"
)

cat(
  "p-value for H1 that MSE naive < MSE IW-e:",
  wilcox.test(all_results[["yhat_naive", "y"]],
    all_results[["yhat_iw_est_clipped", "y"]],
    alternative = "less"
  )$p.value,
  "\n"
)
cat("\n")
cat(
  "p-value for H1 that MSE IW-e < MSE IW-t:",
  wilcox.test(all_results[["yhat_iw_est_clipped", "y"]],
    all_results[["yhat_iw_true_clipped", "y"]],
    alternative = "less"
  )$p.value,
  "\n"
)
cat("\n")

cat(
  "p-value for H1 that MSE naive < MSE IW-t:",
  wilcox.test(all_results[["yhat_naive", "y"]],
    all_results[["yhat_iw_true_clipped", "y"]],
    alternative = "less"
  )$p.value,
  "\n"
)
cat("\n")

cat("\nInterpolation vs extrapolation:\n")
x_to_s_results <- transformed_results[[6]]
graph_range <- names(transformed_results)[6]
cat(graph_range, ":\n")
output_table(x_to_s_results,
  columns = c("y", "y_interp", "y_extrap")
  # rows = c("yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped"),
  # row_labels = c("RR", "IW-t", "IW-e")
)
