source("R/utils.R")

rows <- c("yhat_naive", "yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped", "yhat_dr_true_clipped", "yhat_dr_est_clipped")
columns <- c("y", "yhat_imputed", "y_weighted_true", "y_weighted_est")
labels <- c("Naive", "RR", "IW-t", "IW-e", "DR-t", "DR-e")

output_table <- function(transformed_results, rows, columns, labels) {
  transformed_results <- transformed_results[rows, columns]
  mse_stats <- get_mse_stats(transformed_results)
  maxes <- apply(mse_stats$means, 2, function(col) col == min(col))
  all_formatted <- get_mse_formatted(transformed_results, bold = maxes)
  mse_results <- table_to_tex(all_formatted[rows, columns])
  for (i in 1:length(mse_results)) {
    cat(labels[i], " & ", mse_results[[i]], "\\\\ \n")
  }
  cat("\n")
}

load("output/tables/exp1/results_data_transformed_60_1000_npos_indep_FALSE.RData")
all_results <- transformed_results[[1]]
graph_range <- names(transformed_results)[1]
cat(graph_range, ":\n")
output_table(results, rows, columns, labels)
cat("\n")

qqnorm(log(all_results[["yhat_iw_true", "y"]]))
qqnorm(log(all_results[["yhat_repeated", "y"]]))
qqnorm(log(all_results[["yhat_naive", "y"]]))
cat(
  "p-value for H1 MSE repeated < MSE naive:",
  wilcox.test(all_results[["yhat_repeated", "y"]],
              all_results[["yhat_naive", "y"]],
              alternative = "less")$p.value,
  "\n"
)
cat(
  "p-value for H1 that MSE naive < MSE IW-t:",
  wilcox.test(all_results[["yhat_naive", "y"]],
              all_results[["yhat_iw_true", "y"]],
              alternative = "less")$p.value,
  "\n"
)
cat("\n")

cat("\nInterpolation vs extrapolation:\n")
x_to_s_results <- transformed_results[[6]]
graph_range <- names(transformed_results)[6]
cat(graph_range, ":\n")
output_table(x_to_s_results, rows, c("y", "y_interp", "y_extrap"), labels)
