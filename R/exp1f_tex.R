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

load("output/tables/exp1/results_data_transformed_20_500_npos_indep_FALSE.RData")
# for(i in c(1:6)) {
# for(i in c(1, 6)) {
for(i in c(1)) {
  graph_range <- names(transformed_results)[i]
  cat(graph_range, ":\n")
  output_table(transformed_results[[i]], rows, columns, labels)
}

