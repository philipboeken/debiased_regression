source("R/utils.R")

rows <- c("yhat_naive", "yhat_recursive", "yhat_ipw_true_clipped", "yhat_ipw_est_clipped", "yhat_dr_true_clipped", "yhat_dr_est_clipped")
columns <- c("yhat_true", "y", "yhat_imputed", "y_weighted_true", "y_weighted_est")
labels <- c("Naive", "RR", "IPW-t", "IPW-e", "DR-t", "DR-e")

output_table <- function(mse_results_per_graph, rows, columns, labels) {
  all_mse_results <- unlist(mse_results_per_graph, recursive = FALSE)
  all_mse_results <- lapply(all_mse_results, function(l) l[rows, columns])
  mse_stats <- get_mse_stats(all_mse_results)
  maxes <- apply(mse_stats$means, 2, function(col) col == min(col))
  all_formatted <- get_mse_formatted(all_mse_results, bold = maxes)
  mse_results <- table_to_tex(all_formatted[rows, columns])
  for (i in 1:length(mse_results)) {
    cat(labels[i], " & ", mse_results[[i]], "\\\\ \n")
  }
  cat("\n")
}

load("output/tables/exp1/results_data_100_1000_pos_indep_FALSE.RData")
output_table(mse_results_per_graph, rows, columns, labels)

load("output/tables/exp1/results_data_100_1000_npos_indep_FALSE.RData")
output_table(mse_results_per_graph, rows, columns, labels)

load("output/tables/exp1/results_data_100_1000_pos_dep_FALSE.RData")
output_table(mse_results_per_graph, rows, columns, labels)

load("output/tables/exp1/results_data_100_1000_npos_dep_FALSE.RData")
output_table(mse_results_per_graph, rows, columns, labels)
