library(ggplot2)
methods <- c("yhat_naive", "yhat_repeated", "yhat_iw_true_clipped", "yhat_iw_est_clipped", "yhat_dr_true_clipped", "yhat_dr_est_clipped")
# metrics <- c("y", "yhat_imputed", "y_weighted_true", "y_weighted_est")
# metrics <- c("y", "y_interp", "y_extrap")
# metrics <- c("y", "yhat_true")
metrics <- c("y")
method_labels <- c("Naive", "RR", "IW-t", "IW-e", "DR-t", "DR-e")

transform_gg <- function(data, methods, metrics, method_labels) {
  result <- NA
  for(metric in metrics) {
    for (i in 1:length(methods)) {
      method <- methods[i]
      method_label <- method_labels[i]
      n <- length(data[[method, metric]])
      result <- rbind(result, data.frame(method=replicate(n, method_label),
                                         value=data[[method, metric]],
                                         metric=replicate(n, metric)))
    }
  }
  result[!is.na(result$metric),]
}

load("~/Documents/PhD/debiased_regression/output/tables/exp1/results_data_transformed_20_500_npos_indep_FALSE.RData")
for(i in c(1, 6)) {
  graph_range <- names(transformed_results)[i]
  cat(graph_range, ":\n")
  results <- transformed_results[[i]]
  if(graph_range == "x_to_s") {
    metrics_for_graph_range <- c(metrics, "y_interp", "y_extrap")
  } else {
    metrics_for_graph_range <- metrics
  }
  plot(ggplot(transform_gg(results, methods, metrics_for_graph_range, method_labels), 
              aes(x = method, y = value, color = metric)) + 
         geom_boxplot(outlier.shape = NA) + scale_y_log10() + ggtitle(graph_range) +
       ylim(0, 3)
       )
  # boxplot(results[rows, "y"], names = labels, log="y")
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
