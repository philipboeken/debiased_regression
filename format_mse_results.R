source("experiment.R")

get_mse_formatted <- function(list_of_mse_results) {
    formatted_results <- list_of_mse_results[[1]]
    formatted_results[, ] <- NA
    for (i in rownames(formatted_results)) {
        for (j in colnames(formatted_results)) {
            mse_for_type <- sapply(list_of_mse_results, function(e) e[i, j])
            formatted_results[i, j] <- sprintf("%.4e (%.3e)", mean(mse_for_type), var(mse_for_type))
        }
    }
    formatted_results
}

args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])
n_iter <- numeric(1)
load("data/valid_graphs.RData")
mse_results_per_graph <- lapply(1:nrow(valid_graphs), function(graph_nr) {
    files <- list.files(
        path = "output/mse_results",
        pattern = glob2rx(sprintf("mse_result_%s_*_%s.RData", graph_nr, n)),
        full.names = TRUE, recursive = FALSE
    )
    n_iter <<- length(files)
    lapply(files, function(filename) {
        load(filename)
        mse_result
    })
})

all_mse_results <- unlist(mse_results_per_graph, recursive = FALSE)

outfile <- sprintf("output/mse_results_combined_%s_%s.txt", n_iter, n)
cat("n_iter:", n_iter, "\n", file = outfile, append = FALSE)
cat("n:", n, "\n\n", file = outfile, append = TRUE)

cat("All combined:", "\n", file = outfile, append = TRUE)
all_formatted <- get_mse_formatted(all_mse_results)

write_table(all_formatted, file = outfile, append = TRUE)

for (i in 1:length(mse_results_per_graph)) {
    cat("Graph", i, "\n", file = outfile, append = TRUE)
    formatted <- get_mse_formatted(mse_results_per_graph[[i]])
    write_table(formatted, file = outfile, append = TRUE)
}

