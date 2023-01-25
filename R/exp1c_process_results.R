source("R/utils.R")

get_mse_results_per_graph <- function(n_iter, n, pos_mode, indep_mode, graph_known) {
    load("data/exp1/valid_graphs.RData")
    mse_data_folder <- sprintf(
        "data/exp1/mse_results_%s_%s_%s_%s_%s",
        n_iter, n, pos_mode, indep_mode, graph_known
    )
    lapply(1:nrow(valid_graphs), function(graph_nr) {
        files <- list.files(
            path = mse_data_folder,
            pattern = glob2rx(sprintf("mse_result_%s_*.RData", graph_nr)),
            full.names = TRUE, recursive = FALSE
        )
        lapply(files, function(filename) {
            load(filename)
            mse_result
        })
    })
}

write_mse_results <- function(
    mse_results_per_graph,
    n_iter, n, pos_mode, indep_mode, graph_known) {
    outfile <- sprintf(
        "output/tables/exp1/results_formatted_%s_%s_%s_%s_%s.txt",
        n_iter, n, pos_mode, indep_mode, graph_known
    )
    cat("n_iter:", n_iter, "\n", file = outfile, append = FALSE)
    cat("n:", n, "\n\n", file = outfile, append = TRUE)

    for (graph_range in list(1:27, 28:51, 1:51)) {
        cat("Combined:", range(graph_range), "\n", file = outfile, append = TRUE)

        all_mse_results <- unlist(mse_results_per_graph[graph_range], recursive = FALSE)
        all_formatted <- get_mse_formatted(all_mse_results)

        write_table(all_formatted, file = outfile, append = TRUE)
    }

    for (i in 1:51) {
        cat("Graph", i, "\n", file = outfile, append = TRUE)
        formatted <- get_mse_formatted(mse_results_per_graph[[i]])
        write_table(formatted, file = outfile, append = TRUE)
    }
}

n_iter <- get_arg_numeric(1)
n <- get_arg_numeric(2)
pos_mode <- get_arg_character(3, "pos")
indep_mode <- get_arg_character(4, "indep")
graph_known <- get_arg_logical(5, FALSE)

mse_results_per_graph <- get_mse_results_per_graph(n_iter, n, pos_mode, indep_mode, graph_known)

save(mse_results_per_graph,
    file = sprintf(
        "output/tables/exp1/results_data_%s_%s_%s_%s_%s.RData",
        n_iter, n, pos_mode, indep_mode, graph_known
    )
)

write_mse_results(mse_results_per_graph, n_iter, n, pos_mode, indep_mode, graph_known)
