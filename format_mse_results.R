# Run as: Rscript format_mse_results.R <n_iter> <n_obs> <pos_mode> <indep_mode> <graph_known>

source("experiment.R")

get_mse_results_per_graph <- function(n_iter, n, pos_mode, indep_mode, graph_known) {
    load("data/valid_graphs.RData")
    mse_outfolder <- sprintf(
        "output/mse_results/mse_results_%s_%s_%s_%s",
        n, pos_mode, indep_mode, graph_known
    )
    lapply(1:nrow(valid_graphs), function(graph_nr) {
        files <- list.files(
            path = mse_outfolder,
            pattern = glob2rx(sprintf("mse_result_%s_*_%s.RData", graph_nr, n)),
            full.names = TRUE, recursive = FALSE
        )
        lapply(files, function(filename) {
            load(filename)
            mse_result
        })
    })
}

get_mse_stats <- function(list_of_mse_results) {
    means <- list_of_mse_results[[1]]
    means[, ] <- NA
    vars <- means
    for (i in rownames(means)) {
        for (j in colnames(means)) {
            mse_for_type <- sapply(list_of_mse_results, function(e) e[i, j])
            means[i, j] <- mean(mse_for_type)
            vars[i, j] <- var(mse_for_type)
        }
    }
    list(means = means, vars = vars)
}

get_mse_formatted <- function(list_of_mse_results) {
    mse_stats <- get_mse_stats(list_of_mse_results)
    idx_orer <- order(mse_stats$means$y)
    formatted_results <- list_of_mse_results[[1]][idx_orer, ]
    formatted_results[, ] <- NA
    for (i in rownames(formatted_results)) {
        for (j in colnames(formatted_results)) {
            formatted_results[i, j] <- sprintf("%.4e (%.3e)", mse_stats$means[i, j], mse_stats$vars[i, j])
        }
    }
    formatted_results
}

write_table <- function(table, file, append = FALSE) {
    out_temp <- capture.output(table)
    keep <- 1 + nrow(table)
    out <- out_temp[1:keep]
    for (i in 2:(length(out_temp) / keep)) {
        length_skip <- max(nchar(rownames(table))) + 1
        out <- paste(out, substring(out_temp[((i - 1) * keep + 1):(i * keep)], length_skip), sep = "")
    }
    cat(out, "\n", file = file, sep = "\n", append = append)
}

write_mse_results <- function(
    mse_results_per_graph,
    n_iter, n, pos_mode, indep_mode, graph_known) {
    outfile <- sprintf(
        "output/mse_results/mse_results_combined_%s_%s_%s_%s_%s.txt",
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

args <- commandArgs(trailingOnly = TRUE)
n_iter <- as.numeric(args[1])
n <- as.numeric(args[2])
pos_mode <- as.character(args[3])
indep_mode <- as.character(args[4])
graph_known <- as.integer(args[5])
graph_known <- as.logical(if (is.na(graph_known)) 0 else graph_known)
mse_results_per_graph <- get_mse_results_per_graph(n_iter, n, pos_mode, indep_mode, graph_known)

save(mse_results_per_graph,
    file = sprintf(
        "output/mse_results/mse_results_per_graph_%s_%s_%s_%s_%s.RData",
        n_iter, n, pos_mode, indep_mode, graph_known
    )
)

write_mse_results(mse_results_per_graph, n_iter, n, pos_mode, indep_mode, graph_known)
