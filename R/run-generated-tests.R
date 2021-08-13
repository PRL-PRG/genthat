#' @export
#
test_generated_file <- function(test) {
    ext_file <- file.path(dirname(test), paste0(tools::file_path_sans_ext(basename(test)), ".ext"))
    env <- if (file.exists(ext_file)) {
        ext <- readRDS(ext_file)
        parent.env(ext) <- testthat::test_env()
        ext
    } else {
        testthat::test_env()
    }

    capture.output(r <- { source(test, local=env); env$genthat_extracted_call() })
    r
}

#' @export
#
run_generated_test <- function(tests, quiet=TRUE) {
    if (length(tests) == 0) {
        return(numeric())
    }

    result <- lapply(tests, function(test) {
        stopifnot(file.exists(test))

        tryCatch({
            if (!quiet) {
                cat("Running ", test, "... ")
            }

            time <- stopwatch(res <- test_generated_file(test))

            time <- as.numeric(time, units="secs")

            if (!quiet) {
                cat("OK (", time, " sec)\n")
            }

            time
        }, error=function(e) {
            if (!quiet) {
                cat("FAILED\n")
            }

            as_chr_scalar(e$message)
        })
    })
    names(result) <- tests

    elapsed <- sapply(result, function(x) if (is.numeric(x)) x else NA)
    errors <- sapply(result, function(x) if (is.character(x)) x else NA)

    attr(elapsed, "errors") <- errors
    elapsed
}
