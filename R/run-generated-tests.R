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

    # Since testthat's 3rd edition the "stop" reporter no longer aborts on the
    # first failing expectation (and the `wrap` argument is gone), so we collect
    # the results with a silent reporter and raise ourselves if any expectation
    # failed or errored.
    results <- testthat::test_file(test, reporter="silent", env=env)

    problems <- unlist(lapply(results, function(res) {
        vapply(
            Filter(function(e) inherits(e, c("expectation_failure", "expectation_error")), res$results),
            function(e) sprintf("Test failed: '%s'\n* %s", res$test, trimws(conditionMessage(e))),
            character(1)
        )
    }))

    if (length(problems) > 0) {
        stop(paste(problems, collapse="\n"))
    }

    results
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

            if (length(res) == 0) {
                stop("testthat::test_file result was empty")
            }

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
