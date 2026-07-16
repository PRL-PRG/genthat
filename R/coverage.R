#' @importFrom covr package_coverage
#' @export
#
# nocov start: drives covr itself, so it cannot run under covr instrumentation
# (nested covr); test-coverage.R skips under covr, leaving these lines uncovered.
compute_tests_coverage <- function(path, tests, quiet=TRUE) {
    stopifnot(is_chr_scalar(path), file.exists(path))

    tmp=tempfile(pattern="genthat-test-coverage-", fileext=".RDS")
    on.exit(if (file.exists(tmp)) file.remove(tmp))

    # deparse the path too: raw interpolation would break on Windows paths,
    # where backslashes (e.g. C:\Users) are parsed as string escapes.
    code <- sprintf(
        'saveRDS(genthat:::do_compute_tests_coverage(%s), %s)',
        paste0(deparse(tests, control=c()), collapse=""),
        deparse(tmp)
    )

    covr::package_coverage(path, type="none", code=code, quiet=quiet)

    result <- readRDS(tmp)
    if (length(result) == 0) {
        return(list())
    }

    raw_coverage <- attr(result, "raw_coverage")

    names(result) <- tests

    coverage <- sapply(result, function(x) if (is.numeric(x)) x[1] else NA)
    elapsed <- sapply(result, function(x) if (is.numeric(x)) x[2] else NA)
    errors <- sapply(result, function(x) if (is.character(x)) x else NA)

    attr(coverage, "elapsed") <- elapsed
    attr(coverage, "errors") <- errors
    attr(coverage, "raw_coverage") <- raw_coverage

    coverage
}

#' @importFrom covr percent_coverage
#
do_compute_tests_coverage <- function(tests) {
    # accessing the covr:::.coverage is a bit of a hack, yet the most effective
    # way how to do get increasing coverage per tests

    n <- length(tests)
    i <- 1

    get_coverage <- function() {
        coverage <- structure(as.list(covr:::.counters), class="coverage")
    }

    result <- lapply(tests, function(test) {
        old_coverage <- list2env(as.list(covr:::.counters), env=new.env(parent=emptyenv()))

        log_debug("Running ", i, "/", n, ":", test)
        i <<- i + 1

        tryCatch({
            time <- genthat:::stopwatch(genthat::test_generated_file(test))
            time <- as.numeric(time, units="secs")

            coverage <- get_coverage()
            coverage <- covr::percent_coverage(coverage)

            log_debug("Finished ", test, " in ", time, " coverage ", coverage)

            c(coverage, time)
        }, error=function(e) {
            # we do not want to capture coverage of failed tests
            # TODO: again not the nicest, we should ask for API to manipulate coverage
            assignInNamespace(".counters", old_coverage, ns="covr")

            log_debug("Failed ", test)
            as_chr_scalar(e$message)
        })
    })

    attr(result, "raw_coverage") <- covr::tally_coverage(get_coverage())
    result
}
# nocov end
