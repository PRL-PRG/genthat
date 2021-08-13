
# Run the generated unit test in a new R process with a known seed
# to record the return value of a reproducible run

run_test_in_isolation <- function(test, seed) {
    callr::r(
        function(test, seed) {
            .Random.seed <<- seed
            source(test)
            genthat_extracted_call()
        },
        args=list(test=test, seed=seed)
    )
}

record_test_exts <- function(test, extfile, seed=NULL) {
    if (is.null(seed)) {
        set.seed(NULL)
        seed <- .Random.seed
    }

    exts <- list(
        .ext.retv = run_test_in_isolation(test, seed),
        .ext.seed = seed
    )

    saveRDS(exts, file=extfile)
}