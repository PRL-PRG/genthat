context("genthat")

test_that("tracing control work", {
    capture <- list()

    f <- function(x,y) x + y
    decorate_function(f, record_fun=function(...) capture <<- list(...))

    disable_tracing()
    f(1L, 2L)

    expect_equal(is_tracing_enabled(), FALSE)
    expect_equal(length(capture), 0L)

    enable_tracing()
    f(1L, 2L)

    expect_equal(is_tracing_enabled(), TRUE)
    expect_equal(length(capture), 6L)
    expect_equal(capture$retv, 3L)
})

test_that("process_traces with export action work", {
    tmp <- chartr("\\", "/", tempfile())

    on.exit({
        unlink(tmp, recursive=TRUE)
    })

    expect_false(dir.exists(tmp))

    trace_1 <- create_trace("fun1")
    trace_2 <- create_trace("fun2", retv=1)
    trace_3 <- create_trace("fun3", error=simpleError("Bad call"))
    trace_4 <- create_trace("fun4", failure=simpleError("Something is wrong"))

    ret <- process_traces(list(trace_1, trace_2, trace_3, trace_4), output_dir=tmp, action="export")

    expect_equal(nrow(ret), 4)

    expect_equivalent(ret[1, 1], file.path(tmp, "_NULL_", "fun1", "trace-1.RDS"))
    expect_equivalent(ret[2, 1], file.path(tmp, "_NULL_", "fun2", "trace-1.RDS"))
    expect_equivalent(ret[3, 1], file.path(tmp, "_NULL_", "fun3", "trace-1.RDS"))
    expect_equivalent(ret[4, 1], NA_character_)

    expect_equivalent(ret[, 2], c(NA, NA, NA, "Something is wrong"))

    rdss <- lapply(ret[1:3, 1], readRDS)
    expect_equal(rdss, list(trace_1, trace_2, trace_3))

    # test with empty input
    ret <- process_traces(list(), output_dir=tmp, action="export")
    expect_equal(dim(ret), c(0, 2))
})

test_that("process_traces with generate action work", {

    tmp <- chartr("\\", "/", tempfile())

    on.exit({
        unlink(tmp, recursive=TRUE)
    })

    expect_false(dir.exists(tmp))

    trace_1 <- create_trace("fun1")
    trace_2 <- create_trace("fun2", retv=1)
    trace_3 <- create_trace("fun3", error=simpleError("Bad call"))
    trace_4 <- create_trace("fun4", failure=simpleError("Something is wrong"))

    withr::with_options(list(genthat.keep_failed_traces=TRUE), {
        ret <- process_traces(list(trace_1, trace_2, trace_3, trace_4), output_dir=tmp, action="generate")
    })

    expect_equal(nrow(ret), 4)

    expect_equivalent(ret[1, 1], file.path(tmp, "_NULL_", "fun1", "failed-trace-1.RDS"))
    expect_equivalent(ret[2, 1], file.path(tmp, "_NULL_", "fun2", "test-1.R"))
    expect_equivalent(ret[3, 1], file.path(tmp, "_NULL_", "fun3", "failed-trace-1.RDS"))
    expect_equivalent(ret[4, 1], file.path(tmp, "_NULL_", "fun4", "failed-trace-1.RDS"))

    expect_equivalent(ret[, 2], c(
        "Trace error: No return value",
        NA,
        "Code error: Bad call",
        "Trace error: Something is wrong"
    ))

    rdss <- lapply(ret[c(1,3,4), 1], readRDS)
    expect_equal(rdss, list(trace_1, trace_3, trace_4))

    # test with empty input
    ret <- process_traces(list(), output_dir=tmp, action="generate")
    expect_equal(dim(ret), c(0, 2))
})
