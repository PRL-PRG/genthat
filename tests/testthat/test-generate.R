context("test generation")

test_that("format_calling_args", {
    serializer <- new(Serializer)

    format_args <- function(x) paste(format_calling_args(x, serializer=serializer), collapse=", ")

    expect_equal(format_args(NULL), "")
    expect_equal(format_args(list(1)), "1")
    expect_equal(format_args(list(1, 2)), "1, 2")
    expect_equal(format_args(list(a=1)), "a=1")
    expect_equal(format_args(list(a=1, b=2)), "a=1, b=2")
    expect_equal(format_args(list(a=1, 2, 3)), "a=1, 2, 3")
    expect_equal(format_args(list(a=1, 2, 3, c=4)), "a=1, 2, 3, c=4")
})

test_that("generate_test", {
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive=TRUE))

    t1 <- create_trace(fun="c", pkg=NULL, args=list("1"), retv="1")
    expect_equal(generate_test_file(t1, tmp), file.path(tmp, "_NULL_", "c", "test-1.R"))

    t2 <- create_trace(fun="c", pkg=NULL, args=list("1"))
    expect_error(generate_test_file(t2, tmp), "Trace error: No return value")

    t3 <- create_trace(fun="c", pkg=NULL, failure=simpleError("Ups!"))
    expect_error(generate_test_file(t3, tmp), "Trace error: Ups!")
})

test_that("generate_call supports infix functions for base package", {
    serializer <- new(Serializer)
    trace <- create_trace("%in%", pkg="base", args=list(x=1, table=c(1, 2)), retv=TRUE)
    expect_equal(generate_call(trace, serializer=serializer), "1 %in% c(1, 2)")
})

test_that("generate_call supports infix functions for others", {
    serializer <- new(Serializer)
    trace <- create_trace("%in%", pkg="mypkg", args=list(x=1, table=c(1, 2)), retv=TRUE)
    expect_equal(generate_call(trace, serializer=serializer), "mypkg:::`%in%`(x=1, table=c(1, 2))")
})

test_that("generate escapes global non-syntactic names", {
    serializer <- new(Serializer)
    expect_equal(
        generate_globals(list(`%>%`="A", `__b`="B", c="C"), serializer=serializer),
        paste(c('`%>%` <- "A"', '`__b` <- "B"', 'c <- "C"'), collapse="\n")
    )
})

test_that("generate adds seed", {
    runif(1) # initialize RNG

    seed <- .Random.seed
    trace <- create_trace("f", "p", args=NULL, retv=1, seed=seed)
    test <- generate_test(trace)

    # the header separates sections with blank lines, so match on presence
    # rather than a fixed line index
    expect_true(".Random.seed <<- .ext.seed" %in% test)
    expect_equal(attr(test, "externals"), list2env(list(.ext.seed=seed), parent=emptyenv()))
})

test_that("save_test saves also externals", {
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive=TRUE))

    test <- "x <- .ext.x; y <- .ext.1"
    attr(test, "externals") <- list2env(list(.ext.x=1, .ext.1=2), parent=emptyenv())
    test_file <- save_test("p", "f", test, tmp)

    ext_file <- 'test-1.ext'
    ext <- readRDS(file.path(tmp, 'p', 'f', ext_file))

    expect_equal(length(ext), 2)
    expect_equal(ext$.ext.x, 1)
    expect_equal(ext$.ext.1, 2)
})

