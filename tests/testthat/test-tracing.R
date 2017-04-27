
library(testthat)

source("./utils.R")

context("Trace storing and retrieval")

test_that("basic trace pushing", {
    fn1 <- function(a, b) { a + b + 1L }

    decorate_functions("fn1")
    fn1(4L, 3L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(call=list(4L,3L),vals=list())")
        expect_equal(trace$retv, "8L")
    }
    expect_equal(counter, 1)
})

test_that("basic trace pushing - non-syntactic names", {
    `for if` <- function(a, b) { a + b + 1L }

    decorate_functions("for if")
    `for if`(4L, 3L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "for if")
        expect_equal(trace$args, "list(call=list(4L,3L),vals=list())")
        expect_equal(trace$retv, "8L")
    }
    expect_equal(counter, 1)
})

test_that("basic trace pushing", {
    fn1 <- function(a, b) { a + b + 1L }

    decorate_functions("fn1")

    x <- 42L
    y <- 15L
    fn1(x, y + x)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(call=list(quote(x),quote(y+x)),vals=list(x=42L,y=15L))")
        expect_equal(trace$retv, "100L")
    }
    expect_equal(counter, 1)
})

test_that("can serialize list passed as parameter", {
    decorate_functions("length")

    length(list(a = c(2L, 3L), b = c(4L, 5L)))

    undecorate_all()

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        #todo: expect_equal(trace$args, "list(call=list(quote(x),quote(y+x)),vals=list(x=42L,y=15L))")
        expect_equal(trace$retv, "2L")
    }
    expect_equal(counter, 1)
})


test_that("tracing - names of function parameters enclosed in backticks are supported", {
    fn1 <- function(a, `b c`, `function`) { a + `b c` + `function` + 1L }

    decorate_functions("fn1")
    fn1(4L, 3L, 2L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(call=list(4L,3L,2L),vals=list())")
        expect_equal(trace$retv, "10L")
    }
    expect_equal(counter, 1)
})

test_that("tracing - basic tracing of formula", {
    fn1 <- function(formula) { terms(formula) }

    decorate_functions("fn1")
    fn1(x ~ y)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()

        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(call=list(quote(x~y)),vals=list())")
        expect_equal(trace$retv, "quote(x~y)")
    }
    expect_equal(counter, 1)
})

test_that("tracing - basic tracing of lm formula", {
    lm.coeff <- function(m, d) lm(m, d)$coeff

    decorate_functions("lm.coeff")
    
    l <- list(x=10L, y=20L) #cannot be inlined due to tracing bug
    lm.coeff(x ~ y, l)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()

        expect_equal(trace$func, "lm.coeff")
        expect_equal(trace$args, "list(call=list(quote(x~y),quote(l)),vals=list(l=list(x=10L,y=20L)))")
        expect_match(trace$retv, "^structure\\(readBin\\(as.raw\\(c\\(0,0,0,0,0,0,0x24,0x40,0xa2,0x7,0,0,0,0,0xf0,0x7f\\)\\)")
    }
    expect_equal(counter, 1)
})

