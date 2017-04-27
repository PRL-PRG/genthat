
library(testthat)
library(devtools)

source("./utils.R")

context("Testcase generation from traces")

test_that("generate_tc() basic usage", {
    trace <- list(
        type = "trace",
        func = "add",
        args = "list(call=list(a = 3L, b = 4L),vals=list())",
        retv = "8L"
    )

    test_case <- generate_tc(trace)
    test_case <- generate_tc(trace)
    expect_equal(test_case$type, "testcase")
    expect_match(test_case$source, "test_that\\(")
    expect_true(run_test_case(test_case$source, list(add = function(a, b) a + b + 1)))
})

test_that("generate_tc() basic usage - expressions", {
    trace <- list(
        type = "trace",
        func = "add",
        args = "list(call=list(a = 3L, b = y),vals=list(y = 4))",
        retv = "8L"
    )

    test_case <- generate_tc(trace)
    expect_equal(test_case$type, "testcase")
    expect_match(test_case$source, "test_that\\(")
    expect_true(run_test_case(test_case$source, list(add = function(a, b) a + b + 1)))
})

test_that("gen_tests() basic usage", {
    with_tempdir(function(dir1) {
        genthat:::push_trace(list(
            type = "trace",
            func = "add",
            args = "list(call=list(a = 3L, b = 4L),vals=list())",
            retv = "8L"
        ))

        gen_tests(output_dir = dir1)

        files <- list.files(dir1, no.. = TRUE)
        expect_match(files, c("tc-0.R"));

        contents <- read_file(files[1])
        expect_match(contents, "test_that\\(");
    })
})

## TODO why is this not passing ?
##test_that("gen_from_package() basic usage", {
##    with_tempdir(function(dir1) {
##        gen_from_package(get_example_package_path(), include_tests = TRUE,  output_dir = dir1)
##
##        files <- list.files(dir1, no.. = TRUE)
##        expect_equal(length(files), 7);
##    })
##})
#
#test_that("generate_tc() trace reexecution - positive", {
#    with_tempdir(function(dir1) {
#        trace <- list(
#            type = "trace",
#            func = "base:::sort",
#            args = "list(c(1,4,2))",
#            retv = "c(1,2,4)"
#        )
#        res <- generate_tc(trace)
#        expect_equal(res$type, "testcase")
#    })
#})
#
#test_that("generate_tc() trace reexecution - negative", {
#    with_tempdir(function(dir1) {
#        trace <- list(
#            type = "trace",
#            func = "base:::sort",
#            args = "list(c(1,4,2))",
#            retv = "c(2,4,1)"
#        )
#        res <- generate_tc(trace)
#        expect_equal(res$type, "error")
#        expect_equal(res$error_type, "RETV_MISMATCH")
#    })
#})
#
#test_that("generate_tc() trace reexecution - error thrown", {
#    with_tempdir(function(dir1) {
#        trace <- list(
#            type = "trace",
#            func = "base:::sort",
#            args = "list(list(1,4,2))",
#            retv = "c(1,2,4)"
#        )
#        res <- generate_tc(trace)
#        expect_equal(res$type, "error")
#        expect_equal(res$error_type, "ERROR_THROWN")
#    })
#})
#
