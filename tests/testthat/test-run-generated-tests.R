context("run-generated-tests")

run_generated_code <- function(code) {
    tmp <- tempfile()
    on.exit(file.remove(tmp))

    writeLines(code, tmp)
    run_generated_test(tmp)
}

test_that("empty test does not run", {
    x <- run_generated_code("")

    expect_true(is.na(x))
    expect_equivalent(attr(x, "errors"), "testthat::test_file result was empty")
})

test_that("test failure", {
    x <- run_generated_code("test_that('x', expect_false(TRUE))")

    expect_true(is.na(x))
    # the exact wording of the failure comes from testthat and varies between
    # versions, so only assert on the stable parts
    expect_match(attr(x, "errors"), "Test failed: 'x'", fixed=TRUE)
    expect_match(attr(x, "errors"), "TRUE", fixed=TRUE)
    expect_match(attr(x, "errors"), "FALSE", fixed=TRUE)
})

test_that("test exception", {
    x <- run_generated_code("test_that('x', stop('problem'))")

    expect_true(is.na(x))
    expect_match(attr(x, "errors"), "Test failed: 'x'", fixed=TRUE)
    expect_match(attr(x, "errors"), "problem", fixed=TRUE)
})

test_that("test no tests", {
    x <- run_generated_code("test_that('x', 1+1)")

    expect_true(is.numeric(x))
    expect_true(is.na(attr(x, "errors")))
})

test_that("test warning", {
    x <- run_generated_code("test_that('x', { warning('w1'); expect_true(TRUE) })")

    expect_true(is.numeric(x))
    expect_true(is.na(attr(x, "errors")))
})

test_that("test success", {
    x <- run_generated_code("test_that('x', expect_true(TRUE))")

    expect_true(is.numeric(x))
    expect_true(is.na(attr(x, "errors")))
})
