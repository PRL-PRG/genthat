context("ccovrage")

test_that("test coverage", {
    # compute_tests_coverage() runs covr on the target package. When genthat
    # itself is being instrumented (the test-coverage / mutation-testing
    # workflows), this nests covr inside covr, which does not work. The
    # function is still exercised by the regular R CMD check run.
    skip_on_covr()

    with_test_pkgs({
        t1 <- tempfile(pattern="t1")
        t2 <- tempfile(pattern="t2")
        t3 <- tempfile(pattern="t3")
        t4 <- tempfile(pattern="t4")

        on.exit({
            file.remove(c(t1, t2, t3, t4))
        })

        # adds coverage
        cat("library('samplepkg'); test_that('test1', expect_equal(my_add(1, 2), 3))", file=t1)
        # does not add coverage
        cat("library('samplepkg'); test_that('test2', expect_equal(my_add(2, 3), 5))", file=t2)
        # fails so it should not
        cat("library('samplepkg'); test_that('test3', expect_equal(my_public('a'), 'error'))", file=t3)
        # add coverage
        cat("library('samplepkg'); test_that('test3', expect_equal(my_call(my_add, 1, 2), 3))", file=t4)

        coverage <- compute_tests_coverage(
            file.path(dirname(getwd()), "test-pkgs-src", "samplepkg"),
            c(t1, t2, t3, t4)
        )
        expect_true(coverage[1] > 0)
        expect_true(coverage[2] == coverage[1])
        expect_true(is.na(coverage[3]))
        expect_true(coverage[4] > coverage[2])

        errors <- attr(coverage, "errors")
        expect_true(is.na(errors[1]))
        expect_true(is.na(errors[2]))
        expect_true(is.character(errors[3]))
        expect_true(is.na(errors[4]))
    })
})
