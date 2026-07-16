context("genthat on sample package")

library(dplyr)
library(stringr)

# The sample package contains the following code:
#
# - examples [6]
#   - My-public.Rd [0]
#     - contains one example which does not touch any of the package code
#   - My-add.Rd [2]
#     - two calls to my_add
#   - My-call.Rd [4]
#     - two calls to my_call
#     - two calls to my_add
# - tests [6]
#   - testthat/testMain.R [8]
#      - one call to my_public
#      - one call to my_private
#      - one call to my_add
#      - four calls to my_call
#      - one call to my_warning
#      - one call to my_error - not recorded
# - vignettes [2]
#   - my-ext-vignette-notrace.Rmd [0]
#     - no calls to simplepkg
#   - my-ext-vigntte-trace.Rmd [1]
#     - one call to my_add
#   - my-vignette.Rmd [1]
#     - one call to my_add

test_that("trace_package works on a single file from sample package", {
    skip_on_cran()
    skip_on_ci()

    with_test_pkgs({
        output_dir <- tempfile()
        f1 <- tempfile()

        on.exit({
            file.remove(c(f1))
            unlink(output_dir, recursive=TRUE)
        })

        # a trace
        cat("samplepkg::my_add(1,1)", file=f1)

        ret <- trace_package("samplepkg", f1, output_dir=output_dir, action="export", quiet=!is_debug_enabled())

        expect_equal(length(ret), 1)
        expect_equal(names(ret), f1)

        expect_equal(ret[[f1]]$output, file.path(output_dir, "samplepkg", "my_add", "trace-1.RDS"))
        expect_equal(ret[[f1]]$error, NA)
    })
})

test_that("trace_package works on a sample package", {
    skip_on_cran()
    skip_on_ci()

    with_test_pkgs({
        output_dir <- tempfile()
        f1 <- tempfile()
        f2 <- tempfile()
        f3 <- tempfile()
        f4 <- tempfile()

        on.exit({
            file.remove(c(f1, f2, f3))
            unlink(output_dir, recursive=TRUE)
        })

        # a trace
        cat("samplepkg::my_add(1,1)", file=f1)
        # no trace
        cat("1+1", file=f2)
        # error
        cat("errorrrr!", file=f3)
        # f4 is deliberately missing

        # test export
        ret <- trace_package(
            "samplepkg",
            c(f1, f2, f3, f4),
            output_dir=output_dir,
            action="export",
            quiet=!is_debug_enabled()
        )

        expect_equal(length(ret), 4)

        expect_equal(ret[[f1]]$output, file.path(output_dir, "samplepkg", "my_add", "trace-1.RDS"))
        expect_equal(ret[[f1]]$error, NA)
        expect_equal(nrow(ret[[f2]]), 0)
        expect_equal(nrow(ret[[f3]]), 0)
        expect_equal(ret[[f4]], paste(f4, "does not exist"))

        # test generate
        ret <- trace_package(
            "samplepkg",
            c(f1, f2, f3, f4),
            output_dir=output_dir,
            action="generate",
            quiet=!is_debug_enabled()
        )

        expect_equal(length(ret), 4)

        expect_equal(ret[[f1]]$output, file.path(output_dir, "samplepkg", "my_add", "test-1.R"))
        expect_equal(ret[[f1]]$error, NA)
        expect_equal(nrow(ret[[f2]]), 0)
        expect_equal(nrow(ret[[f3]]), 0)
        expect_equal(ret[[f4]], paste(f4, "does not exist"))
    })
})

test_that("gen_from_package works on a sample package", {
    skip_on_cran()
    skip_on_ci()

    ret <- with_test_pkgs({
        output_dir <- tempfile()

        on.exit({
            unlink(output_dir, recursive=TRUE)
        })

        gen_from_package(
            "samplepkg",
            types="all",
            tracer="sequence",
            output_dir=output_dir,
            action="export",
            quiet=!is_debug_enabled()
        )
    })

    # check by sources
    expect_nrow(dplyr::filter(ret, str_detect(file, "My-add.Rd.R$")), 2)
    expect_nrow(dplyr::filter(ret, str_detect(file, "My-call.Rd.R$")), 4)
    expect_nrow(dplyr::filter(ret, str_detect(file, "testthat.R$")), 8)
    expect_nrow(dplyr::filter(ret, str_detect(file, "my-ext-vignette-trace.R$")), 1)
    expect_nrow(dplyr::filter(ret, str_detect(file, "my-vignette.R$")), 1)

    # check by functions
    expect_nrow(dplyr::filter(ret, str_detect(output, "/my_add/")), 7)
    expect_nrow(dplyr::filter(ret, str_detect(output, "/my_call/")), 6)
    expect_nrow(dplyr::filter(ret, str_detect(output, "/my_public/")), 1)
    expect_nrow(dplyr::filter(ret, str_detect(output, "/my_warning/")), 1)
    expect_nrow(dplyr::filter(ret, str_detect(output, "/my_private/")), 1)

    # check that =there is no error
    expect_equal(sum(is.na(ret$error)), 16)
})

test_that("gen_from_package works on a sample package", {
    skip_on_cran()
    skip_on_ci()

    with_test_pkgs({
        output_dir <- tempfile()

        on.exit({
            unlink(output_dir, recursive=TRUE)
        })

        ret <- gen_from_package(
            "emptypkg",
            types="all",
            tracer="sequence",
            output_dir=output_dir,
            action="export",
            quiet=!is_debug_enabled()
        )

        expect_equal(nrow(ret), 0)
        expect_equal(names(ret), c("file", "output", "error"))
    })
})
