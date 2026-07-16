context("extract-package-code")

test_that("extract package code work on sample package", {
    skip_on_cran()
    skip_on_ci()

    tmp <- tempfile()
    on.exit(unlink(tmp, recursive=TRUE))

    with_test_pkgs({
        examples <- file.path(tmp, "examples", c("My-add.R", "My-call.R", "My-public.R"))
        tests <- file.path(tmp, "tests", "testthat.R")
        vignettes <- file.path(tmp, "vignettes",
            c("my-ext-vignette-notrace.R", "my-ext-vignette-trace.R", "my-vignette.R"))

        # test examples
        ret <- extract_package_code("samplepkg", types="examples", output_dir=tmp)
        expect_length(ret, 1)
        expect_equal(ret$examples, examples)
        expect_true(all(file.exists(examples)))

        # test tests
        ret <- extract_package_code("samplepkg", types="tests", output_dir=tmp)
        expect_length(ret, 1)
        expect_equal(ret$tests, tests)

        # test vignettes
        ret <- extract_package_code("samplepkg", types="vignettes", output_dir=tmp)
        expect_length(ret, 1)
        expect_equal(ret$vignettes, vignettes)

        # test all
        ret <- extract_package_code("samplepkg", types="all", output_dir=tmp)
        expect_length(ret, 3)
        expect_equal(unlist(ret, use.names=FALSE), c(examples, tests, vignettes))

        # test filter
        ret <- extract_package_code("samplepkg", types="examples", filter="add", output_dir=tmp)
        expect_length(ret, 1)
        expect_equal(ret$examples, examples[1])

        # empty package
        ret <- extract_package_code("emptypkg", types="all", output_dir=tmp)
        expect_equal(unlist(ret, use.names=FALSE), character())
    })
})

