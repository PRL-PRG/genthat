#!/usr/bin/env Rscript

# Run this file either as a script:
#
#    (bash)
#    $ GENERATED_TESTS_DIR="./tests" ./test_files.R
#    $ ./test_files.R ./tests/yaml/as.yaml/test-36.R ./tests/yaml/yaml.load/test-57.R
#
# or with testthat::test_file()
#
#    (R)
#    > withr::with_envvar(
#          list(GENERATED_TESTS_DIR="./tests"),
#          testthat::test_file("./test_files.R")
#      )

GENERATED_TESTS_DIR=Sys.getenv("GENERATED_TESTS_DIR", unset=NA)

run_file <- function(Rfile, seed) {
    .Random.seed <<- seed
    source(Rfile)

    .Random.seed <<- seed
    genthat_extracted_call()
}


test_file <- function(Rfile) {
    extfile <- gsub(".R$", ".ext", Rfile)
		params <- readRDS(extfile)
    testthat::test_that(Rfile, testthat::expect_equal(run_file(Rfile, params$.ext.seed), params$.ext.retv))
}


run <- function(args) {
    Rfiles <- list()

    if (length(args) >= 1 ) {
      Rfiles <- args
    } else if (! is.na(GENERATED_TESTS_DIR)) {
      Rfiles <- list.files(GENERATED_TESTS_DIR, recursive=TRUE, pattern="\\.R$", full.names=TRUE)
    } else {
      printUsage()
      stop("No arguments and GENERATED_TESTS_DIR is unset.")
    }

    invisible(lapply(Rfiles, test_file))
}

printUsage <- function() {
  message("test_files.R [test1.R test2.R test3.R ...]")
  message(" testk.R     - the files to test. Should be accompanied by a corresponding testk.ext file.")
  message("               If no files are specified, all the files in the GENERATED_TESTS_DIR directory are run")
  message("")
}

run(commandArgs(trailingOnly=TRUE))
