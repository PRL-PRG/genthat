#!/usr/bin/env Rscript

verifyResult <- function(res, expected_retv) {
	isTRUE(all.equal(res, expected_retv))
}

doRuns <- function(name, iterations, innerIterations, params) {
    total <- 0
    for (i in 1:iterations) {

        results <- vector(mode = "list", length = innerIterations)

        startTime <- Sys.time()
        for (k in 1:innerIterations) {
          .Random.seed <<- params$.ext.seed
          # wrap the result in a list to prevent NULL assignments
          # from removing a cell from the vector
          results[[k]] <- list(genthat_extracted_call())
        }
        endTime <- Sys.time()

        for (k in 1:innerIterations) {
          if (!verifyResult(results[[k]], list(params$.ext.retv))) {
              message("Benchmark failed: incorrect result")
              message("res=\n", results[[k]], "\n\nexpected=\n", list(params$retv))
              stop("Benchmark failed")
          }
        }

        runTime <- (as.numeric(endTime) - as.numeric(startTime)) * 1000000

        cat(name, ": iterations=1 runtime: ", round(runTime), "us\n", sep = "")
        total <- total + runTime
    }
    total
}

run <- function(args) {
    if (length(args) < 2 || 3 < length(args))
        stop(printUsage())

    name <- args[[1]]
    numIterations <- strtoi(args[[2]])


    innerIterations <- 1
    if (length(args) >= 3)
      innerIterations <- strtoi(args[[3]])

    Rfile <- normalizePath(paste0(name, ".R"))
    extfile <- normalizePath(paste0(name, ".ext"))

		params <- readRDS(extfile)
    .Random.seed <<- params$.ext.seed
    source(Rfile)
    
    total <- as.numeric(doRuns(name, numIterations, innerIterations, params));
    cat(name, ": ",
        "iterations=", numIterations, "; ",
        "average: ", round(total / numIterations), " us; ",
        "total: ", round(total), "us\n\n", sep="")
    #cat("Total runtime: ", total, "us\n\n", sep="")
}

printUsage <- function() {
    cat("harness.r benchmark num-iterations [inner-iterations]\n")
    cat("\n")
    cat("  benchmark           - benchmark class name (filename without the extension)\n")
    cat("  num-iterations      - number of times to execute benchmark\n")
    cat("  inner-iterations    - number of times the benchmark is executed in an inner loop,\n")
    cat("                        which is measured in total, default: 1\n")

}

run(commandArgs(trailingOnly=TRUE))
