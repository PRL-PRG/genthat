
toStringLiteral <- function (str) {
    concatVec(deparse(str))
}

listToArgumentList <- function(args) {
    elems <- c()
    argNames <- names(args)
    for (i in seq(along.with = args)) {
        src <- if (length(argNames) != 0 && argNames[[i]] != "") {
            concat(argNames[i], " = ", args[[i]])
        } else {
            args[[i]]
        }
        elems <- c(elems, src) 
    }
    concatVec(elems, sep=", ")
}

#' Do we have access to the function in test generation phase, to recompute the return value from the arguments?
#'
#' @param function_name string name of binding
#' @return boolean value signaling whether evaluating the supplied function name in the global scope yields the function or not.
#'
#' @examples
#' isAccessibleFunction("myFn1") == FALSE
#' isAccessibleFunction("ggplot2::ggplot") == TRUE
isAccessibleFunction <- function(function_name) {
    length(grep("::", function_name)) != 0 # TODO handle functions and package-names containing "::"
}

is_valid_trace <- function(trace) {
    if (
        is.list(trace) &&
        length(trace) == 4 &&
        trace$type == "trace" &&
        is.character(trace$func) &&
        length(trace$func) == 1 &&
        is.character(trace$args) &&
        length(trace$args) == 1 &&
        is.character(trace$retv) &&
        length(trace$retv) == 1
    ) {
        TRUE
    } else {
        FALSE
    }
}

deserialize <- function(x) eval(parse(text=x))

#' @title Generate test case from trace
#'
#' @description This function generates a test case from the passed trace.
#' @param trace trace value
#'
generate_tc <- function(trace) {
  if (!is_valid_trace(trace)) stop("Passed invalid trace to generate_tc()")
  func <- trace$func
  args <- trace$args
  retv <- trace$retv

  # args <=> list(call=list(...),vals=list(...))
  trace_args <- parse(text = args)[[1]]
  call_exprs <- as.list(trace_args[[2]])[-1]
  call_vals <- as.list(trace_args[[3]])[-1]

  warnings <- list()
  error <- NULL
  threw_error <- FALSE

  if (FALSE) {
  # TODO reenable
  #if (isAccessibleFunction(func)) {
      pkgName <- sub("^(.*):::.*$", "\\1", func)
      fn <- eval(parse(text=func))
      pargs <- deserialize(args)
      pretv <- deserialize(retv)
      call <- as.call(c(list(fn), pargs)) # TODO why can't you pass empty list

      new_retv <- withCallingHandlers(
            tryCatch(
                eval(call, envir = getNamespace(pkgName)),
                error = function(e) {
                    threw_error <<- TRUE
                    error <<- e$message
                    NULL
                }
            ),
            warning=function(w) {
                warnings <- listAppend(warnings, w$message)
                invokeRestart("muffleWarning")
            }
      )

      if (threw_error) {
          # TODO generate test-case asserting same error message is thrown
          return(list(
              type = "error",
              error_type = "ERROR_THROWN",
              func = func
          ))
      }

      sameRetv <- isTRUE(all.equal(new_retv, pretv))
      if (!sameRetv) {
          return(list(
              type = "error",
              error_type = "RETV_MISMATCH",
              func = func
          ))
      }
  }

  assignments <- lapply2(call_vals, function(val, name) paste0("\t", name, " <- ", as.character(val), "\n"))
  val_section <- paste(assignments, collapse="")
  call_section <- paste0(func, "(", paste(sapply(call_exprs, deparse), collapse=", "), ")")

  test_body <- concat(
      "\t# expected return value\n",
      "\texpected <- ", retv, "\n",
      "\n",
      "\t# variables used in arguments\n",
      val_section,
      "\n",
      "\texpect_equal(", call_section, ", expected)\n"
  )

  list(
    type = "testcase",
    source = concat(
      "library(testthat)\n",
      "test_that(", deparse(func), ", {\n",
      test_body,
      "})"
    )
  )
}

#' @title Generates tests from captured information.
#'
#' @description This function takes the tracing information collected by capture and generates
#' testthat compatible testcases.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @export
gen_tests <- function(output_dir = "generated_tests") {
    if (missing(output_dir) && !dir.exists(output_dir)) {
        if (!dir.create(output_dir)) {
            stop("Couldn't create output dir!")
        }
    } else if (!dir.exists(output_dir)) {
        stop(paste0("Output dir (", output_dir, ") does not exist"))
    }

    n_success <- 0
    n_failed <- 0
    test_cases <- map_iterator(traces, function(trace) {
        if (trace$type == "trace") {
            res <- generate_tc(trace)
            if (res$type == "error") {
                n_failed <<- n_failed + 1
            } else {
                fname <- file.path(output_dir, paste0("tc-", n_success, ".R"))
                n_success <<- n_success + 1
                write(res$source, file = fname)
            }
        }
    })

    list(n_success = n_success, n_failed = n_failed)
}

