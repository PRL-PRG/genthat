create_trace <- function(fun, pkg=NULL, args=list(), globals=list(), retv, seed, error, failure, skipped=0) {
    stopifnot(is.character(fun) && length(fun) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))
    stopifnot(missing(retv) || missing(error) || missing(failure))

    trace <- list(fun=fun, pkg=pkg, args=as.list(args), globals=as.list(globals))

    if (!missing(seed)) {
        trace$seed <- seed
    }

    if (!missing(retv)) {
        trace$retv <- retv
        class(trace) <- "genthat_trace"
    } else if (!missing(error)) {
        trace$error <- error
        class(trace) <- "genthat_trace_error"
    } else if (!missing(failure)) {
        trace$failure <- failure
        class(trace) <- "genthat_trace_failure"
    } else if (skipped > 0) {
        trace$skipped <- skipped
        class(trace) <- "genthat_trace_skipped"
    } else {
        class(trace) <- "genthat_trace_entry"
    }

    trace
}

#' @export
format.genthat_trace <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_entry <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_error <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}

#' @export
format.genthat_trace_failure <- function(x, ...) {
    paste(utils::capture.output(utils::str(x)), collapse="\n")
}
