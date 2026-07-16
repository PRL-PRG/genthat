format_calling_args <- function(args, include_names=TRUE, serializer) {
    args <- lapply(args, serializer$serialize_value)

    if (include_names && !is.null(names(args))) {
        pairs <- zip(name=names(args), value=args)

        lapply(pairs, function(x) {
            if(!is_empty_str(x$name)) {
                paste0(escape_name(x$name), "=", x$value)
            } else {
                x$value
            }
        })
    } else {
        args
    }
}

dump_raw_trace <- function(trace) {
    s <- utils::capture.output(utils::str(trace))
    s <- paste("# ", s, collapse="\n")
    s <- paste0("# TRACE:\n", s, "\n\n")
    s
}

generate_call <- function(trace, serializer) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    pkg <- trace$pkg
    if (is.null(pkg)) {
        pkg <- ""
    }

    if (is_infix_fun(fun) && pkg == "base") {
        args <- format_calling_args(trace$args, include_names=FALSE, serializer)

        if (length(args) != 2) {
            stop("Call to infix function: `", fun,
                 "` does not have exactly two arguments, instead it has: ",
                 paste(args, collapse=", "))
        }

        sep <- if (is_infix_fun_no_space(fun)) "" else " "

        paste(args[[1]], fun, args[[2]], collapse=sep)
    } else {
        args <- format_calling_args(trace$args, include_names=TRUE, serializer)
        args_str <- paste(args, collapse=", ")
        fun <- escape_name(fun)
        pkg <- trace$pkg
        pkg <- if (is.null(trace$pkg)) "" else paste0(pkg, ":::")

        paste0(pkg, fun, '(', args_str, ')')
    }
}

generate_globals <- function(globals, serializer) {
    names <- sapply(names(globals), escape_name, USE.NAMES=FALSE)
    values <- lapply(globals, serializer$serialize_value)

    paste(names, values, sep=" <- ", collapse="\n")
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#' @param include_trace_dump whether to include raw trace at the beginning of the test
#' the trace is formatted using `str` function.
#'
#' @importFrom utils str
#' @export
generate_test <- function(trace, ...) {
    UseMethod("generate_test")
}

#' @export
generate_test.genthat_trace <- function(trace, include_trace_dump=FALSE, format_code=TRUE) {
    tryCatch({
        externals <- new.env(parent=emptyenv())
        serializer <- new(Serializer)
        call <- generate_call(trace, serializer)
        globals <- generate_globals(trace$globals, serializer)
        retv <- serializer$serialize_value(trace$retv)

        header <- "library(testthat)\n\n"
        if (include_trace_dump) {
            header <- paste(header, dump_raw_trace(trace), sep="\n")
        }

        if (!is.null(trace$seed)) {
            # .Random.seed is only looked in user environment
            header <- paste0(header, ".Random.seed <<- .ext.seed\n\n")
            externals$.ext.seed <- trace$seed
        }

        code <- paste0(
            header,
            'test_that("', trace$fun, '", {\n',
            globals,
            if (nchar(globals) > 0) '\n' else '',
            '\nexpect_equal(', call, ', ', retv, ')\n})'
        )

        if (format_code) {
            code <- reformat_code(code)
        }

        serializer$externals(externals)
        attr(code, "externals") <- externals

        code
    }, error=function(e) {
        # this so we can have a systematic prefix for the error message
        # which helps to filter problems by their class
        stop(simpleError(paste("Generate error:", trimws(e$message, which="both")), e$call))
    })
}

#' @export
generate_test.genthat_trace_entry <- function(trace, ...) {
    stop("Trace error: No return value")
}

#' @export
generate_test.genthat_trace_skipped <- function(trace, ...) {
    stop("Trace error: Trace too big (", trace$skipped, ")")
}

#' @export
generate_test.genthat_trace_error <- function(trace, ...) {
    stop(paste("Code error:", trace$error$message))
}

#' @export
generate_test.genthat_trace_failure <- function(trace, ...) {
    stop(paste("Trace error:", trace$failure$message))
}

#' @param tests this should be a data.frame with class genthat_tests, a result
#'     from calling `generate_tests`.
#' @export
#'
save_test <- function(pkg, fun, code, output_dir) {
    stopifnot(is_chr_scalar(pkg))
    stopifnot(is_chr_scalar(fun))
    stopifnot(is.character(code), length(code) > 0)
    stopifnot(is_chr_scalar(output_dir))

    dname <- file.path(output_dir, sanitize_path_name(pkg), sanitize_path_name(fun))
    stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

    fname <- next_file_in_row(file.path(dname, "test.R"))

    externals <- attr(code, "externals")
    if (length(externals) > 0) {
        fname_ext <- paste0(tools::file_path_sans_ext(fname), ".ext")
        saveRDS(externals, fname_ext)
    }

    write(paste(code, collapse="\n\n"), file=fname)

    fname
}

reformat_code <- function(code) {
    tryCatch({
        code <- formatR::tidy_source(
            text=code,
            output=FALSE,
            comment=FALSE,
            blank=TRUE,
            arrow=TRUE,
            brace.newline=FALSE,
            indent=4,
            width.cutoff=120
        )

        code <- code$text.tidy

        paste(code, collapse="\n")
        code
    })
}

#' @title generate test from a trace
#' @description given a trace, it generates a test and stores it in a file
#' @return a data frame with the following
#' pkg        : chr
#' fun        : chr
#' filename   : chr (can be NA in which case error must be a chr)
#' error      : chr (can be NA in which case code must be NA)
#' elapsed    : numeric (can be NA)
#'
#' @export
#'
generate_test_file <- function(trace, output_dir, ...) {
    code <- generate_test(trace, ...)

    pkg <- if (is.null(trace$pkg)) "_NULL_" else trace$pkg
    fun <- if (is.null(trace$fun)) "_NULL_" else trace$fun

    save_test(pkg, fun, code, output_dir)
}
