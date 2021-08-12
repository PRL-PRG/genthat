#' @param filter a regular expression matching desired filenames without the '.R' extension
#' @export
#'
# TODO: support commentDontrun, commentDonttest
extract_package_code <- function(pkg,
                                 lib_paths=NULL,
                                 pkg_dir=find.package(pkg),
                                 types=c("examples", "tests", "vignettes", "all"),
                                 output_dir=tempfile(pattern="genthat-extract_package"),
                                 filter=NULL) {

    stopifnot(is_chr_scalar(pkg))
    stopifnot(dir.exists(pkg_dir))
    stopifnot(is.null(filter) || is_chr_scalar(filter))

    if ("all" %in% types) {
        types <- c("examples", "tests", "vignettes")
    }

    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)

    # so the output list is named
    names(types) <- types

    lapply(types, function(type) {
        fun <- switch(
            type,
            examples=extract_package_examples,
            tests=extract_package_tests,
            vignettes=extract_package_vignettes
        )

        # each type has its own folder not to clash with one another
        output <- file.path(output_dir, type)
        stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

        files <- fun(pkg, pkg_dir, output_dir=output, lib_paths=lib_paths)

        if (!is.null(filter)) {
            files <- files[grepl(filter, tools::file_path_sans_ext(files))]
        }

        names(files) <- NULL
        files
    })
}

#' @importFrom tools Rd_db Rd2ex
extract_package_examples <- function(pkg, pkg_dir, output_dir, lib_paths=NULL) {
    db <- tryCatch({
        tools::Rd_db(basename(pkg_dir), lib.loc=dirname(pkg_dir))
    }, error=function(e) {
        c()
    })

    if (!length(db)) {
        return(character())
    }

    files <- names(db)

    examples <- sapply(files, function(x) {
        f <- file.path(output_dir, paste0(basename(x), ".R"))
        tools::Rd2ex(db[[x]], f, defines=NULL)

        if (!file.exists(f)) {
            if (is_debug_enabled()) {
                log_debug("Rd file `", x, "' does not contain any code to be run")
            }
            NA
        } else {
            # prepend the file with library call
            txt <- c(
                paste0("library(", pkg, ")"),
                "",
                "",
                readLines(f)
            )
            writeLines(txt, f)
            f
        }
    })

    na.omit(examples)
}

extract_package_tests <- function(pkg, pkg_dir, output_dir, lib_paths=NULL) {
    test_dir <- file.path(pkg_dir, "tests")

    if (!dir.exists(test_dir)) {
        return(character())
    }

    files <- Sys.glob(file.path(test_dir, "*"))
    file.copy(files, output_dir, recursive=TRUE)

    tests <- file.path(output_dir, basename(files))
    tests <- tests[!dir.exists(tests)]
    tests <- tests[grepl("\\.R$", tests)]

    tests
}

#' @importFrom tools pkgVignettes checkVignettes
extract_package_vignettes <- function(pkg, pkg_dir, output_dir, lib_paths=NULL) {
    vinfo <- tools::pkgVignettes(pkg, source=T, lib.loc=lib_paths)
    if (length(vinfo$docs) == 0) {
        return(character())
    }

    if (length(vinfo$sources) == 0) {
        # so far no sources. The following should generate them if there are any
        # sources in the R code. It might actually run the vignettes as well.
        # That is a pity, but there is no way to tell it not to (the tangle is
        # needed to it extracts the R code)
        tools::checkVignettes(pkg, pkg_dir, tangle=TRUE, weave=FALSE, workdir="src", lib.loc=lib_paths)
    }

    # check if there are some sources
    vinfo <- tools::pkgVignettes(pkg, source=T, lib.loc=lib_paths)
    files <- as.character(unlist(vinfo$sources))
    if (length(files) == 0) {
        return(character())
    }

    file.copy(files, to=output_dir)
    vignettes <- file.path(output_dir, basename(files))

    vignettes
}
