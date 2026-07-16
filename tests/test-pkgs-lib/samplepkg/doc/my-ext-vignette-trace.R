## -----------------------------------------------------------------------------
x <- tempfile()
on.exit(file.remove(x))
writeLines(c("library(samplepkg)", "my_add(2,3)"), x)
system2(file.path(R.home("bin"), "R"), c("--no-save", "--no-restore"), stdin=x, env=c("R_TESTS=''"))

