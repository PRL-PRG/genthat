## -----------------------------------------------------------------------------
x <- tempfile()
writeLines("ls()", x)
system2(file.path(R.home("bin"), "R"), c("--no-save", "--no-restore"), stdout="", stderr="", stdin=x, env=c("R_TESTS=''"))

