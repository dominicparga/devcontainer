# https://wiki.archlinux.org/index.php/R

# The .First function is called after everything else in .Rprofile is executed
.First <- function() {
    # Print a welcome message
    message("Welcome back ", Sys.getenv("USER"), "!")
    message("- working directory is ", getwd())
    collapse_str <- "\n  - "
    message(
        "- .libPaths() is",
        collapse_str,
        paste(.libPaths(), collapse = collapse_str)
    )
}

# number of digits to print. Default is 7, max is 15
options(digits = 12)
# Disable default conversion of character strings to factors
options(stringsAsFactors = FALSE)
# Don't show stars indicating statistical significance in model outputs
options(show.signif.stars = FALSE)
# show all warnings and show them when happening
options(warn = 1)
# post-mortem debugging facilities
error <- quote(dump.frames("${R_HOME}/testdump", TRUE))