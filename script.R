

args <- commandArgs(trailingOnly = TRUE)
path <- normalizePath(file.path(".", args[1]))

cat("Path =", path)
