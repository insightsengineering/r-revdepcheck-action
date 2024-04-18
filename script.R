catnl <- function(x = "") cat(sprintf("%s\n", x))
if_error <- function(x, y = NULL) {
  res <- try(x, silent = TRUE)
  if (is(res, "try-error")) {
    return(y)
  } else {
    return(res)
  }
}
`%||%` <- function(x, y) {
  if (!length(x) || is.null(x)) y else x
}

args <- commandArgs(trailingOnly = TRUE)
setwd(normalizePath(file.path(args[1])))

number_of_workers <- 3L #as.integer(args[2])

# Install required packages
cat("Install required packages\n")
install.packages(c(
  "pak"
))
pak::pkg_install(c(
  "cli",
  "miniCRAN",
  "pkgbuild",
  "pkgdepends",
  "r-lib/revdepcheck",
  "usethis",
  "yaml"
), ask = FALSE)

# Read config file
cli::cli_h1("Reading `.revdeprefs.yaml` config file...")
if (!file.exists(".revdeprefs.yaml")) {
  cli::cli_inform("Missing `.revdeprefs.yaml` file.")
  cli::cli_inform("This indicates all reverse dependencies from CRAN.")
  refs <- character(0L)
} else {
  refs <- yaml::read_yaml(".revdeprefs.yaml")
  if (length(refs) == 0) {
    cli::cli_inform("Empty `.revdeprefs.yaml` file")
    cli::cli_inform("This indicates all reverse dependencies from CRAN.")
  }

  if (!is.character(refs) && !is.null(refs)) {
    cli::cli_abort("Unknown structure of `.revdeprefs.yaml` file. Returning.")
    return(NULL)
  }
}
cli::cli_inform("References used:")
cli::cli_bullets(refs)

# init
cli::cli_h1("Initiating...")

## revdepcheck
cli::cli_h2("revdepcheck...")
revdepcheck::revdep_reset()
unlink("./revdep/", recursive = TRUE)
revdepcheck:::db_disconnect(".")
usethis::use_revdep()
revdepcheck:::db_setup(".")

## miniCRAN
cli::cli_h2("miniCRAN...")
minicran_path <- tempfile()
dir.create(minicran_path)
miniCRAN::makeRepo(pkgs = "rlang", path = minicran_path, type = c("source", .Platform$pkgType))
options("repos" = c(
  "minicran" = paste0("file:///", minicran_path),
  getOption("repos")
))
cat("DEBUG: repos\n")
print(getOption("repos"))


# include refs in revdepcheck
cli::cli_h1("Adding refs to revdepcheck...")
## for ref in refs:
## - download package sources
## - build .tar.gz file
## - move .tar.gz file to cache dir and refresh cashe
## - add to revdep
for (ref in refs) {
  cli::cli_inform(sprintf("Adding %s ...", ref))

  ref_parsed <- pkgdepends::parse_pkg_ref(ref)
  pkg <- ref_parsed$package

  if (!is(ref_parsed, "remote_ref_standard") && !is(ref_parsed, "remote_ref_cran")) {
    temp_download_dest_dir <- tempfile()
    dir.create(temp_download_dest_dir)
    x <- pak::pkg_download(ref, dest_dir = temp_download_dest_dir)

    if (file.exists(x$fulltarget)) {
      targz_path <- x$fulltarget
    } else if (file.exists(x$fulltarget_tree)) {
      if (file.info(x$fulltarget_tree)$isdir) {
        targz_path <- pkgbuild::build(
          file.path(x$fulltarget_tree, "package"),
          dest_path = targz_path,
          binary = FALSE,
          vignettes = FALSE
        )
      } else {
        untarred_dir <- tempfile()
        dir.create(untarred_dir)
        untar(x$fulltarget_tree, exdir = untarred_dir)
        sources_dir <- list.dirs(untarred_dir, recursive = FALSE)[1]
        targz_path <- pkgbuild::build(
          sources_dir,
          binary = FALSE,
          vignettes = FALSE
        )
      }
    }

    miniCRAN::addLocalPackage(pkg, dirname(targz_path), minicran_path)

    cli::cli_inform("Added to minicran!")
  }

  revdepcheck::revdep_add(packages = pkg)
  cli::cli_inform("Added to revdep todo!")
}
cli::cli_inform("All references added!")

cli::cli_inform("The current revdep todo (empty indicates the default - all revdeps):")
print(revdepcheck::revdep_todo())


cat("DEBUG: available packages\n")
for (repo in head(getOption("repos"), -1)) {
  cat(repo)
  cat("\n")
  print(row.names(available.packages(repos = repo)))
  cat("---\n")
}

cli::cli_h1("Executing revdepcheck...")
revdepcheck::revdep_check(num_workers = number_of_workers)


cli::cli_h1("Summary...")
print(revdepcheck::revdep_summary())

for (revdep in revdepcheck::revdep_todo()$package) {
  cli::cli_h2(sprintf("Summary for: %s", revdep))
  print(if_error(revdepcheck::revdep_details(revdep = revdep)))
}

cli::cli_h1("Printing the output reports...")

cli::cli_h2("revdep/README.md")
catnl(readLines("revdep/README.md", warn = FALSE))

cli::cli_h2("revdep/problems.md")
catnl(readLines("revdep/problems.md", warn = FALSE))

cli::cli_h2("revdep/failures.md")
catnl(readLines("revdep/failures.md", warn = FALSE))

cli::cli_h2("revdep/cran.md")
catnl(readLines("revdep/cran.md", warn = FALSE))


cli::cli_h1("Check duration...") # this does not include download and install
print(
  setNames(
    do.call(
      rbind.data.frame,
      lapply(
        revdepcheck::revdep_summary(),
        function(i) c(i$package, if_error(i$old[[1]]$duration) %||% "?", if_error(i$new$duration) %||% "?")
      )
    ),
    c("package", "old", "new")
  )
)

stopifnot(identical(readLines("revdep/problems.md", warn = FALSE), "*Wow, no problems at all. :)*"))
