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
number_of_workers <- as.integer(args[2])
timeout <- as.integer(args[3])

# Install required packages
catnl("Installing required packages...")
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
cli::cli_h1("Configuration")
cli::cli_progress_bar()
cli::cli_progress_step("Reading `.revdeprefs.yaml` config file...")
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
cli::cli_progress_done()
cli::cli_inform("References used:")
cli::cli_bullets(refs)


# init
cli::cli_h1("Initiate pre-requisites")
cli::cli_progress_bar()

## install pkg
# cache pkg and its dependencies
cli::cli_progress_step("Installing the package...")
pkg_name <- read.dcf("DESCRIPTION")[1, "Package"][[1]]
crancache::install_packages(pkg_name, quiet = TRUE)

## revdepcheck
cli::cli_progress_step("Initiating `revdepcheck`...")
revdepcheck::revdep_reset()
unlink("./revdep/", recursive = TRUE)
revdepcheck:::db_disconnect(".")
usethis::use_revdep()
revdepcheck:::db_setup(".")

## miniCRAN
cli::cli_progress_step("Initiating `miniCRAN`...")
minicran_path <- tempfile()
dir.create(minicran_path)
# added `rlang` as a dummy package as the `pkgs` arg cannot be empty
miniCRAN::makeRepo(pkgs = "rlang", path = minicran_path, type = c("source", .Platform$pkgType))
# add minicran repo path to repos so that revdepcheck can use it
# this is the directory where we will store packages from config file
options("repos" = c(
  "minicran" = paste0("file:///", minicran_path),
  getOption("repos")
))

cli::cli_progress_done()


cli::cli_h1("Add refs to revdepcheck")
# include refs in revdepcheck
## Add revdep to miniCRAN repo so that it can be found by the revdepcheck.
## miniCRAN accepts only prebuilt .tar.gz file and build requires all the dependencies pre-installed.
## This is why we need to install all the deps of revdep (incl. tested package).
## It's important to use `crancache` as much as possible to make use of caching.
## Algorithm:
## for ref in refs:
## - install dependencies using `crancache`
## - install pkg using `pak` - this also gives prebuilt .tar.gz file
## - move .tar.gz file to miniCRAN repo
## - add to revdep todo table
cli::cli_progress_bar("Adding refs to revdepcheck", total = length(refs))
for (ref in refs) {
  cli::cli_progress_message("Adding {ref}...")

  ref_parsed <- pkgdepends::parse_pkg_ref(ref)
  ref_pkg <- ref_parsed$package

  if (!is(ref_parsed, "remote_ref_standard") && !is(ref_parsed, "remote_ref_cran")) {
    cli::cli_progress_message("Installing dependencies of {ref}...")
    ref_deps <- pkgdepends::new_pkg_deps(ref, config = list(dependencies = FALSE))
    ref_deps$resolve()
    ref_deps_df <- ref_deps$get_resolution()[1, "deps"][[1]]
    ref_deps_hard <- ref_deps_df[
      tolower(ref_deps_df$type) %in% tolower(pkgdepends::pkg_dep_types_hard()) & ref_deps_df$ref != "R",
      "package"
    ]
    crancache::install_packages(ref_deps_hard, quiet = TRUE)

    cli::cli_progress_message("Installing {ref}...")
    ref_install <- pak::pkg_install(ref)
    ref_cache <- pkgcache::pkg_cache_find(package = ref_pkg)
    ref_targz <- subset(
      ref_cache,
      (built == 1 | built == TRUE | built == "TRUE") & platform == "source" & version == ref_install$version[1],
      fullpath
    )[[1]]
    # cache might have multiple files for a given package and version
    # copy this file to the temp dir and add to miniCRAN from that dir
    temp_dir <- tempfile()
    dir.create(temp_dir)
    file.copy(
      ref_targz,
      file.path(temp_dir, paste0(sub("(.*?_.*?)_.*", "\\1", basename(ref_targz)), ".tar.gz"))
    )
    miniCRAN::addLocalPackage(ref_pkg, temp_dir, minicran_path)

    cli::cli_inform("Added {ref} to minicran!")
  }

  revdepcheck::revdep_add(packages = ref_pkg)
  cli::cli_inform("Added {ref} to revdep todo!")
  cli::cli_progress_update()
}
cli::cli_progress_done()
cli::cli_inform("All references added!")

cli::cli_inform("The current revdep todo (empty indicates the default - all revdeps):")
print(revdepcheck::revdep_todo())


# Execute
cli::cli_h1("Execute revdepcheck")
revdepcheck::revdep_check(num_workers = number_of_workers, timeout = timeout)


# Print results
cli::cli_h1("Summary")
print(revdepcheck::revdep_summary())

for (revdep in revdepcheck::revdep_todo()$package) {
  cli::cli_h2(sprintf("Summary for: %s", revdep))
  if_error(print(revdepcheck::revdep_details(revdep = revdep)))
}

cli::cli_progress_step("Printing the output reports...")

cli::cli_h2("revdep/README.md")
catnl(readLines("revdep/README.md", warn = FALSE))

cli::cli_h2("revdep/problems.md")
catnl(readLines("revdep/problems.md", warn = FALSE))

cli::cli_h2("revdep/failures.md")
catnl(readLines("revdep/failures.md", warn = FALSE))

cli::cli_h2("revdep/cran.md")
catnl(readLines("revdep/cran.md", warn = FALSE))

cli::cli_h2("Check duration...")
# this does not include download and install times
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
