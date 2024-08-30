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
check_if_pkg_available <- function(pkg, ver) {
  length(
    available.packages(
      filter = list(
        add = TRUE,
        function (db) {
          db[db[, "Package"] == pkg & db[, "Version"] == ver, ]
        }
      )
    )
  ) > 0
}
install_and_add_to_minicran <- function(pkg, minicran_path) {
  avail_pkgs <- rownames(available.packages())
  x <- pak::pkg_install(pkg)
  for (i in seq_len(nrow(x))) {
    i_package <- x$package[i]
    i_version <- x$version[i]
    if (check_if_pkg_available(i_package, i_version)) next
    i_cache <- pkgcache::pkg_cache_find(package = i_package, version = i_version, platform = "source")
    if (nrow(i_cache) == 0) next
    i_targz <- i_cache$fullpath[1]
    temp_dir <- tempfile()
    on.exit(unlink(temp_dir))
    dir.create(temp_dir)
    file.copy(
      i_targz,
      file.path(temp_dir, paste0(i_package, "_", i_version, ".tar.gz"))
    )
    miniCRAN::addLocalPackage(i_package, temp_dir, minicran_path)
  }
  invisible(NULL)
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
miniCRAN::makeRepo(pkgs = "rlang", path = minicran_path, type = c("source", .Platform$pkgType))
# add minicran repo path to repos so that revdepcheck can use it
# this is the directory where we will store packages from config file
options("repos" = c(
  "minicran" = paste0("file:///", minicran_path),
  getOption("repos")
))

## install pkg
cli::cli_progress_step("Installing the package (CRAN)...")
pkg_name <- read.dcf("DESCRIPTION")[, "Package"]
pkg_ref_released <- if (pkg_name %in% rownames(available.packages())) {
  pkg_name
} else {
  # @TODO: think of a better way to get ref for released version of non-CRAN packages
  read.dcf("DESCRIPTION")[1, "URL"] |>
    gsub("\n", "", x = _) |>
    gsub("/$", "", x = _) |>
    strsplit(x = _, split = ",") |>
    _[[1]] |>
    grep(x = _, "github.com", value = TRUE) |>
    gsub(".*github.com/", "\\1", x = _) |>
    paste0("@*release")
}
install_and_add_to_minicran(pkg_ref_released, minicran_path)
cli::cli_progress_step("Installing the package (DEV)...")
install_and_add_to_minicran(".", minicran_path)

cli::cli_progress_done()


cli::cli_h1("Add refs to revdepcheck")
# include refs in revdepcheck
## Add refs revdepcheck and also to miniCRAN so that it can be found from there
cli::cli_progress_bar("Adding refs to revdepcheck", total = length(refs))
for (ref in refs) {
  cli::cli_progress_message("Adding {ref}...")

  install_and_add_to_minicran(ref, minicran_path)

  ref_pkg <- pkgdepends::parse_pkg_ref(ref)$package
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
if (length(revdepcheck::revdep_summary())) {
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
} else {
  print("(empty)")
}

stopifnot(identical(readLines("revdep/problems.md", warn = FALSE), "*Wow, no problems at all. :)*"))
