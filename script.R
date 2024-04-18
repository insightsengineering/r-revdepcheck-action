args <- commandArgs(trailingOnly = TRUE)
setwd(normalizePath(file.path(args[1])))

number_of_workers <- 1L #as.integer(args[2])

# Install required packages
cat("Install required packages\n")
install.packages(c(
  "pak",
  "cranlike",
  "pkgdepends",
  "yaml",
  "cli",
  "usethis",
  "pkgbuild"
))
pak::pkg_install(c(
  "r-lib/revdepcheck",
  "r-lib/crancache"
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

## clean & refresh cache
cli::cli_h2("crancache...")
crancache::crancache_clean()
invisible(crancache::available_packages())

cat("DEBUG: `crancache::get_cache_package_dirs()`\n")
print(crancache::get_cache_package_dirs())
cat("DEBUG: `crancache:::get_crancache_repos(\"source\")`\n")
print(crancache:::get_crancache_repos("source"))

cache_dir <- crancache::get_cache_package_dirs()[["other/source"]]
options("repos" = c(
  crancache:::get_crancache_repos("source"),
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
    pkgcache::pkg_cache_delete_files(package = pkg)

    temp_download_dest_dir <- tempfile()
    dir.create(temp_download_dest_dir)
    x <- pak::pkg_download(ref, dest_dir = temp_download_dest_dir)

    if (file.exists(x$fulltarget)) {
      targz_path <- x$fulltarget
    } else if (file.exists(x$fulltarget_tree)) {
      targz_path <- tempfile(fileext = ".tar.gz")
      if (file.info(x$fulltarget_tree)$isdir) {
        pkgbuild::build(
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
        pkgbuild::build(
          sources_dir,
          dest_path = targz_path,
          binary = FALSE,
          vignettes = FALSE
        )
        unlink(untarred_dir, recursive = TRUE)
      }
    }

    # copy .tar.gz file into pkgcache dir
    target_path <- file.path(cache_dir, basename(x$fulltarget))
    file.copy(targz_path, target_path)
    unlink(targz_path)

    cranlike::update_PACKAGES(cache_dir)
    cli::cli_inform("Added to crancache!")
  }

  revdepcheck::revdep_add(packages = pkg)
  cli::cli_inform("Added to revdep todo!")
}
cli::cli_inform("All references added!")

cli::cli_inform("The current revdep todo (empty indicates the default - all revdeps):")
print(revdepcheck::revdep_todo())


cat("DEBUG: list dirs of cache_dir\n")
print(list.dirs(crancache::get_cache_dir()))
cat("DEBUG: available packages\n")
for (repo in getOption("repos")) {
  cat(repo)
  cat("\n")
  print(row.names(available.packages(repos = repo)))
  cat("---\n")
}

cli::cli_h1("Executing revdepcheck...")
revdepcheck::revdep_check(num_workers = number_of_workers)


cli::cli_h1("Printing the output reports...")
catnl <- function(x = "") cat(sprintf("%s\n", x))

cli::cli_h2("revdep/README.md")
catnl(readLines("revdep/README.md", warn = FALSE))

cli::cli_h2("revdep/problems.md")
catnl(readLines("revdep/problems.md", warn = FALSE))

cli::cli_h2("revdep/failures.md")
catnl(readLines("revdep/failures.md", warn = FALSE))

cli::cli_h2("revdep/cran.md")
catnl(readLines("revdep/cran.md", warn = FALSE))


stopifnot(identical(readLines("revdep/problems.md", warn = FALSE), "*Wow, no problems at all. :)*"))
