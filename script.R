# Introduction:
# The `revdepcheck` package uses the `crancache::install_packages()` function for package installation. This function, in turn, utilizes `utils::install.packages()` and incorporates an additional caching mechanism for efficiency.
#
# Current Limitations:
# Optimising this behavior to use `pak` (and family) would require considerable changes to the source code, which is currently not feasible. There is an existing issue on this topic (https://github.com/r-lib/revdepcheck/issues/187), but it's unlikely to be addressed in the near future.
#
# Proposed Solution:
# The proposed solution is to create cranlike local repo, use `pak` for installation, find its cache and copy the binaries to cranlike repo and finally use this cranlike repo in revdepcheck. This way we can benefit from pak installation features (incl. Remotes, caching, PPM) and still use revdepcheck for reverse dependency checks.
#
# Design Overview:
# 1. Create a CRAN-like repository using the `miniCRAN` package.
# 2. Install the target package (both CRAN and DEV) and its reverse dependencies using the `pak` package.
# 3. If a package is not available on the default CRAN (e.g., it's hosted on GitHub), retrieve the cached binaries and add them to the miniCRAN repository.
# 4. Run `revdepcheck::revdep_check()` with local CRAN-like repository to check reverse dependencies.

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
`%df_empty%` <- function(x, y) {
  if (is.data.frame(x) && nrow(x) == 0) y else x
}
available_packages <- as.data.frame(available.packages())
check_if_pkg_available <- function(pkg, ver = NULL) {
  if (is.null(ver)) {
    nrow(subset(available_packages, Package == pkg)) > 0
  } else {
    nrow(subset(available_packages, Package == pkg & Version >= ver)) > 0
  }
}
check_if_added <- function(pkg, ver = NULL, minicran_path) {
  minicran_ap <- as.data.frame(available.packages(repos = paste0("file:///", minicran_path)))
  if (nrow(minicran_ap) == 0) return(FALSE)
  if (is.null(ver)) {
    nrow(subset(minicran_ap, Package == pkg)) > 0
  } else {
    nrow(subset(minicran_ap, Package == pkg & Version == ver)) > 0
  }
}
get_tar_gz <- function(pkg, version, path) {
  if (!grepl(".*.tar.gz$", path) && !grepl(".*.tar.gz-t$", path)) {
    cli::cli_abort("Unknown path type: {path}. Expected .tar.gz or .tar.gz-t file.")
  }

  if (!file.exists(path)) {
    cli::cli_warn("Path {path} does not exist, skipping...")
    return(NULL)
  }

  is_valid_tar_gz <- function(file) {
    if (grepl(".*.tar.gz-t$", file)) {
      return(TRUE)
    }
    res <- try(untar(file, list = TRUE), silent = TRUE)
    !inherits(res, "try-error")
  }

  if (grepl(".*.tar.gz$", path) && !grepl(".*.tar.gz-t$", path)) {
    if (!is_valid_tar_gz(path)) {
      cli::cli_abort("File {path} is not a valid tar archive.")
    }
    return(path)
  } else if (grepl(".*.tar.gz-t$", path)) {
    tgz_path <- tempfile(fileext = ".tar.gz")

    if (file.info(path)$isdir) {
      pkgbuild::build(file.path(path, pkg), dest_path = tgz_path, binary = FALSE, manual = FALSE, vignettes = FALSE)
      return(tgz_path)
    } else {
      untarred_dir <- tempfile()
      dir.create(untarred_dir)
      on.exit(unlink(untarred_dir, recursive = TRUE), add = TRUE)

      if (!is_valid_tar_gz(path)) {
        cli::cli_abort("File {path} is not a valid tar archive.")
      }
      untar(path, exdir = untarred_dir)
      sources_dirs <- list.dirs(untarred_dir, full.names = TRUE, recursive = FALSE)
      pkgbuild::build(sources_dirs[1], dest_path = tgz_path, binary = FALSE, manual = FALSE, vignettes = FALSE)
      return(tgz_path)
    }
  }
}
get_tar_gz_from_cache <- function(pkg, version) {
  i_cache <- pkgcache::pkg_cache_find(package = pkg, version = version, platform = "source") %df_empty%
    pkgcache::pkg_cache_find(package = pkg, version = version)

  if (nrow(i_cache) == 0) return(NULL)

  get_tar_gz(pkg, version, i_cache$fullpath[[1]])
}
add_to_minicran <- function(pkg, version, tar_gz_path, minicran_path) {
  cli::cli_inform(sprintf("Adding %s version %s to miniCRAN...", pkg, version))

  if (check_if_added(pkg, version, minicran_path)) {
    return(invisible(NULL))
  }

  tempdir <- tempfile()
  dir.create(tempdir)
  on.exit(unlink(tempdir, recursive = TRUE), add = TRUE)

  new_file_name <- sprintf("%s_%s.tar.gz", pkg, version)
  file.copy(tar_gz_path, file.path(tempdir, new_file_name))

  miniCRAN::addLocalPackage(pkg, tempdir, minicran_path)

  invisible(NULL)
}
build_and_add_to_minicran <- function(pkg, version, fulltarget, fulltarget_tree, minicran_path) {
  cli::cli_inform(sprintf("Building and adding %s version %s to miniCRAN...", pkg, version))

  tgz_path <- NULL

  if (file.exists(fulltarget)) {
    tgz_path <- fulltarget
  } else if (file.exists(fulltarget_tree)) {
    tgz_path <- get_tar_gz(pkg, version, fulltarget_tree)
  } else {
    tgz_path <- get_tar_gz_from_cache(pkg, version)
  }

  if (is.null(tgz_path)) {
    cli::cli_warn("Failed to get tar.gz file for {pkg} version {version}, skipping...")
    return(invisible(NULL))
  }

  add_to_minicran(pkg, version, tgz_path, minicran_path)

  invisible(NULL)
}
download_and_add_to_minicran <- function(ref, minicran_path) {
  cli::cli_inform(sprintf("Downloading and adding %s to miniCRAN...", ref))

  x <- pak::pkg_download(ref, dependencies = TRUE)

  for (i in seq_len(nrow(x))) {
    if (check_if_pkg_available(x[i, "package"], x[i, "version"])) {
      next
    }
    if (check_if_added(x[i, "package"], x[i, "version"], minicran_path)) {
      next
    }

    build_and_add_to_minicran(
      x[i, "package"],
      x[i, "version"],
      x[i, "fulltarget"],
      x[i, "fulltarget_tree"],
      minicran_path
    )
  }

  invisible(NULL)
}
install_and_add_to_minicran <- function(ref, minicran_path) {
  cli::cli_inform(sprintf("Installing and adding %s to miniCRAN...", ref))

  x <- pak::pkg_install(ref, ask = FALSE, dependencies = TRUE)

  for (i in seq_len(nrow(x))) {
    if (check_if_pkg_available(x[i, "package"], x[i, "version"])) {
      next
    }
    if (check_if_added(x[i, "package"], x[i, "version"], minicran_path)) {
      next
    }

    build_and_add_to_minicran(
      x[i, "package"],
      x[i, "version"],
      x[i, "fulltarget"],
      x[i, "fulltarget_tree"],
      minicran_path
    )
  }

  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
setwd(normalizePath(file.path(args[1])))
number_of_workers <- as.integer(args[2])
timeout <- as.integer(args[3])

# Install required packages
catnl("Installing required packages...")
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", quiet = TRUE)
}
if (!requireNamespace("pkgcache", quietly = TRUE)) {
  install.packages("pkgcache", quiet = TRUE)
}
pak::pkg_install(c(
  "cli",
  "miniCRAN",
  "pkgbuild",
  "pkgdepends",
  "r-lib/revdepcheck",
  "usethis",
  "yaml"
), ask = FALSE)
options(
  repos = c(
    PPM = pkgcache::repo_resolve("PPM@latest"),
    getOption("repos")
  )
)


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

## miniCRAN
cli::cli_progress_step("Initiating `miniCRAN`...")
minicran_path <- tempfile()
dir.create(minicran_path)
on.exit(unlink(minicran_path, recursive = TRUE), add = TRUE)
# added `rlang` as a dummy package as the `pkgs` arg cannot be empty
miniCRAN::makeRepo(pkgs = "rlang", path = minicran_path, type = c("source", .Platform$pkgType), quiet = TRUE)
# add minicran repo path to repos so that revdepcheck can use it
# this is the directory where we will store packages from config file
options("repos" = c(
  "minicran" = paste0("file:///", minicran_path),
  getOption("repos")
))
#Sys.setenv(CRANCACHE_REPOS = "minicran, cran, bioc")

## installing the package
### CRAN version
cli::cli_progress_step("Installing CRAN version of the package...")
pkg_name <- read.dcf("DESCRIPTION")[1, "Package"][[1]]
if (check_if_pkg_available(pkg_name)) {
  pkg_ref_released <- pkg_name
} else {
  # try to get the package reference from the DESCRIPTION file (URL field)
  pkg_url <- gsub("\n|/$", "", strsplit(read.dcf("DESCRIPTION")[1, "URL"], ",")[[1]])
  pkg_url_gh <- grep("github.com", pkg_url, value = TRUE)
  pkg_ref_released <- paste0(gsub(".*github.com/", "", pkg_url_gh), "@*release")
  if (length(pkg_ref_released) == 0) {
    cli::cli_abort("Unable to automatically determine the package reference for GitHub release.")
    return(NULL)
  }
}
cli::cli_inform(sprintf("Using package reference: %s", pkg_ref_released))
install_and_add_to_minicran(pkg_ref_released, minicran_path)
### DEV version
cli::cli_progress_step("Installing DEV version of the package...")
pkg_ref_dev <- "."
install_and_add_to_minicran(pkg_ref_dev, minicran_path)

## revdepcheck
cli::cli_progress_step("Initiating `revdepcheck`...")
revdepcheck::revdep_reset()
unlink("./revdep/", recursive = TRUE)
revdepcheck:::db_disconnect(".")
usethis::use_revdep()
revdepcheck:::db_setup(".")

cli::cli_progress_done()


cli::cli_h1("Add refs to revdepcheck")
# include refs in revdepcheck
## Add refs revdepcheck and also to miniCRAN so that it can be found from there
cli::cli_progress_bar("Adding refs to revdepcheck", total = length(refs))
for (ref in refs) {
  cli::cli_progress_message("Adding {ref}...")

  tryCatch({
    download_and_add_to_minicran(ref, minicran_path)

    ref_pkg <- pkgdepends::parse_pkg_ref(ref)$package
    revdepcheck::revdep_add(packages = ref_pkg)

    cli::cli_inform("Added {ref} to revdep todo!")
  }, error = function(e) {
    cli::cli_warn(sprintf("Failed to download and add %s to miniCRAN: %s", ref, e$message))
  })

  cli::cli_progress_update()
}
cli::cli_progress_done()
cli::cli_inform("All references added!")

cli::cli_inform("The current revdep todo (empty indicates the default - all revdeps):")
print(revdepcheck::revdep_todo())


cli::cli_h1("miniCRAN status")
miniCRAN::pkgAvail(repos = minicran_path)[, c("Package", "Version")]


# Execute
cli::cli_h1("Execute revdepcheck")
revdepcheck::revdep_check(num_workers = number_of_workers, timeout = timeout, quiet = FALSE)


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

if (!identical(readLines("revdep/problems.md", warn = FALSE), "*Wow, no problems at all. :)*")) {
  stop("There are errors. Please refer to the logs above.")
}
