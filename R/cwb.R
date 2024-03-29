#' Utilities to install the Corpus Workbench (CWB)
#' 
#' The CWB comprises a set of command line tools for corpus preparation and
#' management. Functionality for installing and managing these Tools.
#' 
#' @details Use `cwb_install()` to download and install CWB binaries (v3.5) from
#'   [SourceForge](https://cwb.sourceforge.io/). If successful, `cwb_install()`
#'   returns the directory of the CWB, otherwiese `NULL`. For the installation
#'   on macOS and Linux, see \url{https://cwb.sourceforge.io/install.php}.
#' @param url_cwb URL for downloading the CWB.
#' @param cwb_dir The directory where the CWB shall be installed.
#' @param md5 The md5 checksum of the compressed file to be downloaded. 
#' @param verbose A `logical` value, whether to output messages.
#' @rdname cwb
#' @export cwb_install
#' @importFrom utils unzip untar download.file
#' @importFrom httr http_error
#' @importFrom fs path_temp path_tidy path
#' @importFrom tools md5sum
cwb_install <- function(
    url_cwb = cwb_get_url(),
    md5 = attr(url_cwb, "md5"),
    cwb_dir = fs::path(fs::path_temp(), "cwb"),
    verbose = TRUE
  ){
  
  if (!dir.exists(cwb_dir)){
    success <- dir.create(cwb_dir)
    if (isFALSE(success)){
      cli_alert_danger(
        "Directory {.path {cwb_dir}} does not exist - cannot create it"
      )
      return(NULL)
    }
  } else {
    if (length(list.files(cwb_dir)))
      cli_alert_danger("Directory {.path {cwb_dir}} is not empty")
  }
  
  subdir <- gsub(
    "^(.*?)(-UPDATED|)(\\.tar\\.gz|\\.zip)$", "\\1",
    basename(url_cwb)
  )

  # Download CWB, return NULL in case of failure
  if (isTRUE(http_error(url_cwb))){
    cli_alert_danger("HTTP error / cannot access URL {.path {url_cwb}}")
    return(NULL)
  }
  tryCatch(
    success <- curl::curl_download(
      url_cwb,
      destfile = path(path_temp(), basename(url_cwb)),
      quiet = !verbose
    ),
    error = function(e){
      cli_alert_danger(
        "Downloading CWB from URL {.path {url_cwb}} failed - returning NULL"
      )
      return(NULL)
    }
  )
  if (is.null(success) | !file.exists(success)){
    cli_alert_danger(
      "Downloading CWB from URL {.path {url_cwb}} failed - returning NULL"
    )
    return(NULL)
  }
  
  if (is.character(md5)){
    stopifnot(nchar(md5) > 0L)
    if (verbose){
      cli_process_start(
        "checking whether md5 sum of downloaded file is {.val {md5}}"
      )
    }
    if (md5sum(path(path_temp(), basename(url_cwb))) != md5){
      cli_process_failed()
      return(NULL)
    } else {
      cli_process_done()
    }
  }

  if (.Platform$OS.type == "windows"){
    unzip(fs::path(path_temp(), basename(url_cwb)), exdir = path_temp())
    bin_dir <- fs::path(cwb_dir, "bin")
    if (!dir.exists(bin_dir)) dir.create(bin_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "bin"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(bin_dir, basename(x)))
    }
  } else if (Sys.info()["sysname"] == "Darwin"){
    untar(fs::path(path_temp(), basename(url_cwb)), exdir = path_temp())
    bin_dir <- fs::path(cwb_dir, "bin")
    if (!dir.exists(bin_dir)) dir.create(bin_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "bin"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(bin_dir, basename(x)))
    }
    lib_dir <- fs::path(cwb_dir, "lib")
    if (!dir.exists(lib_dir)) dir.create(lib_dir)
    for (x in list.files(fs::path(path_temp(), subdir, "lib"), full.names = TRUE)){
      file.copy(from = x, to = fs::path(lib_dir, basename(x)))
    }
  } else if (Sys.info()["sysname"] == "Linux"){
    # "aarch64" and "arm64" are the same thing (= Apple Silicon)
    if (Sys.info()["machine"] %in% c("arm64", "aarch64")){
      cli_alert_warning("package architecture will not match system (arm64)")
      return(NULL)
    }
    
    debfile <- path(path_temp(), basename(url_cwb))
    # This requires sudo privileges and is likely to fail unless the R session
    # has been started using sudo rights 
    system2(command = "dpkg", args = sprintf("-i %s", debfile))
    success <- try(system2(command = "dpkg", args = sprintf("-i %s", debfile)))
    if (class(success)[1] == "try-error"){
      cli_alert_warning(paste0(c(
        "CWB installation aborted. Are sudo privileges missing? ",
        "Consider installation from command line, see: ",
        "{.href https://cwb.sourceforge.io/install.php}"
      )))
      return(NULL)
    }
  }
  
  unlink(path(path_temp(), basename(url_cwb)))
  unlink(path(path_temp(), subdir))
  cwb_bindir <- fs::path(cwb_dir, "bin")
  Sys.setenv("CWB_BINDIR" = cwb_bindir)
  cwb_bindir
}

#' @details `cwb_get_url()` will return the URL for downloading the appropriate
#'   binary (Linux / macOS) of the CWB (v3.5), or the source tarball
#'   (Linux). The md5 checksum of the file to be downloaded is part of the
#'   return value as "md5" attribute.
#' @rdname cwb
#' @export cwb_get_url
cwb_get_url <- function(){
  if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin"){
      if (Sys.info()["machine"] == "arm64"){
        url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/darwin/cwb-3.5.0-macos-11.0-arm64.tar.gz"
        attr(url_cwb, "md5") <- "fcf0516e02624cf991a3f77d4cbefcab"
      } else {
        url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/darwin/cwb-3.5.0-macos-10.13-x86_64.tar.gz"
        attr(url_cwb, "md5") <- "29ab6a93ffe9e740e73411f654c19957"
      }
    } else if (Sys.info()["sysname"] == "Linux"){
      if (TRUE){
        url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/deb/cwb_3.5.0-1_amd64.deb"
        attr(url_cwb, "md5") <- "85fb41efd2ad11c566e8d0c5f018634d"
      }
    } else {
      stop("Platform is 'unix', but Sys.info()['sysname'] is neither 'Darwin' (i.e. MacOS) nor 'Linux'")
    }
  } else if (.Platform$OS.type == "windows"){
    url_cwb <- "https://sourceforge.net/projects/cwb/files/cwb/cwb-3.5/windows/cwb-3.5.0-win64-x86_64.zip"
    attr(url_cwb, "md5") <- "3295703d562eec7fe5ad45c0d36a93b7"
  }
  url_cwb
}

#' @param bindir The directory with CWB binaries.
#' @param verbose Logical, whether to show progress messages.
#' @details `cwb_get_bindir()` detects the directory with the cwb command line
#'   programs. Defaults to using the value of the environment
#'   variable "CWB_BINDIR". If unset, the value of `cwb-config --bindir` is
#'   used. Returns `NULL` if CWB installation is not found.
#' @export cwb_get_bindir
#' @rdname cwb
cwb_get_bindir <- function(bindir = Sys.getenv("CWB_BINDIR"), verbose = TRUE){
  if (file.exists(bindir)){
    if (verbose) cli_alert_info("directory with CWB binaries: {.path {bindir}}")
    return(bindir)
  } else {
    # we try the `cwb-config` command line tool and return the path 
    # it returns if successful
    
    bindir <- try(
      {suppressWarnings(
          system2(command = "cwb-config", args = "--bindir", stderr = TRUE)
      )},
      silent = TRUE
    )
    if (class(bindir)[1] != "try-error"){
      if (verbose) cli_alert_info(
        "`cwb-config` reports directory with CWB binaries: {.path {bindir}}"
      )
      return(bindir)
    }
    
    # if the `cwb_config` tool is not on the PATH, it might be here:
    cwb_config <- "/usr/local/bin/cwb-config"
    if (file.exists(cwb_config)){
      bindir <- system(paste(cwb_config, "--bindir", sep = " "), intern = TRUE)
      if (verbose) cli_alert_info(paste0(c(
        "`cwb-config` (in: {.path /usr/local/bin/}) reports directory with ",
        "CWB binaries: {.path {bindir}}"
      )))
      return(bindir)
    } else {
      return(NULL)
    }
  }
  return(NULL)
}

#' @details `cwb_is_installed()` will check whether the CWB is installed.
#' @export cwb_is_installed
#' @rdname cwb
cwb_is_installed <- function(){
  if (is.null(cwb_get_bindir())) FALSE else TRUE
}
