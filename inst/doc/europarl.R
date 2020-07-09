## ----load_cwbtools, eval = TRUE-----------------------------------------------
library(cwbtools)
library(devtools)

## ----create_tmp_dir, eval = TRUE----------------------------------------------
tmp_dir <- tempdir()

## ----execute, eval = TRUE-----------------------------------------------------
exec <- FALSE

## ----download_europarl_tarball, eval = exec-----------------------------------
#  europarl_url <- "http://corpora.linguistik.uni-erlangen.de/demos/download/Europarl3-CWB-2010-02-28.tar.gz"
#  europarl_tarball <- file.path(tmp_dir, basename(europarl_url))
#  download.file(url = europarl_url, destfile = europarl_tarball)

## ----untar_europarl_tarball, eval = exec--------------------------------------
#  untar(tarfile = europarl_tarball, exdir = tmp_dir)
#  unlink(europarl_tarball)

## ----get_corpora, eval = exec-------------------------------------------------
#  europarl_registry_dir <- file.path(tmp_dir, "Europarl3-CWB", "registry")
#  europarl_data_dir <- file.path(tmp_dir, "Europarl3-CWB", "data")
#  corpora <- list.files(europarl_registry_dir)
#  corpora

## ----adjust_home_dirs, eval = exec--------------------------------------------
#  for (corpus in corpora){
#    registry <- registry_file_parse(corpus = corpus, registry_dir = europarl_registry_dir)
#    registry[["home"]] <- file.path(europarl_data_dir, gsub("^europarl-(.*)$", "\\1", corpus))
#    registry_file_write(data = registry, corpus = corpus, registry_dir = europarl_registry_dir)
#  }

## ----create_package_skeleton, eval = exec-------------------------------------
#  europarl_pkg_dir <- file.path(tmp_dir, "europarl")
#  if (!file.exists(europarl_pkg_dir)) dir.create(europarl_pkg_dir)
#  pkg_create_cwb_dirs(pkg = europarl_pkg_dir)

## ----create_description, eval = exec------------------------------------------
#  europarl_desc <- paste0(
#    readLines(file.path(tmp_dir, "Europarl3-CWB", "readme.txt")),
#    collapse = " "
#    )
#  europarl_desc

## ----package_version, eval = exec---------------------------------------------
#  pkg_version <- "0.0.2"

## ----add_description, eval = exec---------------------------------------------
#  pkg_add_description(
#    pkg = europarl_pkg_dir, package = "europarl", version = pkg_version,
#    date = Sys.Date(),
#    author = "cwbtools",
#    maintainer = "Andreas Blaette <andreas.blaette@uni-due.de>",
#    description = europarl_desc
#    )
#  pkg_add_configure_scripts(pkg = europarl_pkg_dir)

## ----move_corpora, eval = exec------------------------------------------------
#  for (corpus in corpora){
#    pkg_add_corpus(pkg = europarl_pkg_dir, corpus = corpus, registry = europarl_registry_dir)
#  }

## ----remove_stuff, eval = exec------------------------------------------------
#  unlink(file.path(tmp_dir, "Europarl3-CWB"), recursive = TRUE)

## ----build_package, eval = exec-----------------------------------------------
#  europarl_tarball <- build(pkg = europarl_pkg_dir, path = tmp_dir, vignettes = TRUE)

## ----install_package, eval = exec---------------------------------------------
#  install.packages(pkgs = europarl_tarball, repos = NULL)

## ----list_corpora, eval = exec------------------------------------------------
#  library(RcppCWB)
#  europarl_pkg_registry <- system.file(package = "europarl", "extdata", "cwb", "registry")
#  Sys.setenv(CORPUS_REGISTRY = europarl_pkg_registry)
#  cqp_initialize()
#  cqp_list_corpora()

## ----query_europe, eval = exec------------------------------------------------
#  query <- "Europe"
#  id <- cl_str2id(
#    corpus = "europarl-en", registry = europarl_pkg_registry,
#    str = query, p_attribute = "word"
#    )
#  cpos <- cl_id2cpos(
#    corpus = "europarl-en", registry = europarl_pkg_registry,
#    id = id, p_attribute = "word"
#  )
#  tab <- data.frame(
#    i = unlist(lapply(1:length(cpos), function(x) rep(x, times = 11))),
#    cpos = unlist(lapply(cpos, function(x) (x - 5):(x + 5)))
#    )
#  tab[["id"]] <- cl_cpos2id(
#    corpus = "europarl-en", registry = europarl_pkg_registry,
#    cpos = tab[["cpos"]], p_attribute = "word"
#    )
#  tab[["str"]] <- cl_id2str(
#    corpus = "europarl-en", registry = europarl_pkg_registry,
#    p_attribute = "word", id = tab[["id"]]
#    )
#  concordances_list <- split(tab[["str"]], as.factor(tab[["i"]]))
#  concordances <- unname(sapply(concordances_list, function(x) paste(x, collapse = " ")))
#  head(concordances)

## ----unlink_things, eval = exec-----------------------------------------------
#  unlink(tmp_dir, recursive = TRUE)
#  unlink(file.path(tmp_dir, "europarl"))

