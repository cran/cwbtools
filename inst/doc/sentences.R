## ---- eval = TRUE-------------------------------------------------------------
library(cwbtools)
library(RcppCWB)

## ---- eval = TRUE-------------------------------------------------------------
registry_dir_tmp <- fs::path(tempdir(), "registry_dir_tmp")
corpus_dir_tmp <- fs::path(tempdir(), "corpus_dir_tmp")

dir.create(path = registry_dir_tmp)
dir.create(path = corpus_dir_tmp)

regdir_envvar <- Sys.getenv("CORPUS_REGISTRY")
Sys.setenv(CORPUS_REGISTRY = registry_dir_tmp)

## ---- eval = TRUE-------------------------------------------------------------
is_corpus_available <- cwbtools::corpus_install(
  doi = "10.5281/zenodo.3823245",
  registry_dir = registry_dir_tmp, corpus_dir = corpus_dir_tmp,
  verbose = FALSE
)

## -----------------------------------------------------------------------------
list.files(file.path(corpus_dir_tmp, "germaparlsample"))

## ---- eval = is_corpus_available----------------------------------------------
germaparl_size <- cl_attribute_size(
  corpus = "GERMAPARLSAMPLE",
  attribute = "word", attribute_type = "p"
)
cpos_vec <- seq.int(from = 0L, to = germaparl_size - 1L)
ids <- cl_cpos2id(corpus = "GERMAPARLSAMPLE", p_attribute = "pos", cpos = cpos_vec)
pos <- cl_id2str(corpus = "GERMAPARLSAMPLE", p_attribute = "pos", id = ids)

## ---- eval = is_corpus_available----------------------------------------------
sentence_end <- grep("\\$\\.", pos)
sentence_factor <- cut(x = cpos_vec, breaks = c(0L, sentence_end), include.lowest = TRUE, right = FALSE)
sentences_cpos <- unname(split(x = cpos_vec, f = sentence_factor))
region_matrix <- do.call(rbind, lapply(sentences_cpos, function(cpos) c(cpos[1L], cpos[length(cpos)])))

## ---- eval = is_corpus_available----------------------------------------------
head(region_matrix)

## ---- eval = is_corpus_available----------------------------------------------
s_attribute_encode(
  values = as.character(seq.int(from = 0L, to = nrow(region_matrix) - 1L)),
  data_dir = registry_file_parse(corpus = "GERMAPARLSAMPLE")[["home"]],
  s_attribute = "s",
  corpus = "GERMAPARLSAMPLE",
  region_matrix = region_matrix,
  method = "R",
  registry_dir = Sys.getenv("CORPUS_REGISTRY"),
  encoding = registry_file_parse(corpus = "GERMAPARLSAMPLE")[["properties"]][["charset"]],
  delete = TRUE,
  verbose = TRUE
)

## ---- eval = is_corpus_available----------------------------------------------
left <- cl_cpos2lbound("GERMAPARLSAMPLE", cpos = 60, s_attribute = "s")
right <- cl_cpos2rbound("GERMAPARLSAMPLE", cpos = 60, s_attribute = "s")
ids <- cl_cpos2id("GERMAPARLSAMPLE", cpos = left:right, p_attribute = "word")
cl_id2str("GERMAPARLSAMPLE", p_attribute = "word", id = ids)

## ---- eval = is_corpus_available----------------------------------------------
unlink(corpus_dir_tmp)
unlink(registry_dir_tmp)

Sys.setenv(CORPUS_REGISTRY = regdir_envvar)

