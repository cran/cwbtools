## ----create_tmp_dirs----------------------------------------------------------
registry_tmp <- fs::path(tempdir(), "registry")
data_dir_tmp <- fs::path(tempdir(), "data_dir")

if (!file.exists(registry_tmp)){
  dir.create (registry_tmp)
} else {
  file.remove(list.files(registry_tmp, full.names = TRUE))
}
if (!file.exists(data_dir_tmp)) dir.create(data_dir_tmp)

## ----load_pkgs----------------------------------------------------------------
library(cwbtools)
library(data.table)

## ----message = FALSE, results = FALSE-----------------------------------------
austen_data_dir_tmp <- fs::path(data_dir_tmp, "austen")

if (!file.exists(austen_data_dir_tmp)) dir.create(austen_data_dir_tmp)
file.remove(list.files(austen_data_dir_tmp, full.names = TRUE))

## -----------------------------------------------------------------------------
Austen <- CorpusData$new()

## -----------------------------------------------------------------------------
tbl <- tidytext::unnest_tokens(
  janeaustenr::austen_books(),
  word, text, to_lower = FALSE
)

Austen$tokenstream <- as.data.table(tbl)

## -----------------------------------------------------------------------------
Austen$tokenstream[, stem := SnowballC::wordStem(Austen$tokenstream[["word"]], language = "english")]

## -----------------------------------------------------------------------------
Austen$tokenstream[, cpos := 0L:(nrow(tbl) - 1L)]

## -----------------------------------------------------------------------------
cpos_max_min <- function(x)
  list(cpos_left = min(x[["cpos"]]), cpos_right = max(x[["cpos"]]))
Austen$metadata <- Austen$tokenstream[, cpos_max_min(.SD), by = book]
Austen$metadata[, book := as.character(book)]
setcolorder(Austen$metadata, c("cpos_left", "cpos_right", "book"))

head(Austen$tokenstream)

## -----------------------------------------------------------------------------
Austen$tokenstream[, book := NULL]
setcolorder(Austen$tokenstream, c("cpos", "word", "stem"))
Austen$tokenstream

## ----message = FALSE----------------------------------------------------------
Austen$encode(
   corpus = "AUSTEN",
   encoding = "utf8",
   p_attributes = c("word", "stem"),
   s_attributes = "book",
   registry_dir = registry_tmp,
   data_dir = austen_data_dir_tmp,
   method = "R",
   compress = FALSE,
   reload = TRUE
)

## -----------------------------------------------------------------------------
ids <- RcppCWB::cl_str2id(
  corpus = "AUSTEN",
  p_attribute = "word",
  str = "pride",
  registry = registry_tmp
)

cpos <- RcppCWB::cl_id2cpos(
  corpus = "AUSTEN",
  p_attribute = "word",
  id = ids,
  registry = registry_tmp
)

length(cpos)

## ----get_unga_teifiles--------------------------------------------------------
teidir <- system.file(package = "cwbtools", "xml", "UNGA")
teifiles <- list.files(teidir, full.names = TRUE)
list.files(teidir)

## ----unga_create_dirs, message = FALSE,  results = FALSE----------------------
unga_data_dir_tmp <- fs::path(data_dir_tmp, "unga")
if (!file.exists(unga_data_dir_tmp)) dir.create(unga_data_dir_tmp)
file.remove(list.files(unga_data_dir_tmp, full.names = TRUE))

## ----unga_instantiate_cd------------------------------------------------------
UNGA <- CorpusData$new()
UNGA

## ----basetable, eval = TRUE---------------------------------------------------
metadata <- c(
  doc_lp = "//legislativePeriod",
  doc_session = "//titleStmt/sessionNo",
  doc_date = "//publicationStmt/date",
  doc_url = "//sourceDesc/url",
  doc_src = "//sourceDesc/filetype"
)
UNGA$import_xml(filenames = teifiles, meta = metadata)
UNGA

## ----cleaning, eval = TRUE----------------------------------------------------
to_keep <- which(is.na(UNGA$metadata[["speaker"]]))
UNGA$chunktable <- UNGA$chunktable[to_keep]
UNGA$metadata <- UNGA$metadata[to_keep][, speaker := NULL]

## ----dissect, eval = TRUE, message = FALSE, results = FALSE-------------------
UNGA$tokenize(lowercase = FALSE, strip_punct = FALSE)
UNGA

## -----------------------------------------------------------------------------
UNGA$tokenstream

## ----message = FALSE----------------------------------------------------------
UNGA$encode(
  registry_dir = registry_tmp,
  data_dir = unga_data_dir_tmp,
  corpus = "UNGA",
  encoding = "utf8",
  method = "R",
  p_attributes = "word",
  s_attributes = c(
    "doc_lp", "doc_session", "doc_date",
    "sp_who", "sp_state", "sp_role"
  ),
  compress = FALSE
)

## -----------------------------------------------------------------------------
id_peace <- RcppCWB::cl_str2id(
  corpus = "UNGA",
  p_attribute = "word",
  str = "peace",
  registry = registry_tmp
)
cpos_peace <- RcppCWB::cl_id2cpos(
  corpus = "UNGA",
  p_attribute = "word",
  id = id_peace,
  registry = registry_tmp
)

tab <- data.frame(
  i = unlist(lapply(1:length(cpos_peace), function(x) rep(x, times = 11))),
  cpos = unlist(lapply(cpos_peace, function(x) (x - 5):(x + 5)))
  )
tab[["id"]] <- RcppCWB::cl_cpos2id(
  corpus = "UNGA", p_attribute = "word",
  cpos = tab[["cpos"]], registry = registry_tmp
)
tab[["str"]] <- RcppCWB::cl_id2str(
  corpus = "UNGA", p_attribute = "word",
  id = tab[["id"]], registry = registry_tmp
)

peace_context <- split(tab[["str"]], as.factor(tab[["i"]]))
peace_context <- unname(sapply(peace_context, function(x) paste(x, collapse = " ")))
head(peace_context)

## ----message = FALSE, results = FALSE-----------------------------------------
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters.tm <- VCorpus(
  DirSource(reut21578),
  list(reader = readReut21578XMLasPlain)
)

## -----------------------------------------------------------------------------
library(tidytext)
reuters.tbl <- tidy(reuters.tm)
reuters.tbl

## -----------------------------------------------------------------------------
topics <- sapply(reuters.tbl[["topics_cat"]], paste, collapse = "|")
places <- sapply(reuters.tbl[["places"]], paste, collapse = "|")

reuters.tbl[["topics"]] <- topics
reuters.tbl[["places"]] <- places

## ----results = FALSE----------------------------------------------------------
Reuters <- CorpusData$new()
reuters_data_dir_tmp <- fs::path(data_dir_tmp, "reuters")
if (!file.exists(reuters_data_dir_tmp)) dir.create(reuters_data_dir_tmp)
file.remove(list.files(reuters_data_dir_tmp, full.names = TRUE))

## -----------------------------------------------------------------------------
Reuters$chunktable <- data.table(reuters.tbl[, c("id", "text")])
Reuters$metadata <- data.table(reuters.tbl[,c("id", "topics", "places")])
Reuters

## ----message = FALSE, results = FALSE-----------------------------------------
Reuters$tokenize()

## -----------------------------------------------------------------------------
Reuters$tokenstream

## ----message = FALSE----------------------------------------------------------
Reuters$encode(
   corpus = "REUTERS",
   encoding = "utf8",
   p_attributes = "word",
   s_attributes = c("topics", "places"),
   registry_dir = registry_tmp,
   data_dir = reuters_data_dir_tmp,
   method = "R",
   compress = FALSE
)

## -----------------------------------------------------------------------------
ids <- RcppCWB::cl_str2id(
  corpus = "REUTERS",
  p_attribute = "word",
  str = "oil",
  registry = registry_tmp
)

cpos <- RcppCWB::cl_id2cpos(
  corpus = "REUTERS",
  p_attribute = "word",
  id = ids,
  registry = registry_tmp
)

length(cpos)

## -----------------------------------------------------------------------------
reuters_size <- RcppCWB::attribute_size(
  corpus = "REUTERS",
  registry = registry_tmp,
  attribute = "word",
  attribute_type = "p"
)

ids <- RcppCWB::cl_cpos2id(
  corpus = "REUTERS",
  registry = registry_tmp,
  p_attribute = "word",
  cpos = 0L:(reuters_size - 1L)
)

token_stream <- RcppCWB::cl_id2str(
  corpus = "REUTERS",
  registry = registry_tmp,
  p_attribute = "word",
  id = ids
)

## -----------------------------------------------------------------------------
stemmed <- SnowballC::wordStem(token_stream, language = "en")

## -----------------------------------------------------------------------------
p_attribute_encode(
  token_stream = stemmed,
  p_attribute = "stem",
  encoding = "utf8",
  corpus = "REUTERS",
  registry_dir = registry_tmp,
  data_dir = reuters_data_dir_tmp,
  method = "R",
  verbose = TRUE,
  quietly = TRUE,
  compress = FALSE
)

## -----------------------------------------------------------------------------
unlink(registry_tmp, recursive = TRUE)
unlink(data_dir_tmp, recursive = TRUE)

