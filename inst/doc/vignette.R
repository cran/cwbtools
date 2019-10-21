## ----create_tmp_dirs-----------------------------------------------------
tmpdir <- tempdir()
if (.Platform$OS.type == "windows") tmpdir <- normalizePath(tmpdir, winslash = "/")

registry_tmp <- file.path(tmpdir, "registry")
data_dir_tmp <- file.path(tmpdir, "data_dir")

if (!file.exists(registry_tmp)){
  dir.create (registry_tmp)
} else {
  file.remove(list.files(registry_tmp, full.names = TRUE))
}
if (!file.exists(data_dir_tmp)) dir.create(data_dir_tmp)

## ----load_pkgs-----------------------------------------------------------
library(cwbtools)
library(data.table)

## ----get_unga_teifiles---------------------------------------------------
teidir <- system.file(package = "cwbtools", "xml", "UNGA")
teifiles <- list.files(teidir, full.names = TRUE)
list.files(teidir)

## ----unga_create_dirs, message = FALSE,  results = FALSE-----------------
unga_data_dir_tmp <- file.path(data_dir_tmp, "unga")
if (!file.exists(unga_data_dir_tmp)) dir.create(unga_data_dir_tmp)
file.remove(list.files(unga_data_dir_tmp, full.names = TRUE))

## ----unga_instantiate_cd-------------------------------------------------
UNGA <- CorpusData$new()
UNGA

## ----basetable, eval = TRUE----------------------------------------------
metadata <- c(
  lp = "//legislativePeriod", session = "//titleStmt/sessionNo",
  date = "//publicationStmt/date", url = "//sourceDesc/url",
  src = "//sourceDesc/filetype"
)
UNGA$import_xml(filenames = teifiles, meta = metadata)
UNGA

## ----cleaning, eval = TRUE-----------------------------------------------
to_keep <- which(is.na(UNGA$metadata[["speaker"]]))
UNGA$chunktable <- UNGA$chunktable[to_keep]
UNGA$metadata <- UNGA$metadata[to_keep][, speaker := NULL]

## ----unga_expressive_colnames--------------------------------------------
setnames(UNGA$metadata, old = c("sp_who", "sp_state", "sp_role"), new = c("who", "state", "role"))

## ----dissect, eval = TRUE, message = FALSE, results = FALSE--------------
UNGA$tokenize(lowercase = FALSE, strip_punct = FALSE)
UNGA

## ------------------------------------------------------------------------
UNGA$tokenstream

## ---- message = FALSE----------------------------------------------------
s_attrs <- c("id", "who", "state", "role", "lp", "session", "date")
UNGA$encode(
  registry_dir = registry_tmp, data_dir = unga_data_dir_tmp,
  corpus = "UNGA", encoding = "utf8", method = "R",
  p_attributes = "word", s_attributes = character(),
  compress = FALSE
  )

## ----check_use, eval = TRUE----------------------------------------------
library(RcppCWB)
id_peace <- cl_str2id(
  corpus = "UNGA", p_attribute = "word",
  str = "peace", registry = registry_tmp
)
cpos_peace <- cl_id2cpos(
  corpus = "UNGA", p_attribute = "word",
  id = id_peace, registry = registry_tmp
)

tab <- data.frame(
  i = unlist(lapply(1:length(cpos_peace), function(x) rep(x, times = 11))),
  cpos = unlist(lapply(cpos_peace, function(x) (x - 5):(x + 5)))
  )
tab[["id"]] <- cl_cpos2id(
  corpus = "UNGA", p_attribute = "word",
  cpos = tab[["cpos"]], registry = registry_tmp
)
tab[["str"]] <- cl_id2str(
  corpus = "UNGA", p_attribute = "word",
  id = tab[["id"]], registry = registry_tmp
)

peace_context <- split(tab[["str"]], as.factor(tab[["i"]]))
peace_context <- unname(sapply(peace_context, function(x) paste(x, collapse = " ")))
head(peace_context)

## ---- message = FALSE, results = FALSE-----------------------------------
austen_data_dir_tmp <- file.path(data_dir_tmp, "austen")
if (!file.exists(austen_data_dir_tmp)) dir.create(austen_data_dir_tmp)
file.remove(list.files(austen_data_dir_tmp, full.names = TRUE))

## ------------------------------------------------------------------------
Austen <- CorpusData$new()

## ------------------------------------------------------------------------
books <- janeaustenr::austen_books()
tbl <- tidytext::unnest_tokens(books, word, text, to_lower = FALSE)
Austen$tokenstream <- as.data.table(tbl)

## ------------------------------------------------------------------------
Austen$tokenstream[, stem := SnowballC::wordStem(tbl[["word"]], language = "english")]

## ------------------------------------------------------------------------
Austen$tokenstream[, cpos := 0L:(nrow(tbl) - 1L)]

## ------------------------------------------------------------------------
cpos_max_min <- function(x) list(cpos_left = min(x[["cpos"]]), cpos_right = max(x[["cpos"]]))
Austen$metadata <- Austen$tokenstream[, cpos_max_min(.SD), by = book]
Austen$metadata[, book := as.character(book)]
setcolorder(Austen$metadata, c("cpos_left", "cpos_right", "book"))

## ------------------------------------------------------------------------
Austen$tokenstream[, book := NULL]
setcolorder(Austen$tokenstream, c("cpos", "word", "stem"))
Austen$tokenstream

## ---- message = FALSE----------------------------------------------------
Austen$encode(
   corpus = "AUSTEN", encoding = "utf8",
   p_attributes = "word", s_attributes = "book",
   registry_dir = registry_tmp, data_dir = austen_data_dir_tmp,
   method = "R", compress = FALSE
)

## ------------------------------------------------------------------------
cqp_reset_registry(registry = registry_tmp)

## ------------------------------------------------------------------------
corpus <- "AUSTEN"
token <- "pride"
p_attr <- "word"
id <- cl_str2id(corpus = corpus, p_attribute = p_attr, str = token, registry = registry_tmp)
cpos <- cl_id2cpos(corpus = corpus, p_attribute = p_attr, id = id, registry = registry_tmp)
count <- length(cpos)
count

## ---- message = FALSE, results = FALSE-----------------------------------
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters.tm <- VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))

## ------------------------------------------------------------------------
library(tidytext)
reuters.tbl <- tidy(reuters.tm)
reuters.tbl

## ------------------------------------------------------------------------
reuters.tbl[["topics_cat"]] <- sapply(
  reuters.tbl[["topics_cat"]],
  function(x) paste(x, collapse = "|")
  )
reuters.tbl[["places"]] <- sapply(
  reuters.tbl[["places"]],
  function(x) paste(x, collapse = "|")
  )

## ---- results = FALSE----------------------------------------------------
Reuters <- CorpusData$new()
reuters_data_dir_tmp <- file.path(data_dir_tmp, "reuters")
if (!file.exists(reuters_data_dir_tmp)) dir.create(reuters_data_dir_tmp)
file.remove(list.files(reuters_data_dir_tmp, full.names = TRUE))

## ------------------------------------------------------------------------
Reuters$chunktable <- data.table(reuters.tbl[, c("id", "text")])
Reuters$metadata <- data.table(reuters.tbl[,c("id", "topics_cat", "places")])
Reuters

## ---- message = FALSE, results = FALSE-----------------------------------
Reuters$tokenize()

## ------------------------------------------------------------------------
Reuters$tokenstream

## ---- message = FALSE----------------------------------------------------
Reuters$encode(
   corpus = "REUTERS", encoding = "utf8",
   p_attributes = "word", s_attributes = c("topics_cat", "places"),
   registry_dir = registry_tmp,
   data_dir = data_dir_tmp,
   method = "R", compress = FALSE
)

## ---- message = FALSE, results = FALSE-----------------------------------
cqp_reset_registry(registry = registry_tmp)

## ------------------------------------------------------------------------
id <- cl_str2id(corpus = "REUTERS", p_attribute = "word", str = "oil", registry = registry_tmp)
cpos <- cl_id2cpos(corpus = "REUTERS", p_attribute = "word", id = id, registry = registry_tmp)
count <- length(cpos)
count

## ------------------------------------------------------------------------
unlink(tmpdir, recursive = TRUE)

