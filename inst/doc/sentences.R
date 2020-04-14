## ---- eval = FALSE-------------------------------------------------------
#  library(cwbtools)
#  library(polmineR)
#  use("polmineR")

## ---- eval = FALSE-------------------------------------------------------
#  # Yet to be written

## ---- eval = FALSE-------------------------------------------------------
#  pos <- corpus("GERMAPARL") %>%
#    get_token_stream(p_attribute = "pos")
#  
#  sentence_end <- grep("\\$\\.", pos)
#  
#  cpos_sentences <- cut(
#    x = seq.int(from = 0L, to = length(pos) - 1L),
#    breaks = c(0L, sentence_end),
#    include.lowest = TRUE,
#    right = FALSE
#  )
#  
#  df <- split(x = cpos, f = cpos_sentences) %>%
#    lapply(function(cpos) c(cpos[1L], cpos[length(cpos)])) %>%
#    unlist() %>%
#    matrix(ncol = 2L, byrow = TRUE) %>%
#    data.frame()
#  
#  colnames(df) <- c("cpos_left", "cpos_right")
#  df[["sentence"]] <- seq.int(from = 0L, to = nrow(df) - 1L)

## ---- eval = FALSE-------------------------------------------------------
#  head(df)

## ---- eval = FALSE-------------------------------------------------------
#  s_attribute_encode(
#    values = as.character(df[["sentence"]]),
#    data_dir = cwbtools::registry_file_parse(corpus = "GERMAPARL")[["home"]],
#    s_attribute = "s",
#    corpus = "GERMAPARL",
#    region_matrix = as.matrix(df[,c("cpos_left", "cpos_right")]),
#    method = "R",
#    registry_dir = registry(pkg = "GERMAPARL"),
#    encoding = cwbtools::registry_file_parse(corpus = "GERMAPARL")[["properties"]][["charset"]],
#    delete = TRUE,
#    verbose = TRUE
#  )

## ---- eval = FALSE-------------------------------------------------------
#  k <- kwic("GERMAPARL", query = "Integration", left = 30, right = 30, boundary = "s")

