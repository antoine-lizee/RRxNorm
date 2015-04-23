

# Initializing packages  ------------------------------------------------

loadOrInstall <- function(pkg) {
  stopifnot(class(pkg) == "character")
  if (!require(pkg)) {
    cat("## Installing the following necessary library:", pkg, "(this should happen only once per machine)\n")
    install.packages(pkg, dependencies = T, repos = "http://cran.rstudio.com/")
    library(pkg)
  }
}

loadOrInstall("httr")
loadOrInstall("jsonlite")


# Parameters of the script -----------------------------------------------

baseURL <- "http://rxnav.nlm.nih.gov/"
basePath <- "REST"
API <- handle(baseURL)
extension <- ".json"

buildPath <- function(action, suffix = NA, extend = TRUE) {
  paste0(basePath, "/", action, ifelse(extend, extension, ""), ifelse(is.na(suffix), "", )) 
}

fuzzyMatches <- function(term) {
  r <- GET(handle = API,
           path = buildPath("approximateTerm"),
           query = list(term = term))
  df <- fromJSON(content(r, "text"))$approximateGroup$candidate
  if (any(is100 <- df$score == 100)) {
    return(df[is100, ])
  } else {
    return(df[1:min(max(sum(df$rank == 1), 3), nrow(df)),])
  }
}

rcxuiInfo <- function(rxcui) {
  r <- GET(handle = API,
           path = buildPath("rxcui", paste(214182, "properties", sep = "/"))
           unlist(fromJSON(content(r, "text"))$properties)
}

r <- GET(buildUrl("approximateTerm"),
         query = list(term = "Hydrocodone-Acetaminophen"))

fromJSON(content(r, "text"))

r <- GET(buildUrl(""),
         path = paste("REST/rxcui", 214182, "properties", sep = "/"))
cbind(unlist(fromJSON(content(r, "text"))$properties))

