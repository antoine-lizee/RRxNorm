# RRxNorm.R
# This script queries the RxNorm API to get the corresponding ids and names of strings representing Medications.
#
# Copyright Antoine Lizee 04/2015 - antoine.lizee@gmail.com 


# Parameters of the script -----------------------------------------------

getConceptInfo <- T
timeout <- 5

# API parameters
baseURL <- "http://rxnav.nlm.nih.gov/"
basePath <- "REST"
extension <- ".json"

# Initializing packages  ------------------------------------------------

cat("## Initializing script...")

loadOrInstall <- function(pkg) {
  stopifnot(class(pkg) == "character")
  if (!require(pkg, character.only = T)) {
    cat("## Installing the following necessary library:", pkg, "(this should happen only once per machine)\n")
    install.packages(pkg, dependencies = T, repos = "http://cran.rstudio.com/")
    library(pkg, character.only = T)
  }
}

loadOrInstall("httr")
loadOrInstall("jsonlite")
loadOrInstall("plyr")


# Main functions ----------------------------------------------------------

buildPath <- function(action, suffix = NA, extend = TRUE) {
  #Builds the path used to query the RxNorm API
  paste0(basePath, "/", action, ifelse(extend, extension, ""), ifelse(is.na(suffix), "", )) 
}

fuzzyMatches <- function(term, n = 3, keep_rxaui = F, set_timeout = 5) {
  #fuzzyMatches() queries the RxNorm API to match a medication string to several
  #rxcuis. It returns all the matches that have a score of 100 if any,
  #otherwise, it returns the most of (i) the top n (ii) the tied best matches.
  #If 'keep_rxaui' is set to FALSE (default), the atomic information is discarded,
  #keeping the metrics of the best match, and the results significantly more compact.
  r <- GET(url = baseURL,
           path = buildPath("approximateTerm"),
           query = list(term = term),
           timeout(set_timeout))
  df <- fromJSON(content(r, "text"))$approximateGroup$candidate
  df <- data.frame(lapply(df, as.numeric))
  if (!keep_rxaui) {
    df <- ddply(df, ~rxcui, summarize,
                score = max(score),
                rank = min(rank),
                nAtoms = length(rxaui) )
  }
  df <- df[order(df$rank), ]
  if (any(is100 <- df$score == 100)) {
    return(df[is100, ])
  } else {
    return(df[1:min(max(sum(df$rank == 1), n), nrow(df)),])
  }
}

if (test <- FALSE) {
  fuzzyMatches("Hydrocodone-Acetaminophen") # General String, several perfect matches => all the 100 are returned
  fuzzyMatches("Hydrocodone-Acetaminophen oiuy", n = 3) # General String + mistake => all the first matches are returned
  fuzzyMatches(" Avandamet 2-500 MG Oral Tablet") # Specific String => The only 100 is returned
  fuzzyMatches(" Avandamet 2-500 MG Oral Tablet poiu", n = 3) # Specific String with mistake => the top n are returned
}

rcxuiInfo <- function(rxcui) {
  #rxcuiInfo() queries the API to get further information about a particular concept.
  r <- GET(url = baseURL,
           path = buildPath("rxcui", paste0(rxcui, "/properties")))
  unlist(fromJSON(content(r, "text"))$properties)
}


# Read Example File ----------------------------------------------------------------

medTable <- read.delim("Data/medications.tsv", stringsAsFactors = F)
medStrings <- medTable$medication[1:5]


# Match the names to rxcuis in batch ---------------------------------------------

cat("## Matching medication names...\n")
pb <- txtProgressBar(i <- 0, length(meds), style = 3, initial = NA)
allMatches <- do.call(rbind,
                      lapply(medStrings, function(med) {
                        matches <- fuzzyMatches(med)
                        setTxtProgressBar(pb, i <<- i+1)
                        data.frame(medString = med, matches)
                      }))
close(pb)



# Match the rxcui to their properties -------------------------------------



r <- GET(buildUrl(""),
         path = paste("REST/rxcui", 214182, "properties", sep = "/"))
cbind(unlist(fromJSON(content(r, "text"))$properties))

