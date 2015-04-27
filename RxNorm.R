# RRxNorm.R
# This script queries the RxNorm API to get the corresponding ids and names of strings representing Medications.
# Useful links:
# http://rxnav.nlm.nih.gov/RxNavViews.html#label:appendix
# http://rxnav.nlm.nih.gov/RxNormAPIs.html#
# http://mor.nlm.nih.gov/download/rxnav/RxClassIntro.html
#
# Copyright Antoine Lizee 04/2015 - antoine.lizee@gmail.com 


rm(list = ls())

# Parameters of the script -----------------------------------------------

getConceptInfo <- T #Do you want to query each rxcui that has been matched to expand its properties?
defaultTimeout <- 5 #Timeout for the API calls
test <- F #Just to print out some tests of the main requesting functions

# API parameters
baseURL <- "http://rxnav.nlm.nih.gov/"
basePath <- "REST"
extension <- ".json"


# Initializing packages  ------------------------------------------------

cat("## Initializing script...\n")

loadOrInstall <- function(pkg) {
  #Small helper to automatically install missing packages
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
  paste0(basePath, "/", 
         action, 
         ifelse(is.na(suffix), "", paste0("/", suffix)),
         ifelse(extend, extension, ""))
}

fuzzyMatches <- function(term, n = 3, keep_rxaui = F, set_timeout = defaultTimeout) {
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
  if (is.null(df)) return(NULL)
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

if (test) {
  print(fuzzyMatches("Hydrocodone-Acetaminophen")) # General String, several perfect matches => all the 100 are returned
  print(fuzzyMatches("Hydrocodone-Acetaminophen oiuy")) # General String + mistake => all the first matches are returned
  print(fuzzyMatches(" Avandamet 2-500 MG Oral Tablet")) # Specific String => The only 100 is returned
  print(fuzzyMatches(" Avandamet 2-500 MG Oral Tablet poiu", n = 3)) # Specific String with mistake => the top n are returned
}

rxcuiInfo <- function(rxcui, set_timeout = defaultTimeout) {
  #rxcuiInfo() queries the API to get further information about a particular concept.
  r <- GET(url = baseURL,
           path = buildPath("rxcui", paste0(rxcui, "/properties")),
           timeout(set_timeout))
  if (is.null(content(r))) return(NULL)
  info <- data.frame(fromJSON(content(r, "text"))$properties, stringsAsFactors = F)
  info[c("rxcui")] <- as.numeric(info[c("rxcui")])
  return(info)
}

if (test) {
  rxcuiInfo(214182)
}


# Read Example File ----------------------------------------------------------------

medTable <- read.delim("Data/medications.tsv", stringsAsFactors = F)
medStrings <- unique(medTable$medication)


# Match the names to rxcuis in batch ---------------------------------------------

cat("## Matching medication names...\n")
pb <- txtProgressBar(i <- 0, length(medStrings), style = 3, initial = NA)
allMatches <- do.call(rbind,
                      lapply(medStrings, function(med) {
                        matches <- fuzzyMatches(med)
                        Sys.sleep(0.1)
                        setTxtProgressBar(pb, i <<- i+1)
                        if (!is.null(matches)) data.frame(medString = med, matches) else NULL
                      }))
close(pb)


# Match the rxcui to their properties -------------------------------------

if (getConceptInfo) {
  cat("## Getting concept information...\n")
  rxcuis <- unique(allMatches$rxcui)
  pb <- txtProgressBar(i <- 0, length(rxcuis), style = 3, initial = NA)
  rxcuiProperties <- do.call(rbind,
                             lapply(rxcuis, function(rxcui) {
                               properties <- rxcuiInfo(rxcui)
                               Sys.sleep(0.1)
                               setTxtProgressBar(pb, i <<- i+1)
                               properties
                             }))
  close(pb)
  
  allMatches <- merge(allMatches, rxcuiProperties)
}


# Merge the data with the original med identifier to match ----------------

results <- merge(medTable, allMatches, by.x = "medication", by.y = "medString")


# Write output table ------------------------------------------------------

write.csv(results, "allMatchesWithProperties.csv")
save(results, medTable, file = "allMatchesWithProperties.RData")



