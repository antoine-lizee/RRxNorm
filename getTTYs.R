# getTTYs
# This script is pulling from the relevant page of the MetaThesaurus of UMLS the abbreviations for the term types (TTY) of Sources.
# To be noted : the url has abbreviations beyonf the TTYs.
#
# Copyright Antoine Lizee 04/2015 antoine.lizee@gmail.com


# Initialization ----------------------------------------------------------

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


# Retrieve information ----------------------------------------------------

url <- "http://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html#mrdoc_TTY"
# get the parsed content from the url
abbrHtmlTables <- httr::content(httr::GET(url)) 
# extract the tables as data frames:
abbrTables <- XML::readHTMLTable(abbrHtmlTables, stringsAsFactors = F)
# Select the one mentioning the TTY .. Sources as its title:
TTYAbbreviations <- abbrTables[[which(sapply(abbrTables, function(table) any(grepl(pattern = "TTY.*Source", table[1,]))))]]

TTYAbbreviations <- TTYAbbreviations[-1,]
colnames(TTYAbbreviations) <- c("abbr", "meaning")


# Write output ------------------------------------------------------------

write.csv(TTYAbbreviations, file = "Output/TTYInSourceAbbreviations.csv", row.names = F)

