# RRxNorm2.R
#
# Copyright Antoine Lizee 04/2015 - antoine.lizee@gmail.com 

rm(list = ls())
library(plyr)


# Load files --------------------------------------------------------------

if (file.exists(fileName <- "Input/allMatchesWithProperties.RData")) {
  load(fileName)
} else {
  stop("Get the data from the API first !")
}

medTable <- read.delim("Input/medications.tsv", stringsAsFactors = F)


# First Analysis ----------------------------------------------------------

pc <- function(prop) paste0("(", round(prop*100), "%)")
                            
cat("Here are some numbers:\n")
cat(" 1. Only", length(unique(results$medication)), "medications got matched with a valid concept, out of", nrow(medTable), "initially.\n")

nBestMatches <- tapply(results$score, results$medication, function(scores) sum(scores == max(scores)))
cat(" 2. From these matched medications,", sum(nBestMatches == 1), pc(mean(nBestMatches == 1)), "have an unambiguous first match.\n")

cat(" 3. These unambiguous hits match to a total of", length(unique(results[ results$medication %in% names(nBestMatches)[nBestMatches == 1], "rxcui"])), "different rxcuis.\n")
                       
hm100 <- tapply(results$score == 100, results$medication, sum)
cat(" 4.", sum(hm100>0), pc(mean(hm100>0)), "medications have at least one perfect match, with", 
    sum(hm100 == 1), pc(mean(hm100>0)), "being unambiguous.\n")

cat(" 5. These unambiguous perfect hits match to a total of", length(unique(results[ results$medication %in% names(hm100)[hm100 == 1], "rxcui"])), "different rxcuis.\n")

unambiguousMatches <- ddply(results, ~medication, function(dfi) {
  maxScore <- dfi$score == max(dfi$score)
  if (sum(maxScore) > 1) NULL
  else dfi[maxScore,]
})

write.csv(unambiguousMatches, "Output/unambiguousMatchesWithProperties.csv")


# Have a look -------------------------------------------------------------

View(results[1:25, c("medication", "name", "score")])
