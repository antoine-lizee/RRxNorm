# RRxNorm2.R
#
# The golden link:
# http://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/abbreviations.html#mrdoc_TTY
# http://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/release/precedence_suppressibility.html
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

pc <- function(prop) paste0("(", round(prop*100, 1), "%)")

cat("Here are some numbers:\n")
cat(" 1. Only", length(unique(results$medication)), "medications got matched with a valid concept, out of", nrow(medTable), "initially.\n")

nBestMatches <- tapply(results$score, results$medication, function(scores) sum(scores == max(scores)))
cat(" 2. From these matched medications,", sum(nBestMatches == 1), pc(mean(nBestMatches == 1)), "have an unambiguous first match.\n")

unambiguousMatches <- ddply(results, ~medication, function(dfi) {
  maxScore <- dfi$score == max(dfi$score)
  if (sum(maxScore) > 1) NULL
  else dfi[maxScore,]
})
stopifnot(nrow(unambiguousMatches) == sum(nBestMatches == 1))
cat(" 3. These unambiguous hits match to a total of", length(unique(unambiguousMatches$rxcui)), "different rxcuis.\n")

hm100 <- tapply(results$score == 100, results$medication, sum)
cat(" 4.", sum(hm100>0), pc(mean(hm100>0)), "medications have at least one perfect match, with", 
    sum(hm100 == 1), pc(mean(hm100>0)), "being unambiguous.\n")

cat(" 5. These unambiguous perfect hits match to a total of", length(unique(results[ results$medication %in% names(hm100)[hm100 == 1], "rxcui"])), "different rxcuis.\n")

write.csv(unambiguousMatches, "Output/unambiguousMatches.csv", row.names = F)


# QC -------------------------------------------------------------

# View(unambiguousMatches[ unambiguousMatches$score != 100, c("medication", "name", "score")])
write.csv(unambiguousMatches[ order(unambiguousMatches$score), c("score", "medication", "name")], 
          "Output/unambiguousMatchesForQC.csv",
          row.names = F)


# Enrichment --------------------------------------------------------------

TTYs <- read.csv("Output//TTYInSourceAbbreviations.csv", stringsAsFactors = F)
# Are all the TTYs we retrive in this thesaurus?
all(unique(results$tty) %in% TTYs$abbr)
# See the TTYs from Source of the retrieved concepts:
TTYTable <- ddply(merge(results, TTYs, by.x = "tty", by.y = "abbr", all.y = F), ~ tty, summarize, N=length(meaning), meaning=meaning[1])
TTYTable[order(TTYTable$N),]
# Order them:
ttyPriority <- c(                                
  "SCD", #  "1500" "Semantic Clinical Drug"                                    
  "SBD", #  "1632" "Semantic branded drug"  
  "SCDF", # "  15" "Semantic clinical drug and form"                           
  "SBDF", # "  63" "Semantic branded drug and form"                            
  "BN", #   "  31" "Fully-specified drug brand name that can not be prescribed"
  "SCDC", # "  12" "Semantic Drug Component"                                   
  "SBDC", # "  76" "Semantic Branded Drug Component"                           
  "IN", #   "   8" "Name for an ingredient"                                    
  "MIN", #  "   1" "name for a multi-ingredient"                               
  "PIN", #  "   2" "Name from a precise ingredient"                            
  "GPCK", # "  18" "Generic Drug Delivery Device"                              
  "BPCK", # "  78" "Branded Drug Delivery Device"                              
  "SCDG", # "   2" "Semantic clinical drug group"                              
  "SBDG", # "  35" "Semantic branded drug group"                               
  "DF", #   "   2" "Dose Form" 
  "DFG" #  "  1" "Dose Form Group"
)

resolveBest <- function(dfi) {
  # Function to resolve ambiguity when necessary (and possible)
  dfi$ttypriority <- match(dfi$tty, ttyPriority)
  indexOrder <- order(-dfi$score, dfi$ttypriority, -dfi$nAtoms)
  if(anyDuplicated(dfi[indexOrder[1:2], c("score", "ttypriority", "nAtoms")])) {
      warning(paste("Non-resolvable ambiguity for", dfi$medication))
      return(data.frame(dfi[indexOrder[1],], resolved = F))
  } 
  return(data.frame(dfi[indexOrder[1],], resolved = T))
}

allResolvedMatches <- ddply(results, ~medication, resolveBest, .progress = "text")
resolvedMatches <- allResolvedMatches[allResolvedMatches$resolved, -15]

cat(" 6. After resolving ambiguity based on the term type from source prioritisation, we get ",
    nrow(resolvedMatches), pc(nrow(resolvedMatches)/length(nBestMatches)),
    " medications finally matched.\n")
write.csv(resolvedMatches, "Output/resolvedMatches.csv", row.names = F)
write.csv(allResolvedMatches, "Output/allResolvedMatches.csv", row.names = F)


# QC ----------------------------------------------------------------------

resolvedQC <- allResolvedMatches[ ! allResolvedMatches$medication %in% unambiguousMatches$medication, c("medication", "name", "score")]
# View(resolvedQC)
write.csv(resolvedQC,
          "Output/resolvedMatchesForQC.csv",
          row.names = F)







