if (length(dir(pattern = "allMatchesWithProperties.RData")) == 1) {
  load("allMatchesWithProperties.RData")
} else {
  stop("Get the data from the API first !")
}


# First Analysis ----------------------------------------------------------

cat("Here are some numbers:")
cat(" 1. Only", length(unique(results$medication)), "medications got matched with a valid concept, out of", nrow(medTable), "initially.\n")

hm100 <- tapply(results$score == 100, results$medication, sum)
cat(" 2. ", sum(hm100>0), " (", round(mean(hm100>0)*100), "%) medications have at least a perfect match, with ", 
    sum(hm100 == 1), " (", round(mean(hm100==1)*100), "%) being inambiguous.", sep = "")

cat( "3. From these inambiguous perfect hits, a total of ", sum(hm100 == 1) )