#Functions
calculate_allele_freq <- function(population) {

  # Remove the individual ID column
  genotypes <- population[, -1]

  # Put all genotypes into one list
  genotypes <- unlist(genotypes)

  # Split genotypes into individual alleles
  alleles <- unlist(strsplit(genotypes, ""))

  # Count each allele
  allele_counts <- table(alleles)

  # Calculate total number of alleles
  total_alleles <- length(alleles)

  # Calculate pooled allele frequencies
  pooled_frequency <- allele_counts / total_alleles

  # Return the frequencies
  return(pooled_frequency)
}

