# PopSim

A simple R package for teaching population genetics and the forces of evolution

## Example

```r
#To Simulate with PopSim
sim <- pop_sim(
  gen = 100,
  pop_size1 = 500,
  pop_size2 = 1000,
  pop_size3 = 10000,
  freq1 = 0.1,
  freq2 = 0.5,
  freq3 = 0.8,
  migration_rate = 0.01,
  drift = TRUE,
  bottleneck = 0,
  bottleneck_gen = 20,
  selection = 0.01
)

plot_pop(sim)

```r
#To Calculate Allele Frequency with Nucleotides
generation1 <- data.frame(
  ind=c(1,2,3,4,5,6,7,8,9,10),
  snp1=c("AA", "GA", "GG", "AG", "GG", "GA", "GG", "AA", "GA", "GG"),
  snp2=c("AG", "GG", "GG", "AG", "GG", "GA", "GA", "AA", "GG", "AG"),
  snp3=c("TC", "CT", "TT", "CC", "TT", "CT", "CT","CC", "CT", "TT")
  )

calculate_frequency(generation1)


