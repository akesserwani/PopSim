# PopSim

A simple R package for teaching population genetics and the forces of evolution

## Example

```r
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