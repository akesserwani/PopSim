#' Population Genetics Simulator
#'
#' Simulates allele-frequency change in three populations through
#' genetic drift, migration, natural selection, and bottlenecks.
#'
#' @param gen Number of generations.
#' @param pop_size1 Starting size of population 1.
#' @param pop_size2 Starting size of population 2.
#' @param pop_size3 Starting size of population 3.
#' @param freq1 Starting allele frequency in population 1.
#' @param freq2 Starting allele frequency in population 2.
#' @param freq3 Starting allele frequency in population 3.
#' @param migration_rate Migration rate between populations.
#' @param drift TRUE or FALSE. Whether genetic drift occurs.
#' @param selection Selection coefficient for allele A.
#' @param bottleneck Proportion of each population lost during bottleneck.
#' @param bottleneck_gen Generation when bottleneck occurs.
#'
#' @return A list containing simulation results and parameters.
#' @export


pop_sim <- function(
  gen = 100,

  pop_size1 = 100,
  pop_size2 = 100,
  pop_size3 = 100,

  freq1 = 0.9,
  freq2 = 0.5,
  freq3 = 0.1,

  migration_rate = 0,

  drift = TRUE,

  selection = 0,

  bottleneck = 0,
  bottleneck_gen = 50
) {

  # Check inputs
  if (migration_rate < 0 || migration_rate > 1) {
    stop("migration_rate must be between 0 and 1")
  }
  if (bottleneck < 0 || bottleneck > 1) {
    stop("bottleneck must be between 0 and 1")
  }

  if (any(c(freq1, freq2, freq3) < 0) ||
      any(c(freq1, freq2, freq3) > 1)) {
    stop("allele frequencies must be between 0 and 1")
  }
  if (selection <= -1) {
    stop("selection must be greater than -1")
  }

  # Starting population sizes
  pop1 <- pop_size1
  pop2 <- pop_size2
  pop3 <- pop_size3

  p1 <- freq1
  p2 <- freq2
  p3 <- freq3

  # Create dataframe
  pop_df <- data.frame(
    generation = 0:gen,
    pop1 = NA,
    pop2 = NA,
    pop3 = NA,

    freq1 = NA,
    freq2 = NA,
    freq3 = NA
  )

  # Run simulation

  for (t in 0:gen) {
    if (t > 0) {
      # 1. BOTTLENECK
      if (t == bottleneck_gen && bottleneck > 0) {

        pop1 <- round(pop1 * (1 - bottleneck))
        pop2 <- round(pop2 * (1 - bottleneck))
        pop3 <- round(pop3 * (1 - bottleneck))
        # Prevent populations from reaching zero
        pop1 <- max(pop1, 2)
        pop2 <- max(pop2, 2)
        pop3 <- max(pop3, 2)
      }

      # 2. MIGRATION
      if (migration_rate > 0) {
        migrant_freq <- weighted.mean(

          c(p1, p2, p3),

          w = c(pop1, pop2, pop3)
        )


        p1 <- (1 - migration_rate) * p1 +
              migration_rate * migrant_freq

        p2 <- (1 - migration_rate) * p2 +
              migration_rate * migrant_freq

        p3 <- (1 - migration_rate) * p3 +
              migration_rate * migrant_freq
      }

      # 3. SELECTION
      if (selection != 0) {

        # Allele A has fitness = 1 + selection
        # Other allele has fitness = 1
        p1 <- (p1 * (1 + selection)) /
              ((p1 * (1 + selection)) + (1 - p1))

        p2 <- (p2 * (1 + selection)) /
              ((p2 * (1 + selection)) + (1 - p2))

        p3 <- (p3 * (1 + selection)) /
              ((p3 * (1 + selection)) + (1 - p3))
      }

      # 4. GENETIC DRIFT
      if (drift == TRUE) {
        # Population 1
        A1 <- rbinom(
          1,
          size = 2 * pop1,
          prob = p1
        )

        p1 <- A1 / (2 * pop1)

        # Population 2
        A2 <- rbinom(
          1,
          size = 2 * pop2,
          prob = p2
        )

        p2 <- A2 / (2 * pop2)

        # Population 3
        A3 <- rbinom(
          1,
          size = 2 * pop3,
          prob = p3
        )

        p3 <- A3 / (2 * pop3)
      }
    }



    # Record generation
    pop_df[t + 1, ] <- c(
      t,
      pop1,
      pop2,
      pop3,
      p1,
      p2,
      p3
    )
  }

  # Return results
  return(
    list(
      pop_df = pop_df,

      parms = list(
        gen = gen,
        migration_rate = migration_rate,
        drift = drift,
        selection = selection,
        bottleneck = bottleneck,
        bottleneck_gen = bottleneck_gen
      )
    )
  )
}
