
plot_pop <- function(x) {

  sim <- x$pop_df

  # Two graphs + one small legend area

  layout(
    matrix(c(1, 2, 3), ncol = 1),
    heights = c(1, 1, 0.15)
  )

  # ==========================================
  # 1. ALLELE FREQUENCY
  # ==========================================

  plot(

    sim$generation,
    sim$freq1,

    type = "l",

    col = "orange",
    lwd = 2,

    ylim = c(0, 1),

    xlab = "Generation",
    ylab = "Allele Frequency",

    main = "Allele Frequency"
  )


  lines(
    sim$generation,
    sim$freq2,
    col = "blue",
    lwd = 2
  )


  lines(
    sim$generation,
    sim$freq3,
    col = "red",
    lwd = 2
  )

  # ==========================================
  # 2. POPULATION SIZE
  # ==========================================

  max_pop <- max(
    sim[, c("pop1", "pop2", "pop3")]
  )

  plot(

    sim$generation,
    sim$pop1,

    type = "l",

    col = "orange",
    lwd = 2,

    ylim = c(0, max_pop),

    xlab = "Generation",
    ylab = "Population Size",

    main = "Population Size"
  )

  lines(
    sim$generation,
    sim$pop2,
    col = "blue",
    lwd = 2
  )


  lines(
    sim$generation,
    sim$pop3,
    col = "red",
    lwd = 2
  )

  # ==========================================
  # SHARED LEGEND
  # ==========================================

  par(mar = c(0, 0, 0, 0))

  plot.new()


  legend(

    "center",

    legend = c(
      "Population 1",
      "Population 2",
      "Population 3"
    ),

    col = c(
      "orange",
      "blue",
      "red"
    ),

    lwd = 2,

    horiz = TRUE,

    bty = "n"
  )
}
