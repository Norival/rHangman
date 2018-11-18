draw <- function(triesLeft) {
  # Draws the hangman step by step

  aa <- 11 - triesLeft
  stepsx0 <- c(0, 1, 1, 1, 8, 8, 8, 8, 8, 8)
  stepsx1 <- c(10, 1, 8, 3, 8, 8, 7, 9, 6.5, 9.5)
  stepsy0 <- c(0, 0, 10, 8, 10, 6, 3, 3, 6, 6)
  stepsy1 <- c(0, 10, 10, 10, 8, 3, 1.5, 1.5, 5, 5)

  if (aa == 6)
    symbols(circles = 1, x = 8, y = 7,
            add = TRUE, inches = FALSE, lwd = 2)
  else {
    if (aa > 6)
      aa <- aa - 1
    segments(x0 = stepsx0[aa],
             x1 = stepsx1[aa],
             y0 = stepsy0[aa],
             y1 = stepsy1[aa],
             lwd = 2)
  }

  if (aa == 10)
    text(x = 4, y = 0.5, labels = "YOU ARE DEAD!", col = "red")
}
