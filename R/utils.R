#' Choose a word from a dictionnary
#'
#' Choose a word of the given length from a dictionnary
#'
#' @param dict A vetor of words to choose from
#' @param n.letters The number of letters of the word to be chosen
#' 
#' @return A word. Wil throw an error if no word of the wanted length exists in
#' the dictionnary

.choose_word <- function(dict, n.letters)
{
  dict <- iconv(dict, to = "ASCII//TRANSLIT")
  dict <- dict[-(grep("[[:punct:]]", dict))]

  # get indices of words with n.letters letters
  i <- which(nchar(dict) == n.letters)

  if (length(i) == 0) {
    stop("No word of that length :/")
  }

  # sample a random word and remove accents
  word <- ifelse(length(i) == 1, dict[i], dict[sample(i, 1)])

  return(word)
}


#' Draw the hangman
#'
#' Generate awesome graphics for the game!
#'
#' @param tries.left Number of tries left
#'
#' @return nothing

.draw <- function(tries.left)
{
  # Draws the hangman step by step
  i <- 11 - tries.left
  stepsx0 <- c(0, 1, 1, 1, 8, 0, 8, 8, 8, 8, 8)
  stepsx1 <- c(10, 1, 8, 3, 8, 0, 8, 7, 9, 6.5, 9.5)
  stepsy0 <- c(0, 0, 10, 8, 10, 0, 6, 3, 3, 6, 6)
  stepsy1 <- c(0, 10, 10, 10, 8, 0, 3, 1.5, 1.5, 5, 5)

  if (i == 6) {
    # draw the head
    symbols(circles = 1, x = 8, y = 7,
            add = TRUE, inches = FALSE, lwd = 2)
  } else {
    # draw a segment
    segments(x0 = stepsx0[i],
             x1 = stepsx1[i],
             y0 = stepsy0[i],
             y1 = stepsy1[i],
             lwd = 2)
  }

  if (i == 10) {
    text(x = 4, y = 0.5, labels = "YOU'RE DEAD!", col = "red")
  }

  return(invisible())
}


#' Display the word on the console
#'
#' Display the word on the console, with only found letters shown
#'
#' @param word The word
#' @param found.letters Letters that have already been found
#'
#' @return nothing

.display_word <- function(word, found.letters)
{
  disp.word <- rep("_", nchar(word))
  disp.word[found.letters] <- unlist(lapply(found.letters, function(x) substr(word, x, x)))

  cat(disp.word, "\n")

  return(invisible())
}


#' Display said letters on the console
#'
#' Display the letters that have been said, but not letters that are in the word
#'
#' @param word The word
#' @param said.letters Letters that have already been said
#'
#' @return nothing

.display_letters <- function(word, said.letters)
{
  disp.letters <- said.letters[!(said.letters %in% unlist(strsplit(word, "")))]

  cat("Said letters:", disp.letters, "\n")

  return(invisible())
}
