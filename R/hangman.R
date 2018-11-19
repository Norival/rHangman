#' Play the hangman game
#'
#' Sets a game of hangman with a chosen word or a randomly selected word
#'
#' @param dictionnary Either one of 'french' or 'english'. The dictionnary from
#' which to select a word. Ignored if `word` is given
#' @param n.letters The number of letters of the wanted word. Ignored if `word`
#' is given
#' @param word The word to choose
#' @param graph Display or not the awesome graphics, default to TRUE. Will open
#' a graphical window.
#'
#' @return nothing
#'
#' @export

hangman <- function(dictionnary = "french",
                    n.letters   = 6,
                    word        = NULL,
                    graph       = TRUE)
{
  # create graph window
  if (graph == TRUE) {
    plot(axes = FALSE, type = 'n', 1, xlab = "", ylab = "",
         xlim = c(0, 10), ylim = c(0, 10), asp =1)
  }

  # number of tries
  n.tries <- 11

  # choose word
  if (is.null(word)) {
    word <- .choose_word(dictionnary, n.letters)
  }

  # initialize letters
  said.letters  <- character(0)
  found.letters <- numeric(0)

  # main loop for the game
  while (n.tries > 0) {

    # display word and said letters
    .display_word(word, found.letters)

    # ask for a letter
    letter <- character(0)
    cat("Letter ? (", n.tries, " tries left)\n", sep = "")
    .display_letters(word, said.letters)

    while (length(letter) == 0 || !(letter %in% letters)) {
      letter <- tolower(scan(n = 1, what = "character", quiet = TRUE))
    }

    if (letter %in% said.letters) {
      # the letter has already been said, go to next iteration
      cat("The letter", letter, "has already been said! Choose another one :)\n")
      next
    }

    said.letters <- c(said.letters, letter)
    good.letters <- which(letter == unlist(strsplit(word, "")))

    if (length(good.letters) > 0) {
      # the letter is in the word
      cat("The letter", letter, "occurs", length(good.letters), "times within the word :)\n")
    } else {
      # the letter is not in the word
      cat("The letter", letter, "doesn't occur within the word :(\n")
      n.tries <- n.tries - 1
      .draw(n.tries)
    }

    # store found letters
    found.letters <- c(found.letters, good.letters)

    if (length(found.letters) == nchar(word)) {
      # the player won
      .display_word(word, found.letters)
      cat("You got it! It was:", word, "\n")

      return(invisible())
    }
  }

  # the player lost
  cat("You lose! The word was:", word, "\n")

  return(invisible())
}
