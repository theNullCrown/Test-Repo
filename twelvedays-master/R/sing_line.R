#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param lain The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import english
#'
#' @export

sing_line <- function(dataset, lain, phrase_col) {

  phrases <- dataset %>% pull({{phrase_col}})

  x <- paste("On the", english::ordinal(lain), "day of Christmas, my true love sent to me,")
  y <- paste("")

  if (lain == 1) {
    y <- paste(phrases[1])
  }
  else {
    z <- map_chr(lain:2, ~paste0( y, phrases[.x], ", ", sep ="\n")) %>% str_c(collapse ="")
    z <- paste(z, "and", phrases[1])
    x <- paste(x, z, sep ="\n")
  }


  return(x)

}



