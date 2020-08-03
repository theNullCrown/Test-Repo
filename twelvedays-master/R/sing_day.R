#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, i){

  my_line <- paste("On the", dataset$Day.in.Words[i], "day of Christmas, my true love sent to me", "\n")

  if(i == 1){

    my_line <- paste(my_line, dataset$Full.Phrase[i])

  } else{

    for (j in i:2){

      my_line <- paste0(" ", my_line, dataset$Full.Phrase[j], ", ")

    }
    my_line <- paste(my_line, "and", dataset$Full.Phrase[1])
  }


  return(my_line)

}



