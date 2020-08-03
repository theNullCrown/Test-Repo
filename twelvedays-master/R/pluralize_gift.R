#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift) {

  general <- str_detect(gift, "y$") | str_detect(gift, "oo")

  gift <- gift %>%
    str_replace("oo", "ee") %>%
    str_replace("y$", "ies")

  gift[!general] <- paste0(gift[!general], "s")

 return(gift)

}

