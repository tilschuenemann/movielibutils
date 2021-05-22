#' Extract movie title and year from a vector
#'
#' @param name_vector Name(s) to extract title, year and potentially subtitle from.
#' @param convention See vignette.
#'
#' @return Dataframe containing directory name, movie year and title, optionally subtitles suffix (based on convention parameter).
#' @export
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr between
#' @importFrom dplyr nth
#' @importFrom dplyr mutate
#' @importFrom graphics title
#' @importFrom stringi stri_sub
#' @importFrom stringi stri_extract
#' @importFrom stringi stri_extract_first
#' @importFrom stringi stri_extract_last
#'
#' @examples
#' extract_names("The Matrix (1999)", 3)
#'
#' name_list <- c("The Matrix (1999)", "The Matrix Reloaded (2003)", "The Matrix Revolutions (2003)")
#' extract_names(name_list, 3)
extract_names <- function(name_vector, convention) {

  # debugging
  # names_or_list <- "The Matrix (1999) (OmU)"
  # convention <- 4

  if (is.data.frame(name_vector) && ncol(name_vector) > 1) {
    stop("please specify which column to read names from")
  }

  name_vector <- unlist(name_vector, use.names = FALSE)
  j <- length(name_vector)

  if (!between(convention, 1, 4) && !is.integer(convention)) {
    stop("convention id must be between 1 and 4")
  } else if (length(j) == 0) {
    stop("supplied name vector is empty")
  }

  print("extracting names")

  name_vector <- data.frame(disc_dir = name_vector)

  if (convention == 1) {
    extracted_names <- name_vector %>%
      mutate(
        year = as.integer(stri_extract(disc_dir, regex = "[:digit:]{4}", mode = "first")),
        title = stri_sub(disc_dir, nchar(as.character(year)) + nchar(" - ")+1, nchar(disc_dir))
      )
  } else if (convention == 2) {
    extracted_names <- name_vector %>%
      mutate(
        year = as.integer(stri_extract_first(disc_dir, regex = "[:digit:]{4}")),
        title = stri_sub(disc_dir, nchar(as.character(year)) + nchar("() ")+1, nchar(disc_dir))
      )
  } else if (convention == 3) {
    extracted_names <- name_vector %>%
      mutate(
        year = as.integer(stri_extract_last(disc_dir, regex = "[:digit:]{4}")),
        title = stri_sub(disc_dir, 0, nchar(disc_dir) - nchar(as.character(year)) - nchar(" ()"))
      )
  } else if (convention == 4) {
    extracted_names <- name_vector %>%
      mutate(
        year = as.integer(stri_extract(disc_dir, regex = "[:digit:]{4}", mode = "last")),
        subtitle = stri_extract(disc_dir, regex = "[^\\(|\\)][:alnum:]*", mode = "last"),
        title = stri_sub(disc_dir, 0, nchar(disc_dir) - nchar(as.character(year)) - nchar(subtitle) - nchar(" () ()"))
      )
  }

  return(extracted_names)
}
