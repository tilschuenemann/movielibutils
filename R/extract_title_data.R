
#' Title
#'
#' @param directory_name Directory name to extract data from.
#' @param convention Different naming conventions are supported. See examples.
#'
#' @return Dataframe containing title, year and optionally subtitle abbreviation.
#' @export
#'
#' @importFrom dplyr between
#' @importFrom dplyr nth
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_length
#' @importFrom graphics title
#'
#' @examples
#' extract_title_data("1999 - The Matrix", 1)
#' extract_title_data("(1999) The Matrix", 2)
#' extract_title_data("The Matrix (1999)", 3)
#' extract_title_data("The Matrix (1999) (OmU)", 4)
extract_title_data <- function(directory_name, convention) {
  if (!is.character(directory_name)) {
    print("directory name must be a string")
    stop()
  } else if (!between(convention, 1, 4) && !is.integer(convention)) {
    print("convention id must be between 1 and 4")
    stop()
  }

  # debugging
  # directory_name = "1999 - The Matrix"
  # convention = 1

  # TODO tests: try catch block for invalid names
  #  " (1999)"
  #  " (321)" ?

  if (convention == 1 || convention == 2) {
    year <- str_extract(directory_name, pattern = "[:digit:]{4}")
    title <- substr(directory_name, 8, str_length(directory_name))
    dir_df <- data.frame(
      title = title,
      year = year
    )
  } else if (convention == 3) {
    year <- nth(str_extract_all(directory_name, pattern = "[:digit:]{4}"), n = -1L)
    title <- substr(directory_name, 0, str_length(directory_name) - 8)
    dir_df <- data.frame(
      title = title,
      year = year
    )
  } else if (convention == 4) {
    year <- str_extract_all(directory_name, pattern = "[:digit:]{4}") %>%
      nth(n = -1L) %>%
      as.integer()

    subtitle <- str_extract_all(directory_name,
      pattern = "[^\\(|\\)|[:space:]][:alnum:]*",
      simplify = T
    ) %>%
      nth(n = -1L)

    title <- substr(directory_name, 0, str_length(directory_name)
    - str_length(year)
      - str_length(subtitle)
      - 6)

    dir_df <- data.frame(
      title = title,
      year = year,
      subtitle = subtitle
    )
  }

  return(dir_df)
}
