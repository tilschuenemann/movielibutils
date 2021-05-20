#' Title
#'
#' @param name_vector Name(s) to extract title, year and potentially subtitle from.
#' @param convention See vignette.
#'
#' @return Dataframe containing directory name, movie year and title, optionally subtitles suffix (based on convention parameter).
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom dplyr "%>%"
#' @importFrom dplyr between
#' @importFrom dplyr nth
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_length
#' @importFrom graphics title
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

  # display progress
  # TODO add message "extracting names"
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) (:eta)",
    total = j
  )

  # create df for return
  extracted_names <- NULL

  for (i in 1:j) {

    # extract name based on convention
    if (convention == 1 || convention == 2) {
      year <- str_extract(name_vector[i], pattern = "[:digit:]{4}")
      # TODO 8 is magic number
      title <- substr(name_vector[i], 8, str_length(name_vector[i]))
      extracted_name <- data.frame(
        title = title,
        year = year,
        disc_dir = name_vector[i]
      )
    } else if (convention == 3) {
      year <- nth(str_extract_all(name_vector[i], pattern = "[:digit:]{4}", simplify = TRUE), n = -1L)
      # TODO 7 is magic number
      title <- substr(name_vector[i], 0, str_length(name_vector[i]) - 7)
      extracted_name <- data.frame(
        title = title,
        year = year,
        disc_dir = name_vector[i]
      )
    } else if (convention == 4) {
      year <- str_extract_all(name_vector[i], pattern = "[:digit:]{4}", simplify = TRUE) %>%
        nth(n = -1L) %>%
        as.integer()

      subtitle <- str_extract_all(name_vector[i],
        pattern = "[^\\(|\\)|[:space:]][:alnum:]*",
        simplify = T
      ) %>%
        nth(n = -1L)

      # TODO 6 is magic number
      title <- substr(name_vector[i], 0, str_length(name_vector[i])
      - str_length(year)
        - str_length(subtitle)
        - 6)

      extracted_name <- data.frame(
        title = title,
        year = year,
        subtitle = subtitle,
        disc_dir = name_vector[i, ]
      )
    }

    extracted_names <- rbind(extracted_names, extracted_name)

    pb$tick()
  }

  extracted_names$year <- as.integer(extracted_names$year)

  return(extracted_names)
}
