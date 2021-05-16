#' Title
#'
#' @param title Movie title
#' @param year Release year
#' @param api_key TMDB api key
#'
#' @return TMDB id
#' @export
#'
#' @importFrom httr GET
#' @importFrom stringr str_replace_all
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom dplyr "%>%"
#' @importFrom jsonlite fromJSON
#' @importFrom graphics title
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_id("The Matrix", 1999, api_key)
#' }
#'
get_id <- function(title, year, api_key) {

  # debugging
  # title = "Sodomites"
  # year = 1998

  title <- str_replace_all(title, pattern = "[ ]", replacement = "%20")

  # build url for request
  if (is.null(year)) {
    url <- paste0(
      "https://api.themoviedb.org/3/search/movie?api_key=",
      api_key,
      "&query=",
      title,
      "&include_adult=true"
    )
  } else {
    url <- paste0(
      "https://api.themoviedb.org/3/search/movie?api_key=",
      api_key,
      "&query=",
      title,
      "&year=",
      year,
      "&include_adult=true"
    )
  }

  request <- GET(url)

  # convert from raw characters to json to dataframe
  id_call <- request$content %>%
    rawToChar() %>%
    fromJSON()

  # report NA in case of no results
  if (id_call$total_results == 0) {
    id_call <- data.frame(results.id = NA)
    return(id_call)
  }

  # unlist and format
  # [2] references the actual results
  id_call <-
    unlist(id_call[2], recursive = F, use.names = T) %>%
    do.call(what = cbind, .) %>%
    as.data.frame()

  # if there are more than two results, only the most popular is kept
  n <- nrow(id_call)

  if (is.null(n) || n == 0) {
    # insert NA in case of no result
    id_call <- data.frame(results.id = as.integer(NA))
  } else if (n > 1) {
    # get most popular one if there are multiple results
    id_call <- id_call %>%
      slice(which.max(.data$results.popularity))
  }

  id_call <- as.integer(id_call$results.id) %>%
    unlist()

  # only return tmdb id
  return(id_call)
}
