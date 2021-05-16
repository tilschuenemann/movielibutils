
#' Title
#'
#' @param api_key TMDB api key
#'
#' @return Dataframe containing all TMDB genre ids and names.
#' @export
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @importFrom dplyr "%>%"
#' @importFrom graphics title
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_genres(api_key)
#' }
get_genres <- function(api_key) {
  if (!is.character(api_key) &&
    length(api_key) != 32) {
    print("api_key is not a 32-character string")
  }

  print("getting genres")

  url <- paste0(
    "https://api.themoviedb.org/3/genre/movie/list",
    "?api_key=",
    api_key
  )

  request <- GET(url)

  # convert from raw characters to json to dataframe
  genres_call <- request$content %>%
    rawToChar() %>%
    fromJSON()

  genres_call <-
    unlist(genres_call, recursive = F, use.names = T) %>%
    do.call(cbind, .) %>%
    as.data.frame()

  # type conversion
  genres_call$genres.id <- as.integer(genres_call$genres.id)
  genres_call <- data.table(genres_call)

  return(genres_call)
}
