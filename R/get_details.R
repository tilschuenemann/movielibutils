
#' Title
#'
#' @param tmdb_id TMDB id
#' @param api_key TMDB api key
#'
#' @return Dataframe containing TMDB movie details
#' @export
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr "%>%"
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' get_details(603, api_key)
#' }
get_details <- function(tmdb_id, api_key) {
  # debugging
  # tmdb_id = 704

  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "?api_key=",
    api_key
  )

  request <- GET(url)

  # convert from raw characters to json to dataframe
  details_call <- request$content %>%
    rawToChar() %>%
    fromJSON()

  # TODO do type conversion before multiplying rows

  # WARNING entry gets duplicated per genre id, production company,
  #   production country, spoken language
  details_call <-
    unlist(details_call, recursive = F, use.names = T) %>%
    do.call(cbind, .) %>%
    as.data.frame()

  # type conversion
  to_int <-
    c(
      "budget",
      "genres.id",
      "id",
      "production_companies.id",
      "revenue",
      "runtime",
      "vote_count"
    )

  for (i in 1:7) {
    if (to_int[i] %in% colnames(details_call)) {
      details_call[to_int[i]] <-
        lapply(details_call[to_int[i]], as.integer)
    }
  }

  if ("release_date" %in% colnames(details_call)) {
    details_call$release_date <- as.Date(details_call$release_date)
  }

  if ("vote_average" %in% colnames(details_call)) {
    details_call$vote_average <- as.numeric(details_call$vote_average)
  }

  if ("popularity" %in% colnames(details_call)) {
    details_call$popularity <-
      as.integer(gsub(
        as.character(details_call$popularity),
        pattern = ".",
        replacement = ""
      ))
  }

  return(details_call)
}
