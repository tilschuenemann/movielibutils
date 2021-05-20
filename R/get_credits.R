#' Title
#'
#' @param tmdb_id TMDB id
#' @param api_key TMDB api key
#'
#' @return Dataframe containing TMDB credits (cast and crew).
#' @export
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom dplyr select_all
#' @importFrom dplyr "%>%"
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_replace
#' @importFrom stringr str_pad
#' @importFrom graphics title
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_credits(603, api_key)
#' }
get_credits <- function(tmdb_id, api_key) {
  # debugging
  # tmdb_id = 704

  # TODO tests for tmdb id and api_key

  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    ",/credits?api_key=",
    api_key
  )

  request <- GET(url)

  # convert from raw characters to json to dataframe
  credits_call <- request$content %>%
    rawToChar() %>%
    fromJSON()

  credits_call <-
    unlist(credits_call, recursive = F, use.names = T) %>%
    do.call(cbind, .) %>%
    as.data.frame()

  # clean colnames
  cast_call <- credits_call %>%
    select(starts_with("cast")) %>%
    select_all(~ str_replace(pattern = "cast.", replacement = "", .))

  crew_call <- credits_call %>%
    select(starts_with("crew")) %>%
    select_all(~ str_replace(pattern = "crew.", replacement = "", .))

  # append cast and crew
  cast_crew_details <- bind_rows(cast_call, crew_call)

  # type conversion
  to_int <- c("gender", "id", "cast_id", "order")

  for (i in 1:4) {
    if (to_int[i] %in% colnames(cast_crew_details)) {
      cast_crew_details[to_int[i]] <-
        lapply(cast_crew_details[to_int[i]], as.integer)
    }
  }

  if ("popularity" %in% colnames(cast_crew_details)) {
    cast_crew_details$popularity <-
      as.integer(str_replace(
        pattern = "\\.",
        replacement = "",
        str_pad(
          cast_crew_details$popularity,
          width = 5,
          side = "right",
          pad = "0"
        )
      ))
  }

  return(cast_crew_details)
}
