#' Title
#'
#' @param id_col List of TMDB ids
#' @param api_key TMBD api key
#'
#' @return Dataframe containing all TMDB crew and cast data
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom data.table data.table
#' @importFrom dplyr "%>%"
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom dplyr select_all
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_replace
#' @importFrom stringr str_pad
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' my_movie_ids <- data.frame(results.id = c(603, 604, 605))
#' cast_crew_list(my_movie_ids, api_key)
#' }
cast_crew_list <- function(id_col, api_key) {
  if (is.data.frame(id_col) && ncol(id_col) > 1) {
    stop("please specify which column to read names from")
  }

  id_col <- unlist(id_col, use.names = FALSE)
  j <- length(id_col)

  if (length(j) == 0) {
    stop("supplied name vector is empty")
  }

  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) (:eta)",
    total = j
  )

  # create df for results
  cc_details <- data.frame(results.id = NULL)

  print("getting cast and crew")

  for (i in 1:j) {
    if (is.na(id_col[i])) {
      next
    }

    cc_detail <- get_credits(id_col[i], api_key)
    cc_detail$results.id <- id_col[i]

    cc_details <- bind_rows(cc_details, cc_detail)

    # add progress
    pb$tick()
  }

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

  # clean colnames
  colnames(cc_details) <- colnames(cc_details) %>%
    paste0("cc_", .)

  cc_details <- data.table(cc_details)

  return(cc_details)
}
get_credits <- function(tmdb_id, api_key) {
  # debugging
  # tmdb_id = 704

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

  return(cast_crew_details)
}
