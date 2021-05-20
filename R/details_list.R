
#' Title
#'
#' @param id_list List of TMDB ids
#' @param api_key TMDB api_key
#'
#' @return Dataframe containing all TMDB metadata for movies
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom data.table data.table
#' @importFrom dplyr "%>%"
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' my_list <- c(603, 604, 605)
#' details_list(mylist, api_key)
#' }
#'
details_list <- function(id_list, api_key) {
  if (is.data.frame(id_list) && ncol(id_list) > 1) {
    stop("please specify which column to read names from")
  }

  id_list <- unlist(id_list, use.names = FALSE)
  j <- length(id_list)

  if (length(j) == 0) {
    stop("supplied name vector is empty")
  }

  # build df for movie detail results
  mdetails <- data.frame(results.id = NULL)

  # display progress
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) (:eta)",
    total = j
  )

  print("getting movie details")

  for (i in 1:j) {
    if (is.na(id_list[i])) {
      next
    }

    mdetail <- get_details(id_list[i], api_key)

    mdetails <- bind_rows(mdetails, mdetail)

    # add progress
    pb$tick()
  }


  # type conversion
  # the reason why the columns get probed are that they are not always existent
  #   in the API response
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
    if (to_int[i] %in% colnames(mdetails)) {
      mdetails[to_int[i]] <-
        lapply(mdetails[to_int[i]], as.integer)
    }
  }

  if ("release_date" %in% colnames(mdetails)) {
    mdetails$release_date <- as.Date(mdetails$release_date)
  }

  if ("vote_average" %in% colnames(mdetails)) {
    mdetails$vote_average <- as.numeric(mdetails$vote_average)
  }

  if ("popularity" %in% colnames(mdetails)) {
    mdetails$popularity <-
      as.integer(gsub(
        as.character(mdetails$popularity),
        pattern = ".",
        replacement = ""
      ))
  }

  # prefix colnames
  colnames(mdetails) <- colnames(mdetails) %>%
    paste0("m_", .)

  mdetails <- data.table(mdetails)

  return(mdetails)
}
get_details <- function(tmdb_id, api_key) {
  # debugging
  #   tmdb_id = 704

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

  # WARNING entry gets duplicated per genre id, production company,
  #   production country, spoken language
  details_call <-
    unlist(details_call, recursive = F, use.names = T) %>%
    do.call(cbind, .) %>%
    as.data.frame()

  return(details_call)
}
