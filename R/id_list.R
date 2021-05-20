#' Title
#'
#' @param title_col Vector of titles
#' @param year_col Vector of respective years
#' @param id_col Vector of identifiers
#' @param api_key TMDB api key
#'
#' @return Dataframe containing the movies directory name, title, year, and TMDB id
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom data.table data.table
#' @importFrom dplyr slice
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' id_list("The Matrix", 1999, 1, api_key)
#'
#' df <- dataframe(title_col = c("The Matrix", "The Matrix Reloaded"), year_col = c(1999, 2003), id_col = c(1, 2))
#'
#' id_list(df$title_col, df$year_col, df$id_col, api_key)
#' }
id_list <- function(title_col, year_col, id_col, api_key) {
  # TODO check for empty or null columns
  if (length(title_col) != length(year_col) ||
    length(title_col) != length(id_col)) {
    stop("vector lengths differ")
  }

  # if dataframes are passed as arguments with no specified column
  #   and more than one definite column, the program is stopped
  if (is.data.frame(title_col) && ncol(title_col) != 1 ||
    is.data.frame(year_col) && ncol(year_col) != 1 ||
    is.data.frame(id_col) && ncol(id_col) != 1) {
    stop("supplied dataframes have more than one column / no column specified")
  }

  title_col <- unlist(title_col, use.names = FALSE)
  year_col <- unlist(year_col, use.names = FALSE)
  id_col <- unlist(id_col, use.names = FALSE)

  # create df for storing api results
  api_ids <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(api_ids) <- c("tmdb_id", "disc_dir")
  api_ids$tmdb_id <- as.integer(api_ids$tmdb_id)

  j <- length(title_col)

  # display progress
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) (:eta)",
    total = j
  )

  print("getting tmdb ids")

  for (i in 1:j) {
    api_id <- data.frame(
      results.id = NA,
      disc_dir = id_col[i]
    )

    # first api call with title, year
    api_id$results.id <- get_id(
      title_col[i],
      year_col[i],
      api_key
    )

    # same as above but without year, backup call
    if (is.na(api_id$results.id)) {
      api_id$results.id <- get_id(
        title_col[i],
        NULL,
        api_key
      )
    }

    # append response
    api_ids <- rbind(api_ids, api_id)

    # add progress
    pb$tick()
  }

  # unlist atomic
  api_ids$results.id <- unlist(api_ids$results.id, recursive = F, use.names = F)

  api_ids <- data.table(api_ids)

  return(api_ids)
}

get_id <- function(title, year, api_key) {

  # TODO tests for parameters

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
