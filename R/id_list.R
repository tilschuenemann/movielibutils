
#' Title
#'
#' @param title_col list of titles to look up
#' @param year_col list of years
#' @param id_col list of identifiers
#' @param api_key TMDB api key
#'
#' @return Dataframe containing the movies directory name, title, year, and TMDB id
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom data.table data.table
id_list <- function(title_col, year_col, id_col, api_key) {


  # if(length(title_col) != length(year_col) != length(id_col)){
  #  print("provided columns have different lengths")
  #  stop()
  # }

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
