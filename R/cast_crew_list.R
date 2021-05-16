
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
#'
#' @examples
#' \dontrun{
#' my_movie_ids <- data.frame(results.id = c(603, 604, 605))
#' cast_crew_list(my_movie_ids, api_key)
#' }
cast_crew_list <- function(id_col, api_key) {
  j <- length(id_col)

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

  # clean colnames
  colnames(cc_details) <- colnames(cc_details) %>%
    paste0("cc_", .)

  cc_details <- data.table(cc_details)

  return(cc_details)
}
