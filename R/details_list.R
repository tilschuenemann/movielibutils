
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
#'
#' @examples
#' \dontrun{
#' my_list <- c(603, 604, 605)
#' details_list(mylist, api_key)
#' }
#'
details_list <- function(id_list, api_key) {

  # build df for movie detail results
  mdetails <- data.frame(results.id = NULL)

  j <- length(id_list)

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

  # clean colnames
  colnames(mdetails) <- colnames(mdetails) %>%
    paste0("m_", .)

  mdetails <- data.table(mdetails)

  return(mdetails)
}
