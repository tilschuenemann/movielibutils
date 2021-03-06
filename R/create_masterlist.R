
#' Retrieve all relevant movie metadata in a large data.table for a given name vector.
#'
#' @param wd Directory name containing your movie directories.
#' @param convention See examples.
#' @param api_key TMDB api key
#'
#' @return Extensive dataframe containing directory name, title, year and TMDB metadata for movie as well as cast.
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#' @importFrom dplyr left_join
#' @importFrom graphics title
#'
create_masterlist <- function(wd, convention, api_key) {

  # TODO insert wd file scan and extract_names
  movies <- list.files(wd, recursive = FALSE, full.names = FALSE)

  movies_ex <- extract_names(movies, convention)

  movies_id <- id_list(
    movies_ex$title,
    movies_ex$year,
    movies_ex$disc_dir,
    api_key
  )

  movies_details <- details_list(
    movies_id$results.id,
    api_key
  )

  movies_cc <- cast_crew_list(
    movies_id$results.id,
    api_key
  )

  genres <- get_genres(api_key)

  disc_db_id <- data.table(left_join(movies, movies_id, by = "disc_dir"))


  disc_db_id_dt <- merge(disc_db_id, movies_details,
    by.x = "results.id",
    by.y = "m_id", all.x = T,
  )

  disc_db_id_dt3 <- merge(disc_db_id_dt, movies_cc,
    by.x = "results.id",
    by.y = "cc_results.id", all.x = T, allow.cartesian = T
  )

  disc_db_id_dt3 <- disc_db_id_dt3 %>%
    select(-year, -title)

  return(disc_db_id_dt3)
}
