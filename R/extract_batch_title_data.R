
#' Title
#'
#' @param movie_directory The directory where movies are stored in.
#' @param convention Different naming conventions are supported. See extract_name.
#'
#' @return Dataframe containing directory name, movie year and title, optionally subtitles suffix (based on convention parameter).
#' @export
#'
#' @importFrom progress progress_bar
#' @importFrom dplyr between
#' @importFrom dplyr "%>%"
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' extract_batch_title_data("~/my_movie_folder/", 1)
#' }
extract_batch_title_data <- function(movie_directory, convention) {
  if (!is.character(movie_directory)) {
    print("given movie directory parameter is not a string")
  } else if (!between(convention, 1, 4) && !is.integer(convention)) {
    print("convention parameter must be between 1 and 4")
    stop()
  }

  # get dir names from wd
  disc_movies <- data.frame(disc_dir = list.dirs(movie_directory, recursive = F, full.names = F))

  j <- nrow(disc_movies)

  if (j == 0) {
    print("no directories found")
    stop()
  }

  # display progress
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) (:eta)",
    total = j
  )

  disc_db <- NULL

  print("reading movie directory")

  for (i in 1:j) {
    db_entry <- cbind(
      disc_dir = disc_movies[i, 1],
      extract_title_data(
        disc_movies[i, 1],
        convention
      )
    )

    disc_db <- rbind(disc_db, db_entry)

    disc_db$year <- as.integer(disc_db$year)

    pb$tick()
  }
  return(disc_db)
}
