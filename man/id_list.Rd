% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/id_list.R
\name{id_list}
\alias{id_list}
\title{Retrieve TMDB ids for given movie title and year}
\usage{
id_list(title_col, year_col = NULL, id_col, api_key)
}
\arguments{
\item{title_col}{Vector of titles}

\item{year_col}{Vector of respective years}

\item{id_col}{Vector of identifiers}

\item{api_key}{TMDB api key}
}
\value{
Dataframe containing the movies directory name, title, year, and TMDB id
}
\description{
Retrieve TMDB ids for given movie title and year
}
\examples{
\dontrun{
id_list("The Matrix", 1999, 1, api_key)

df <- dataframe(
  title_col = c("The Matrix", "The Matrix Reloaded"),
  year_col = c(1999, 2003), id_col = c(1, 2)
)

id_list(df$title_col, df$year_col, df$id_col, api_key)
}
}
