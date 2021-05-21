
<!-- README.md is generated from README.Rmd. Please edit that file -->

# movielibutils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

movielibutils helps to retrieve and join
[TMDB](https://www.themoviedb.org/) metadata for your analyzing needs!

## Roadmap

-   ~~create vignette~~

-   ~~remove functions for 1-length arguments and integrate
    functionality for wrappers~~

-   ~~replace stringr with stringi~~

-   ~~add titles to manpages~~

-   refactor variable names and function names

-   add sensible tests within the functions as well as unit testing

-   ~~add lifecycle badge to readme~~

## Similar packages and differences

[TMDb by AndreaCapozio](https://github.com/AndreaCapozio/TMDb)

The TMDb package has a much broader functionality than movielibutils -
tv shows, movies, people, collections, while movielibutils only looks up
a movies id, its details, cast and crew.

While TMDb returns lists, movielibutils delivers data.tables, thus
resulting in a format that is ready to analyze. movielibutils also works
in batches, whereas TMDb only supplies single-call functions.
