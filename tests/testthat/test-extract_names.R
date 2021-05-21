test_that("single extraction with convention 1 works", {
  expect_equal(
    extract_names("1999 - The Matrix", 1),
    data.frame(
      disc_dir = "1999 - The Matrix",
      year = 1999L,
      title = "The Matrix"
    )
  )

  expect_equal(
    extract_names("1999 - 1234 The Matrix 1234", 1),
    data.frame(
      disc_dir = "1999 - 1234 The Matrix 1234",
      year = 1999L,
      title = "1234 The Matrix 1234"
    )
  )
})

test_that("single extraction with convention 2 works", {
  expect_equal(
    extract_names("(1999) The Matrix", 2),
    data.frame(
      disc_dir = "(1999) The Matrix",
      year = 1999L,
      title = "The Matrix"
    )
  )

  expect_equal(
    extract_names("(1999) 1234 The Matrix 1234", 2),
    data.frame(
      disc_dir = "(1999) 1234 The Matrix 1234",
      year = 1999L,
      title = "1234 The Matrix 1234"
    )
  )
})

test_that("single extraction with convention 3 works", {
  expect_equal(
    extract_names("The Matrix (1999)", 3),
    data.frame(
      disc_dir = "The Matrix (1999)",
      year = 1999L,
      title = "The Matrix"
    )
  )

  expect_equal(
    extract_names("1234 The Matrix 1234 (1999)", 3),
    data.frame(
      disc_dir = "1234 The Matrix 1234 (1999)",
      year = 1999L,
      title = "1234 The Matrix 1234"
    )
  )
})

test_that("single extraction with convention 4 works", {
  expect_equal(
    extract_names("The Matrix (1999) (OmU)", 4),
    data.frame(
      disc_dir = "The Matrix (1999) (OmU)",
      year = 1999L,
      subtitle = "OmU",
      title = "The Matrix"
    )
  )

  expect_equal(
    extract_names("1234 The Matrix (1999) (OmU)", 4),
    data.frame(
      disc_dir = "1234 The Matrix (1999) (OmU)",
      year = 1999L,
      subtitle = "OmU",
      title = "1234 The Matrix"
    )
  )
})

test_that("list extraction with convention 1 works", {
  expect_equal(
    extract_names(c(
      "1999 - The Matrix", "2003 - The Matrix Reloaded",
      "2003 - The Matrix Revolutions"
    ), 1),
    data.frame(
      disc_dir = c(
        "1999 - The Matrix", "2003 - The Matrix Reloaded",
        "2003 - The Matrix Revolutions"
      ),
      year = c(1999L, 2003L, 2003L),
      title = c("The Matrix", "The Matrix Reloaded", "The Matrix Revolutions")
    )
  )
})

test_that("dataframe extraction (1-column) with convention 1 works", {
  expect_equal(
    extract_names(data.frame(titles = c(
      "1999 - The Matrix", "2003 - The Matrix Reloaded",
      "2003 - The Matrix Revolutions"
    )), 1),
    data.frame(
      disc_dir = c(
        "1999 - The Matrix", "2003 - The Matrix Reloaded",
        "2003 - The Matrix Revolutions"
      ),
      year = c(1999L, 2003L, 2003L),
      title = c("The Matrix", "The Matrix Reloaded", "The Matrix Revolutions")
    )
  )
})

test_that("dataframe extraction (1-column) with convention 1 works", {
  test_df <- data.frame(titles = c(
    "1999 - The Matrix", "2003 - The Matrix Reloaded",
    "2003 - The Matrix Revolutions"
  ))

  expect_equal(
    extract_names(test_df, 1),
    data.frame(
      disc_dir = c(
        "1999 - The Matrix", "2003 - The Matrix Reloaded",
        "2003 - The Matrix Revolutions"
      ),
      year = c(1999L, 2003L, 2003L),
      title = c("The Matrix", "The Matrix Reloaded", "The Matrix Revolutions")
    )
  )

  expect_equal(
    extract_names(test_df$titles, 1),
    data.frame(
      disc_dir = c(
        "1999 - The Matrix", "2003 - The Matrix Reloaded",
        "2003 - The Matrix Revolutions"
      ),
      year = c(1999L, 2003L, 2003L),
      title = c("The Matrix", "The Matrix Reloaded", "The Matrix Revolutions")
    )
  )
})

# add test with multiple column df with no specified error

test_that("convention not 1-4 throws error", {
  expect_error(extract_names("The Matrix (1999)", 0),
               "convention id must be between 1 and 4")
  })


