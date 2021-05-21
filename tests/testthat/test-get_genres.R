test_that("no (valid) api key",{
  expect_error(get_genres("1abcd"),
               "api_key is not a 32-character string")
  expect_error(get_genres(NULL),
               "api_key is not a 32-character string")
  expect_error(get_genres(1234),
               "api_key is not a 32-character string")
  expect_error(get_genres(12341234123412341234123412341234),
               "api_key is not a 32-character string")
})
