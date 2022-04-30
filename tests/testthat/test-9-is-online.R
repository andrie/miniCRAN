test_that("is online", {
  skip_on_cran()
 
  # Should always be TRUE, but will trigger github action failure if 
  # MRAN is offline
  expect_true(is.online())
})