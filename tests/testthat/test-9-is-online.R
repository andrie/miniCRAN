test_that("is online", {
  skip_on_cran()

  # Should always be TRUE, but will trigger github action failure if
  # p3m is offline
  expect_true(is.online())
  expect_true(is.online(p3m("2024-01-02")))
})
