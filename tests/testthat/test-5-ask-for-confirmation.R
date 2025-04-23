old <- matrix(
  c(
    "nomnoml",
    "BH",
    "digest",
    "2.1",
    "1.54.0-4",
    "0.6.4",
    "2.2",
    "1.55.0-1",
    "0.6.7",
    "https://packagemanager.posit.co/cran/2024-01-02",
    "https://packagemanager.posit.co/cran/2024-01-02",
    "https://packagemanager.posit.co/cran/2024-01-02"
  ),
  ncol = 4,
  dimnames = list(
    c("nomnoml", "BH", "digest"),
    c("Package", "LocalVer", "ReposVer", "Repository")
  )
)


test_that("asking for confirmation", {
  skip_if_not_installed("mockr")

  mockr::with_mock(
    read_line_wrapper = function(...) {
      "y"
    },
    .env = "miniCRAN",
    {
      capture.output(
        z <-
          ask_to_update(old, "source", "3.1", ask = TRUE)
      )
      expect_equal(old, z, ignore_attr = TRUE)
    }
  )

  mockr::with_mock(
    read_line_wrapper = function(...) {
      "n"
    },
    .env = "miniCRAN",
    {
      capture.output(
        z <-
          ask_to_update(old, "source", "3.1", ask = TRUE)
      )
      expect_null(z)
    }
  )

  mockr::with_mock(
    read_line_wrapper = function(...) {
      "y"
    },
    graphics_capable = function() {
      TRUE
    },
    select_from_list = function(choices, ...) {
      choices
    },
    .env = "miniCRAN",
    {
      capture.output(
        z <-
          ask_to_update(old, "source", "3.1", ask = "graphics")
      )
      expect_equal(old, z, ignore_attr = TRUE)
    }
  )
})
