if (require(testthat)) {
  old.env <- Sys.getenv("miniCRAN.mock.download")
  Sys.setenv("miniCRAN.mock.download" = TRUE)
  
  test_check("miniCRAN")
  
  Sys.setenv("miniCRAN.mock.download" = old.env)
  
}
