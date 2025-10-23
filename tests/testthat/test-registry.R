test_that('registry loads', {
  x <- list_datasets()
  expect_s3_class(x, 'tbl_df')
})
