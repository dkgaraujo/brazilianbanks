test_that("the code finds the JSON list of files from the BCB site", {
  json_path <- brazilianbanks:::find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = FALSE)
  expect_equal("https://www3.bcb.gov.br/ifdata/rest/relatorios", json_path)
  expect_equal(TRUE, RJSONIO::isValidJSON(json_path)) # weirdly, this endpoint passes RJSONIO validation, but not jsonlite validation.
})

test_that("data are findable for a reasonable range of dates", {
  yyyymm_start <- 201403
  yyyymm_end <- 202109
  num_qtrs <- length(brazilianbanks:::all_quarters_between(yyyymm_start = yyyymm_start,
                                                           yyyymm_end = yyyymm_end))

  num_qtrs_info <- length(brazilianbanks:::download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = FALSE))
  expect_equal(num_qtrs, num_qtrs_info)
})
