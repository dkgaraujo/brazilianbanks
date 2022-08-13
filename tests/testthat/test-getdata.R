test_that("the code finds the JSON list of files from the BCB site", {
  json_path <- brazilianbanks:::find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = FALSE)
  expect_equal("https://www3.bcb.gov.br/ifdata/rest/relatorios", json_path)
  expect_equal(TRUE, RJSONIO::isValidJSON(json_path)) # weirdly, this endpoint passes RJSONIO validation, but not jsonlite validation.
})
