test_that("the code finds the JSON list of files from the BCB site", {
  json_path <- brazilianbanks:::find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = FALSE)
  expect_equal("https://www3.bcb.gov.br/ifdata/rest/relatorios", json_path)
  expect_equal(TRUE, RJSONIO::isValidJSON(json_path)) # weirdly, this endpoint passes RJSONIO validation, but not jsonlite validation.
})

test_that("IF.data data are findable for a reasonable range of dates", {
  yyyymm_start <- 201403
  yyyymm_end <- 202109

  num_qtrs <- length(brazilianbanks:::all_quarters_between(yyyymm_start = yyyymm_start,
                                                           yyyymm_end = yyyymm_end))

  num_qtrs_info <- length(brazilianbanks:::download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = FALSE))
  expect_equal(num_qtrs, num_qtrs_info)
})

test_that("the Brazilian GDP data is properly downloaded from IBGE", {
  yyyymm_start <- 201403
  yyyymm_end <- 202109

  gdp <- download_GDP_data(yyyymm_start = yyyymm_start,
                           yyyymm_end = yyyymm_end)

  quarters <- all_quarters_between(yyyymm_start = yyyymm_start,
                                   yyyymm_end = yyyymm_end)

  expect_equal(dim(gdp)[1], length(quarters))
  expect_equal(dim(gdp)[2], 2)
  expect_equal(colnames(gdp), c("Quarter", "AnnualGDP"))
  expect_gte(min(gdp$AnnualGDP %/% 1000000), 5) # ensure the order magnitude is broadly right
})

