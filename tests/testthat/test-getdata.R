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

test_that("the raw data is imported correctly", {
  yyyymm_start <- 201403
  yyyymm_end <- 201409
  cache_json <- FALSE
  verbose <- FALSE

  reports_info <- brazilianbanks:::download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = cache_json)
  qtrs <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

  raw_data <- brazilianbanks:::downloads_qtr_data(qtrs = qtrs,
                                                  reports_info = reports_info,
                                                  cache_json = cache_json,
                                                  verbose = verbose)

  expect_equal(length(raw_data), 4)
  expect_equal(names(raw_data), c("cadastroData", "infoData", "dadosData", "relatoriosData"))
  expect_gte(length(raw_data[["cadastroData"]]), 1)
  expect_gte(length(raw_data[["infoData"]]), 1)
  expect_gte(length(raw_data[["dadosData"]]), 1)
  expect_gte(length(raw_data[["relatoriosData"]]), 1)
})

test_that("there is at least one report with income stataement data", {
  qtrs <- brazilianbanks::all_available_quarters()
  yyyymm_start <- yyyymm_end <- sample(qtrs, 1)

  raw_data <- brazilianbanks:::downloads_qtr_data(qtrs = qtrs,
                                                  reports_info = reports_info,
                                                  cache_json = cache_json,
                                                  verbose = verbose)

  reports <- lapply(raw_data$relatoriosData, function(x) list(id = as.character(x$id), Report_name = x$ni, ifd = x$ifd)) %>%
    dplyr::bind_rows(.id = "File")

  income_statement_vars <- reports %>% dplyr::filter(stringr::str_detect(Report_name, "Income Statement"))

  expect_gte(nrow(income_statement_vars), 1)
  expect_equal(ncol(income_statement_vars), 4)
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

