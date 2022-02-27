test_that("the IF.data values for a specific quarter and consolidation type are downloaded", {
  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  IFdata_constype_1 <- download_IFdata_bankdata(yyyymm = yyyymm, 1, cache_json = FALSE)
  expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
  expect_type(IFdata_constype_1, "list")
  expect_s3_class(IFdata_constype_1, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  IFdata_constype_2 <- download_IFdata_bankdata(yyyymm = yyyymm, 2, cache_json = FALSE)
  expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
  expect_type(IFdata_constype_1, "list")
  expect_s3_class(IFdata_constype_1, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  IFdata_constype_3 <- download_IFdata_bankdata(yyyymm = yyyymm, 3, cache_json = FALSE)
  expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
  expect_type(IFdata_constype_1, "list")
  expect_s3_class(IFdata_constype_1, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  IFdata_constype_4 <- download_IFdata_bankdata(yyyymm = yyyymm, 4, cache_json = FALSE)
  expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
  expect_type(IFdata_constype_1, "list")
  expect_s3_class(IFdata_constype_1, "data.frame")
})

test_that("information on financial institutions for each quarter are downloadable", {

  cache_json <- FALSE

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  bank_info_1 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 1, cache_json = cache_json)
  expect_gte(ncol(bank_info_1), 28)
  expect_gt(nrow(bank_info_1), 10)
  expect_type(bank_info_1, "list")
  expect_s3_class(bank_info_1, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  bank_info_2 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 2, cache_json = cache_json)
  expect_gte(ncol(bank_info_2), 28)
  expect_gt(nrow(bank_info_2), 10)
  expect_type(bank_info_2, "list")
  expect_s3_class(bank_info_2, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  bank_info_3 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 3, cache_json = cache_json)
  expect_gte(ncol(bank_info_3), 28)
  expect_gt(nrow(bank_info_3), 10)
  expect_type(bank_info_3, "list")
  expect_s3_class(bank_info_3, "data.frame")

  yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
  bank_info_4 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 4, cache_json = cache_json)
  expect_gte(ncol(bank_info_4), 28)
  expect_gt(nrow(bank_info_4), 10)
  expect_type(bank_info_4, "list")
  expect_s3_class(bank_info_4, "data.frame")
})

# test_data("the quarter-level results are glued together properly", {
#   results <- list()
#
#   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
#   results[[1]] <- download_IFdata_values(yyyymm = yyyymm, consolidation_type = 1, cache_json = FALSE)
#
#   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
#   results[[2]] <- download_IFdata_values(yyyymm = yyyymm, consolidation_type = 1, cache_json = FALSE)
#
#   results <- prepare_data(df_list = results, banks_only = TRUE)
# })

# test_that("the function download IF.data values and compiling them is working", {
#   yyyymm <- 201803
#   consolidation_type <- 1
#   cache_json <- TRUE
#   ifdata <- download_IFdata_values(yyyymm, consolidation_type, cache_json)
#
# })

# test_that("the amount of excess capital is calculated appropriately", {
#
# })
