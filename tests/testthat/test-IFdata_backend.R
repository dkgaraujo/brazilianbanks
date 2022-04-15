# test_that("the BCB API `relatorios` endpoint still works", {
#   cache_json <- FALSE
#   reports <- download_IFdata_reports_info(cache_json = cache_json)
#   expect_gte(length(reports), 87) # 87 is the number of reports from March 2000 to Sep 2021 (latest as of development)
#   expect_type(reports, "list")
# })
#
# test_that("the index for a given date in the full list of available reports is calculated correctly", {
#   yyyymm <- sample(c(201806, 202003, 202106), 1)
#   idx <- which_idx(yyyymm)
#   reports <- download_IFdata_reports_info(cache_json = FALSE)
#   expect_equal(reports[idx][[1]]$dt, yyyymm)
# })
#
# test_that("only the reports for the selected dates are selected", {
#   yyyymm_start <- 201903
#   yyyymm_end <- 202103
#   cache_json <- FALSE
#   selected_quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
#   reports_info <- download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = cache_json)
#   downloaded_quarters <- Reduce(c, lapply(reports_info, function(x) x$dt))
#   expect_equal(downloaded_quarters, selected_quarters)
# })
#
# test_that("the variable names are properly prepared", {
#   yyyymm_start <- 201803
#   yyyymm_end <- 202106
#   var_n <- prepares_var_names(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end, cache_json = FALSE)
#   expect_equal(ncol(var_n), 8)
#   expect_equal(length(unique(var_n$Quarter)), length(all_quarters_between(yyyymm_start, yyyymm_end)))
#   expect_equal("tbl_df" %in% class(var_n), TRUE)
# })
#
# test_that("variable names are properly downloaded", {
#   yyyymm_start <- 201803
#   yyyymm_end <- 202106
#   quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
#   qtr <- sample(quarters, 1)
#   cache_json <- FALSE
#   var_names <- download_IFdata_variables(yyyymm = qtr, cache_json = cache_json)
#   expect_equal(colnames(var_names), c("id", "n", "ni", "d", "di", "a", "td", "lid", "ty"))
#   expect_gte(ncol(var_names), 9)
#   expect_gt(nrow(var_names), 100)
#   expect_type(var_names, "list")
#   expect_s3_class(var_names, "data.frame")
# })
#
# # test_that("the IF.data values for a specific quarter and consolidation type are downloaded", {
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   IFdata_constype_1 <- download_IFdata_bankdata(yyyymm = yyyymm, 1, cache_json = FALSE)
# #   expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
# #   expect_type(IFdata_constype_1, "list")
# #   expect_s3_class(IFdata_constype_1, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   IFdata_constype_2 <- download_IFdata_bankdata(yyyymm = yyyymm, 2, cache_json = FALSE)
# #   expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
# #   expect_type(IFdata_constype_1, "list")
# #   expect_s3_class(IFdata_constype_1, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   IFdata_constype_3 <- download_IFdata_bankdata(yyyymm = yyyymm, 3, cache_json = FALSE)
# #   expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
# #   expect_type(IFdata_constype_1, "list")
# #   expect_s3_class(IFdata_constype_1, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   IFdata_constype_4 <- download_IFdata_bankdata(yyyymm = yyyymm, 4, cache_json = FALSE)
# #   expect_equal(colnames(IFdata_constype_1), c("FinInst", "info_id", "value"))
# #   expect_type(IFdata_constype_1, "list")
# #   expect_s3_class(IFdata_constype_1, "data.frame")
# # })
# #
# # test_that("information on financial institutions for each quarter are downloadable", {
# #
# #   cache_json <- FALSE
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   bank_info_1 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 1, cache_json = cache_json)
# #   expect_gte(ncol(bank_info_1), 28)
# #   expect_gt(nrow(bank_info_1), 10)
# #   expect_type(bank_info_1, "list")
# #   expect_s3_class(bank_info_1, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   bank_info_2 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 2, cache_json = cache_json)
# #   expect_gte(ncol(bank_info_2), 28)
# #   expect_gt(nrow(bank_info_2), 10)
# #   expect_type(bank_info_2, "list")
# #   expect_s3_class(bank_info_2, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   bank_info_3 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 3, cache_json = cache_json)
# #   expect_gte(ncol(bank_info_3), 28)
# #   expect_gt(nrow(bank_info_3), 10)
# #   expect_type(bank_info_3, "list")
# #   expect_s3_class(bank_info_3, "data.frame")
# #
# #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# #   bank_info_4 <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = 4, cache_json = cache_json)
# #   expect_gte(ncol(bank_info_4), 27)
# #   expect_gt(nrow(bank_info_4), 10)
# #   expect_type(bank_info_4, "list")
# #   expect_s3_class(bank_info_4, "data.frame")
# # })
# #
# #
# #
#
# #
# # # test_that("the quarter-level results are glued together properly", {
# # #   results <- list()
# # #
# # #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# # #   results[[1]] <- download_IFdata_values(yyyymm = yyyymm, consolidation_type = 1, cache_json = FALSE)
# # #
# # #   yyyymm <- sample(c(2017, 2018, 2019, 2020), 1) * 100 + sample(c(3, 6, 9, 12), 1)
# # #   results[[2]] <- download_IFdata_values(yyyymm = yyyymm, consolidation_type = 1, cache_json = FALSE)
# # #
# # #   results <- prepare_data(df_list = results, banks_only = TRUE)
# # # })
# #
# # # test_that("the function download IF.data values and compiling them is working", {
# # #   yyyymm <- 201803
# # #   consolidation_type <- 1
# # #   cache_json <- TRUE
# # #   ifdata <- download_IFdata_values(yyyymm, consolidation_type, cache_json)
# # #
# # # })
# #
# # # test_that("the amount of excess capital is calculated appropriately", {
# # #
# # # })
