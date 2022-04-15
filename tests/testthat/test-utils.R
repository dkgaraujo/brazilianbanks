# test_that("all quarters between two YYYYMM dates are calculated correctly", {
#   expect_equal(
#     all_quarters_between(yyyymm_start = 201803, yyyymm_end = 202106),
#     c(201803, 201806, 201809, 201812, 201903, 201906, 201909, 201912, 202003, 202006, 202009, 202012, 202103, 202106)
#     )
#   expect_equal(
#     all_quarters_between(yyyymm_start = 201801, yyyymm_end = 202106),
#     c(201803, 201806, 201809, 201812, 201903, 201906, 201909, 201912, 202003, 202006, 202009, 202012, 202103, 202106)
#   )
#   expect_equal(
#     all_quarters_between(yyyymm_start = 201803, yyyymm_end = 202108),
#     c(201803, 201806, 201809, 201812, 201903, 201906, 201909, 201912, 202003, 202006, 202009, 202012, 202103, 202106)
#   )
# })
#
# test_that("the IBGE API is working to download GDP data", {
#   gdp <- download_GDP_data(201812, 201903)
#   expect_type(gdp, "list")
#   expect_s3_class(gdp, "data.frame")
#   expect_equal(nrow(gdp), 2)
#   expect_equal(ncol(gdp), 2)
# })
