test_that("the main function works as a whole for a single quarter without any adjustments", {

  qtrs <- brazilianbanks::all_available_quarters()
  yyyymm_start <- yyyymm_end <- sample(qtrs, 1)

  dataset <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = FALSE,
    include_growthrate = FALSE,
    cache_json = FALSE,
    verbose = FALSE)

  expect_gte(nrow(dataset), 10)
})

test_that("the main function works as a whole for a single quarter even with the income data adjustment turned on", {
  # the income data adjustment depends on data from other quarters,
  # so it should recognise it has only one quarter and skip the adjustment

  qtrs <- brazilianbanks::all_available_quarters()
  yyyymm_start <- yyyymm_end <- sample(qtrs, 1)

  dataset_with_adj <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = TRUE,
    include_growthrate = FALSE,
    cache_json = FALSE,
    verbose = FALSE)

  dataset_without_adj <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = FALSE,
    include_growthrate = FALSE,
    cache_json = FALSE,
    verbose = FALSE)

  expect_equal(dataset_with_adj, dataset_without_adj)
})

test_that("the main function works as a whole for a single quarter even with the growth rate adjustment", {
  # the growth rate adjustment depends on data from other quarters,
  # so it should recognise it has only one quarter and skip the adjustment

  qtrs <- brazilianbanks::all_available_quarters()
  yyyymm_start <- yyyymm_end <- sample(qtrs, 1)

  dataset_with_adj <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = FALSE,
    include_growthrate = TRUE,
    cache_json = FALSE,
    verbose = FALSE)

  dataset_without_adj <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = FALSE,
    include_growthrate = FALSE,
    cache_json = FALSE,
    verbose = FALSE)

  expect_equal(dataset_with_adj, dataset_without_adj)
})


test_that("the main function works as a whole for a single quarter even with all adjustments", {

  qtrs <- brazilianbanks::all_available_quarters()
  yyyymm_start <- yyyymm_end <- sample(qtrs, 1)

  dataset <- get_bank_stats(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    banks_only = FALSE,
    adjust_income_data = TRUE,
    include_growthrate = TRUE,
    cache_json = FALSE,
    verbose = FALSE)

  expect_gte(nrow(dataset), 10)
})
