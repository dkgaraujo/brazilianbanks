#' Retrieves bank-level statistics.
#'
#' @param yyyymm_start Start calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param yyyymm_end End calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param sources Which data sources of bank-level data to download. Currently only "IF.data", the bank-level dataset made publicly available by the Central Bank of Brazil is available.
#' @param banks_only TRUE. Whether only the observations related to banks should be kept.
#' @param include_growthrate TRUE. Whether the quarter-on-quarter growth rate for the numeric variables should be calculated.
#' @param include_lag TRUE. Whether the numeric variables should also be lagged.
#' @param cache_json TRUE. Whether the JSON files with the raw data should be cached locally.
#' @param verbose Whether the function must inform the user as it progresses.
#' @return A `tibble` with the bank-level time series in a tidy format.
#' @examples
#' \dontrun{
#' get_bank_stats(yyyymm_start = 202003, yyyymm_end = 202106, sources = "IF.data", verbose = FALSE)
#' }
#' @export
get_bank_stats <- function(
  yyyymm_start, yyyymm_end,
  sources = c("IF.data"),
  banks_only = TRUE,
  include_growthrate = TRUE,
  include_lag = TRUE,
  cache_json = TRUE,
  verbose = TRUE) {

  if (verbose) {
    print("Getting the dataset...")
  }

  results <- list()

  if ("IF.data" %in% sources) {
    .GlobalEnv$cache_json <- cache_json
    .GlobalEnv$var_codes <- prepares_var_names(yyyymm_start, yyyymm_end)

    quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)


    for (qtr in quarters) {
      if (verbose) {
        print(paste("Getting results for", qtr))
      }
      results[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 1)
    }
  }

  if (verbose) {
    print("`get_data` is now augmenting the dataset.")
  }

  results <- results %>%
    prepare_data(banks_only = banks_only) %>%
    loans_share_by_risk_level() %>%
    loans_share_by_geographical_region() %>%
    excess_capital(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

  results <- results %>%
    dplyr::left_join(download_GDP_data(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end), by = "Quarter")

  if ("Segmentation_Total.Exposure.or.Total.Assets" %in% colnames(results)) {
    results <- results %>%
      dplyr::mutate(SizeByGDP = Segmentation_Total.Exposure.or.Total.Assets / AnnualGDP / 1000000)
  } else {
    results <- results %>%
      dplyr::mutate(SizeByGDP = Summary_Total.Assets / AnnualGDP / 1000000)
  }


  if (include_growthrate) {
    results <- results %>%
      growthrate()
  }

  if (include_lag) {
    results <- results %>%
      lag_numericvars()
  }

  if (verbose) {
    print("`get_data` is completed!")
  }
  return(results)
}
