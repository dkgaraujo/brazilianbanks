#' Retrieves bank-level statistics.
#'
#' @param yyyymm_start Start calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param yyyymm_end End calendar quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param sources Which data sources of bank-level data to download. Currently only "IF.data", the bank-level dtaset made publicly available by the Central Bank of Brazil is available.
#' @param cache_json TRUE. Whether the JSON files with the raw data should be cached locally.
#' @param banks_only TRUE. Whether only the observations related to banks should be kept.
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
  cache_json = TRUE,
  banks_only = TRUE,
  verbose = TRUE) {

  if (verbose) {
    print("Getting the dataset...")
  }

  if ("IF.data" %in% sources) {
    .GlobalEnv$var_codes <- prepares_var_names(yyyymm_start, yyyymm_end)

    quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

    results <- list()
    for (qtr in quarters) {
      if (verbose) {
        print(paste("Getting results for", qtr))
      }
      results[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 1, cache_json = cache_json)
    }
  }

  results <- results %>% prepare_data(banks_only = banks_only)

  if (verbose) {
    print("`get_data` is completed!")
  }
  return(results)
}
