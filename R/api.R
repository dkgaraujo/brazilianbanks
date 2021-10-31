#' Retrieves bank-level statistics.
#'
#' @param yyyymm_start Start quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param yyyymm_end End quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param sources Which data sources of bank-level data to download. Currently only "IF.data", the bank-level dtaset made publicly available by the Central Bank of Brazil is available.
#' @param verbose Whether the function must inform the user as it progresses.
#' @return A `tibble` with the bank-level time series in a tidy format.
#' @examples
#' \dontrun{
#' get_bank_stats(yyyymm_start = 202103, yyyymm_end = 202106, sources = "IF.data", verbose = FALSE)
#' }
#' @export
get_bank_stats <- function(
  yyyymm_start, yyyymm_end,
  sources = c("IF.data"),
  cache_json = TRUE,
  banks_only = TRUE,
  verbose = TRUE) {
  # TODO: include an option to cache the data
  # TODO: change API for a single `get_data` function, where the data source
  #       would be specified as a parameter, eg `source = "IFdata"`, thus enabling
  #       future data sources such as Pillar 3 information to be added without
  #       changing the API.
  if (verbose) {
    print("Getting the dataset...")
  }

  if ("IF.data" %in% sources) {
    quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
    results <- list()
    for (qtr in quarters) {
      if (verbose) {
        print(paste("Getting results for", qtr))
      }
      results[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 1, cache_json = cache_json)
      #results2[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 2, cache_json = cache_json)
    }
  }

  results <- results %>% prepare_data(banks_only = banks_only)

  if (verbose) {
    print("`get_data` is completed!")
  }
  return(results)
}
