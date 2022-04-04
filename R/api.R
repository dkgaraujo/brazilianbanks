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
  banks_only = TRUE,
  include_growthrate = TRUE,
  include_lag = FALSE,
  cache_json = TRUE,
  verbose = TRUE) {

  if (verbose) {
    print("Getting the dataset...")
  }

  reports_info <- download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = cache_json)
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

  # download all data for the selected quarters
  cadastroData <- list()
  infoData <- list()
  dadosData <- list()
  relatoriosData <- list()
  for (qtr in quarters) {
    reports_qtr <- reports_info[[which(quarters == qtr)]]
    file_names <- Reduce(c, Reduce(c, lapply(reports_qtr$files, function(x) x['f'])))
    for (file_name in file_names) {
      if (grepl("/cadastro", file_name))
        cadastroData[[file_name]] <- jsonlite::read_json(find_IFdata_json(file_name = file_name, cache_json = cache_json)) %>%
          dplyr::bind_rows()

      if (grepl("/info", file_name))
        infoData[[file_name]] <- jsonlite::read_json(find_IFdata_json(file_name = file_name, cache_json = cache_json))

      if (grepl("/dados", file_name))
        dadosData[[file_name]] <- jsonlite::read_json(find_IFdata_json(file_name = file_name, cache_json = cache_json))

      if (grepl("/trel", file_name))
        relatoriosData[[file_name]] <- jsonlite::read_json(find_IFdata_json(file_name = file_name, cache_json = cache_json))
    }
  }

  # transform the data into more efficient / fluid formats to work with
  names(cadastroData) <- names(cadastroData) %>% stringr::str_extract(pattern = "(?<=_)[^_]*(?=\\.)")
  cadastroData <- cadastroData %>% dplyr::bind_rows(.id = "InstType")

  names(infoData) <- names(infoData) %>% substr(start = 1, stop = 6)
  infoData <- infoData %>% lapply(function(x) dplyr::bind_rows(x)) %>% dplyr::bind_rows(.id = "Quarter")

  names(dadosData) <- names(dadosData) %>% stringr::str_extract(pattern = "(?<=s)[^s]*(?=\\.)")
  dadosData <- dadosData %>% lapply(function(x) x$values %>%
    lapply(function(x) {
      df <- cbind(x$e, do.call(rbind.data.frame, x$v))
      colnames(df) <- c("FinInst", "info_id", "value")
      return(df)
    }) %>% dplyr::bind_rows()) %>%
    dplyr::bind_rows(.id = "QuarterType") %>%
    tidyr::separate(col = "QuarterType", into = c("Quarter", "DataRptType"), sep = "_")

  names(relatoriosData) <- names(relatoriosData) %>% stringr::str_extract(pattern = "(?<=l)[^s]*(?=\\.)")
  # `cols_df` represents the variables of the columns with actual data on them
  cols_df2 <- lapply(relatoriosData, function(x) Reduce(rbind, getColsFolhas(x$c))) %>%
    Reduce(rbind, .) %>%
    data.frame() %>%
    dplyr::distinct() %>%
    dplyr::select(-c(co, sc, nac))
  rownames(cols_df) <- NULL

  cols_df <- lapply(relatoriosData, function(x) getColsFolhas(x$c)) %>%
    lapply(function(x) lapply(x, function(xx) c("id" = xx$id, "ifd" = xx$ifd, "ip" = xx$ip)) %>% dplyr::bind_rows()) %>%
    dplyr::bind_rows(.id = "QuarterRpt") %>%
    tidyr::separate(col = "QuarterRpt", into = c("Quarter", "Report"), sep = "_") %>%
    dplyr::left_join(infoData, by = c("Quarter" = "Quarter", "ifd" = "id")) %>%
    dplyr::rename(column_name = ni)

  # `parent_cols_df` represents the columns that are only "parent" columns of their subdivisions
  # these parent columns must be fetched from the data for their name, to couple with the subdivision
  # column names (otherwise there would be a lot of information with the same column name).
  parent_cols_df <- lapply(relatoriosData, function(x) {
    xc <- x$c
    xc[which(xc %>% lapply(function(x) length(x$sc)) %>% Reduce(c, .) > 0)] %>%
      lapply(function(x) c("id" = x$id, "ifd" = x$ifd)) %>%
      dplyr::bind_rows()}) %>%
    dplyr::bind_rows(.id = "QuarterRpt") %>%
    tidyr::separate(col = "QuarterRpt", into = c("Quarter", "Report")) %>%
    dplyr::left_join(infoData, by = c("Quarter" = "Quarter", "ifd" = "id")) %>%
    dplyr::rename(parent_name = ni) %>%
    dplyr::select(-c(td, lid, ty))

  all_data_info <- cols_df %>%
    dplyr::left_join(parent_cols_df,
                     by = c("Quarter" = "Quarter", "ip" = "id"),
                     suffix = c("_column", "_parent")) %>%
    dplyr::mutate(variable_name = paste(ifelse(is.na(parent_name), "", parent_name), column_name, sep = "__"))

  # data_from_cadastro <- cols_df %>% dplyr::filter(td == 1)
  # data_from_dados <- cols_df %>% dplyr::filter(td == 3)
  # Combine the information to match the dataset with variable names

  ###
  var_codes <- prepares_var_names(yyyymm_start, yyyymm_end, reports_info, cache_json)
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

  results <- list()

  for (qtr in quarters) {
    if (verbose) {
      print(paste("Getting results for", qtr))
    }
    results[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 1, var_codes, cache_json)
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

#' Lists all quarters for which there is data available.
#'
#' @inheritParams get_bank_stats
#' @return A vector with all the available quarters in the format YYYYMM.
#' @export
all_available_quarters <- function(cache_json) {
  json_data <- download_IFdata_reports_info(cache_json = cache_json)
  return(Reduce(c, lapply(json_data, function (x) x$dt)))
}
