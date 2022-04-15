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
    if (verbose) {
      print(paste("Preparing data for quarter", yyyymm_to_Date(qtr)))
    }
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
      # cols_df2 <- lapply(relatoriosData, function(x) Reduce(rbind, getColsFolhas(x$c))) %>%
      #   Reduce(rbind, .) %>%
      #   data.frame() %>%
      #   dplyr::distinct() %>%
      #   dplyr::select(-c(co, sc, nac))
      # rownames(cols_df) <- NULL

  cols_df <- lapply(relatoriosData, function(x) getColsFolhas(x$c)) %>%
    lapply(function(x) {
      list_cols <- list()
      for (col_idx in 1:length(x)) {
        if (length(x[[col_idx]]) == 11) {
          colname <- paste0("col", x[[col_idx]]$id)
          list_cols[[colname]] <- x[[col_idx]]
        } else {
          for (subcol_idx in 1:length(x[[col_idx]])) {
            colname <- paste0("col", x[[col_idx]][[subcol_idx]]$id)
            list_cols[[colname]] <- x[[col_idx]][[subcol_idx]]
          }
        }
      }
      return(list_cols)}) %>%
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
    dplyr::mutate(variable_name = ifelse(is.na(parent_name),
                                         column_name,
                                         paste(parent_name, column_name, sep = "__")) %>%
                    clean_col_names())

  dados <- dadosData %>%
    tibble::as_tibble() %>%
    dplyr::left_join(all_data_info %>% dplyr::select(Quarter, lid, variable_name) %>% dplyr::distinct(),
                     by = c("Quarter" = "Quarter", "info_id" = "lid")) %>%
    dplyr::mutate(Quarter = yyyymm_to_Date(Quarter)) %>%
    dplyr::filter(info_id < 0 | info_id > 30)

  dadosWide <- dados %>%
    dplyr::select(-info_id) %>%
    dplyr::distinct() %>%
    #dplyr::mutate(FinInst = factor(FinInst)) %>%
    dplyr::filter(!is.na(variable_name)) %>%
    tidyr::pivot_wider(names_from = variable_name, values_from = value, values_fn = mean) %>%
    dplyr::mutate(FinInst = as.character(FinInst))


  cadastroCols <- all_data_info %>%
    dplyr::filter(td == 1) %>%
    dplyr::select(lid, column_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(lid = paste0("c", lid),
                  column_name = clean_col_names(column_name)) %>%
    dplyr::arrange(lid)

  cadastroLong <- cadastroData %>%
    dplyr::mutate(FinInst = c0,
                  Quarter = yyyymm_to_Date(c1),
                  DataRptType = paste0(100, InstType)) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("c"), names_to = "cnames", values_to = "values") %>%
    dplyr::left_join(cadastroCols, by = c("cnames" = "lid"))

  # From here on, the code focuses only on Prudential and Financial conglomerate consolidation levels
  # the rationale for this is because they are the consolidation structures overseen by supervisors
  # but also in practice, it is hard to combine all IF.data information in a single instance for each bank
  # when conglomerates have more than one firm (which is totally natural, of course)
  cadastro <- cadastroLong %>%
    dplyr::filter(complete.cases(.) & InstType == 1004) %>%
    dplyr::select(-cnames) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "column_name", values_from = "values") %>%
    dplyr::select(-c(Code, Date)) %>%
    dplyr::mutate(#InstType = factor(InstType),
                  #FinInst = factor(FinInst),
                  #DataRptType = factor(DataRptType),
                  #TCB = factor(TCB),
                  #TD = factor(TD),
                  #TC = factor(TC),
                  #Segment = factor(Segment),
                  #Headquarters_._State = factor(Headquarters_._State),
                  #Headquarters_._City = factor(Headquarters_._City),
                  #SR = factor(SR),
                  #TI = factor(TI),
                  #Financial_Conglomerate = factor(Financial_Conglomerate),
                  #Prudential_Conglomerate = factor(Prudential_Conglomerate),
                  Branches = as.numeric(Branches),
                  Banking_Service_Outposts = as.numeric(Banking_Service_Outposts),
                  Last_Change_of_Segment = yyyymm_to_Date(Last_Change_on_Segment),
                  Conglomerate = factor(Conglomerate),
                  Financial_Conglomerate = ifelse(Financial_Conglomerate == "" & TCB %in% c("b3C", "b3S"), FinInst, Financial_Conglomerate),
                  Prudential_Conglomerate = ifelse(Prudential_Conglomerate == "" & TCB %in% c("b3C", "b3S"), FinInst, Prudential_Conglomerate)) %>%
    dplyr::select(-Last_Change_on_Segment) %>%
    dplyr::filter(InstType != 1008) # this InstType 1008 only occurs in 2014-03-31

  congl_data <- cadastro %>%
    # first, at the prudential conglomerate level
    dplyr::left_join(dadosWide %>% dplyr::filter(DataRptType == 1) %>% dplyr::select(-DataRptType), by = c("Quarter", "FinInst")) %>%
    purrr::discard(~ all(is.na(.x))) %>%
    # second, at the financial conglomerate level
    dplyr::left_join(dadosWide %>% dplyr::filter(DataRptType == 3) %>% dplyr::select(-DataRptType), by = c("Quarter" = "Quarter", "Financial_Conglomerate" = "FinInst"), suffix = c("", "_FinCongl")) %>%
    purrr::discard(~ all(is.na(.x)))
    dplyr::select(-InstType)

  ######

  if (banks_only) {
    congl_data <- congl_data %>% dplyr::filter(TCB %in% c("b1", "b2"))
  }

  congl_data <- congl_data %>%
    excess_capital(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)

  ### To avoid breaking the code while working offline

  congl_data <- congl_data %>%
    dplyr::left_join(download_GDP_data(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end), by = "Quarter")

  if ("Total_Exposure_or_Total_Assets" %in% colnames(congl_data)) {
    congl_data <- congl_data %>%
      dplyr::mutate(SizeByGDP = Total_Exposure_or_Total_Assets / AnnualGDP / 1000000)
  } else {
    results <- results %>%
      dplyr::mutate(SizeByGDP = Total_Assets / AnnualGDP / 1000000)
  }

  if (include_growthrate) {
    congl_data <- congl_data %>%
      growthrate()
  }

  if (include_lag) {
    all_data <- all_data %>%
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
all_available_quarters <- function(cache_json = TRUE) {
  json_data <- download_IFdata_reports_info(cache_json = cache_json)
  return(Reduce(c, lapply(json_data, function (x) x$dt)))
}
