# Copyright 2022 Douglas Kiarelly Godoy de Araujo

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#   http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Downloads information about the IF.Data reports in the selected dates or up to the last available date if `yyyymm_end` is `NULL`
#'
#' @inheritParams get_bank_stats
#' @return List with the reports available for the period between the desired quarters
download_IFdata_reports <- function(yyyymm_start, yyyymm_end = NULL, cache_json) {
  json_data <- download_IFdata_reports_info(cache_json = cache_json)
  first_idx <- which_idx(yyyymm_start)
  last_idx <- ifelse(is.null(yyyymm_end), length(json_data), which_idx(yyyymm_end))
  return(json_data[first_idx:last_idx])
}

#' Downloads the information about all available dates directly from the BCB website or from cached files
#'
#' @inheritParams get_bank_stats
#' @return List with the full result from the BCB IF.data "relatorios" API endpoint.
download_IFdata_reports_info <- function(cache_json) {
  json_path <- find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = cache_json)
  return(RJSONIO::fromJSON(json_path))
}

#' Retrieves column information from the JSON files provided by the BCB, especially designed to deal with its nested structured
#'
#' @param x The list of column (or subcolums) information
#' @return A vector with the relevant information for each column
subcols <- function(x) {
  lapply(x, function(x)
    if (length(x$sc) == 0) {
      unlist(x)
    } else {
      subcols(x$sc)
    })
}


#' Returns the index in the list of reports coming from the BCB API
#'
#' @param yyyymm The quarter of interest in the format YYYYMM
#' @return A scalar that indexes the list of reports
which_idx <- function(yyyymm) {
  y <- as.numeric(substr(yyyymm, 1, 4))
  qtr <- as.numeric(substr(yyyymm, 5, 6))
  idx <- (y - 2000) * 4 + (qtr / 3)
  return(idx)
}

#' Adds a column with each firm's D-SIB status (ie, whether they are one of the domestic systemically important banks) and excess capital
#'
#' @inheritParams get_bank_stats
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
excess_capital <- function(dataframe, yyyymm_start, yyyymm_end) {
  # this function transforms the reference table for capital requirements
  # in a more appropriate lookup table for these requirements by quarter
  min_K_req <- min_capital_requirements_quarter(
    yyyymm_start = yyyymm_start,
    yyyymm_end = yyyymm_end,
    capital_requirements = capital_requirements
  )

  # OBS.: Currently, the identification of D-SIBs is hard-coded.
  # Ideally it should be automatic by following the regulatory definition (ie, exposure measure > 10% GDP).
  dataframe <- dataframe %>%
    dplyr::mutate(
      DSIB = ifelse(FinInst %in% c(1000080075, # Bradesco
                                   1000080099, # Itaú
                                   1000080185, # Santander
                                   1000080738, # Caixa
                                   1000080329),  # Banco do Brasil
                    1, 0)) %>%
    dplyr::left_join(min_K_req, by = "Quarter")

  # Now the excess capital is calculated
  dataframe <- dataframe %>%
    dplyr::mutate(
      CET1_above_min = Common_Equity_Tier_I_Ratio - CET1_req,
      Tier1_above_min = Tier_I_Capital_Ratio - Tier1_req,
      TotalCap_above_min = Regulatory_Capital_Ratio - Total_Capital_req,
      DSIB_effective_req = DSIB_req * DSIB,
      CET1_req_buffers = CET1_req + CCoB + effectiveCCyB + DSIB_effective_req,
      CET1_above_buffers = Common_Equity_Tier_I_Ratio - CET1_req_buffers
    )
  return(dataframe)
}

#' Transforms income statement variables to reflect only quarterly performance
#'
#' @inheritParams growthrate
#' @return A `tibble` with income statement variables reflecing only quarterly performance
adjust_income_statement_data <- function(dataframe) {
  # since the IF.data follows BR-GAAP accounting conventions that the income statement variables are cumulated every fiscal quarter, we need to "de-cumulate" them by subtracting Q1 and Q3 data from Q2 and Q4, respectively. This will yield a proper quarterly income statement, that in turn can be used later in trailing-window calculations, etc.
  income_statement_cols <- dataframe %>%
    # 91, 94 and 98 are the income statement report codes and
    # td == 3 filters out columns in these reports from the cadastro dataset
    dplyr::filter(Report_column %in% c(91, 94, 98) & td == 3) %>%
    dplyr::select(variable_name) %>%
    unique() %>%
    #dplyr::mutate(column_name = clean_col_names(column_name)) %>%
    c() %>%
    .[[1]]

  dataframe <- dataframe %>%
    dplyr::group_by(Financial_institution, lubridate::year(Quarter)) %>%
    dplyr::mutate(
      dplyr::across(
        income_statement_cols,
        .fns = list(income_statement_adj = ~ ifelse(lubridate::month(Quarter) %in% c(6, 12),
                                                    .x - lag(.x, order_by = Quarter),
                                                    .x)),
        .names = "{col}_qtr"
        )
    ) %>%
    dplyr::ungroup()
    # dplyr::mutate(ROAE_qtr = ifelse(lubridate::month(Quarter) %in% c(6, 12),
    #                          Net_Income - lag(Net_Income, order_by = Quarter),
    #                          Net_Income) / Equity,
    #        NII_OAE_qtr = ifelse(lubridate::month(Quarter) %in% c(6, 12),
    #                             Income_Statement__Net_Interest_Income__Net_Interest_Income - lag(Income_Statement__Net_Interest_Income__Net_Interest_Income, order_by = Quarter),
    #                             Income_Statement__Net_Interest_Income__Net_Interest_Income) / Equity,
    #        OOI_OAE_qtr = ifelse(lubridate::month(Quarter) %in% c(6, 12), Income_Statement__Other_Operating_Income_and_Expenses__Other_Operating_Income_and_Expenses - lag(Income_Statement__Other_Operating_Income_and_Expenses__Other_Operating_Income_and_Expenses, order_by = Quarter),                                Income_Statement__Other_Operating_Income_and_Expenses__Other_Operating_Income_and_Expenses)  / Equity) %>%
    # dplyr::ungroup() %>%
    # dplyr::group_by(Financial_institution) %>%
    # dplyr::mutate(ROAE = 100 * (ROAE_qtr +
    #                             dplyr::lag(ROAE_qtr, n = 1, order_by = Quarter) +
    #                             dplyr::lag(ROAE_qtr, n = 2, order_by = Quarter) +
    #                             dplyr::lag(ROAE_qtr, n = 3, order_by = Quarter)),
    #        NII_OAE = 100 * (NII_OAE_qtr +
    #                         dplyr::lag(NII_OAE_qtr, n = 1, order_by = Quarter) +
    #                         dplyr::lag(NII_OAE_qtr, n = 2, order_by = Quarter) +
    #                         dplyr::lag(NII_OAE_qtr, n = 3, order_by = Quarter)),
    #        OOI_OAE = 100 * (OOI_OAE_qtr +
    #                         dplyr::lag(OOI_OAE_qtr, n = 1, order_by = Quarter) +
    #                         dplyr::lag(OOI_OAE_qtr, n = 2, order_by = Quarter) +
    #                         dplyr::lag(OOI_OAE_qtr, n = 3, order_by = Quarter))) %>%
    # dplyr::ungroup()
  return(dataframe)
}

#' Adds columns with quarter-on-quarter growth for numeric variables of each bank
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
growthrate <- function(dataframe) {
  dataframe <- dataframe %>%
    dplyr::group_by(FinInst) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect:::where(is.numeric),
        .fns = list(QoQ_growth = ~ ifelse(is.na(dplyr::lag(.x, order_by = Quarter)) | dplyr::lag(.x, order_by = Quarter) == 0,
                                          NA, (.x / dplyr::lag(.x, order_by = Quarter)) - 1)),
        .names = "QoQ_growth_rate_{col}"
      )
    )
  return(dataframe)
}

#' Returns a look-up table with the capital requirements in Brazil for each quarter.
#' Numbers are valid for banks only so far only - especially credit unions and S5 have other values.
#'
#' @inheritParams get_bank_stats
#' @param capital_requirements A tibble with the regulatory capital requirements at each date
min_capital_requirements_quarter <- function(yyyymm_start, yyyymm_end, capital_requirements) {
  stopifnot(yyyymm_start > 201400)
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end) %>%
    yyyymm_to_Date()
  cols_capital <- colnames(capital_requirements)[-(1:2)]
  result <- data.frame(matrix(
    nrow = length(quarters),
    ncol = length(cols_capital)
  ))
  rownames(result) <- quarters
  colnames(result) <- cols_capital
  for (qtr in rownames(result)) {
    if (qtr < as.Date("2019-01-01")) {
      result[qtr,] <- capital_requirements %>%
        dplyr::filter(min_date >= qtr |  max_date >= qtr) %>%
        dplyr::filter(min_date == min(min_date)) %>%
        dplyr::select(-tidyselect::ends_with("date")) %>%
        as.data.frame()
    } else {
      result[qtr,] <- capital_requirements %>%
        dplyr::filter(min_date == as.Date("2019-01-01")) %>%
        dplyr::select(-tidyselect::ends_with("date")) %>%
        as.data.frame()
    }
  }
  result <- tibble::as_tibble(cbind(Quarter = as.Date(rownames(result)), result))
  return(result)
}
