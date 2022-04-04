# New ----------------------------
#' Utility that unnests information on nested columns from the original JSON format
getColsFolhas <- function(colArray) {
  list_cols <-
    lapply(colArray, function(x) {
      ifelse(length(x$sc) == 0,
             return(x),
             getColsFolhas(x$sc))
    }) %>%
    lapply(function(x) ifelse(length(x) == 1, return(x[[1]]), return(x)))
  return(list_cols)
}

# getColsParents <- function(colArray) {
#   list_cols <-
#     lapply(colArray, function(x) {
#       ifelse(length(x$sc) == 0,
#              return(x),
#              getColsFolhas(x$sc))
#     }) %>%
#     lapply(function(x) ifelse(length(x) == 1, return(x[[1]]), return(x)))
#   return(list_cols)
# }
# fetching and preparing the data -----------------------------------------


#' Downloads and prepares (eg, renaming columns, etc) IF.Data values for a specific quarter
#'
#' @param yyyymm The quarter for which the data will be downloaded.
#' @param consolidation_type One of the four consolidation types in IF.Data: 1 - Prudential conglomerates; 2 - Financial conglomerates; 3 - Financial Institutions; 4 - Foreign exchange institutions
#' @param cache_json Boolean. Whether or not will downloaded data be cached locally.
#' @return A `tibble` with the IF.Data values for that quarter.
download_IFdata_values <- function(yyyymm, consolidation_type, var_codes, cache_json) {

  # Downloads files
  df_values_1 <- download_IFdata_bankdata(yyyymm = yyyymm, consolidation_type = 1, cache_json)
  df_values_3 <- download_IFdata_bankdata(yyyymm = yyyymm, consolidation_type = 3, cache_json)
  df_values_4 <- download_IFdata_bankdata(yyyymm = yyyymm, consolidation_type = 4, cache_json)
  df_values <- rbind(df_values_1, df_values_3, df_values_4)
  rm(df_values_1, df_values_3, df_values_4)
  gc()
  df_bankinfo <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type = consolidation_type, cache_json = cache_json)

  # Prepare datasets
  df_values <- df_values %>%
    dplyr::mutate(Quarter = yyyymm_to_Date(yyyymm)) %>%
    dplyr::left_join(
      var_codes %>%
        dplyr::filter(Quarter == yyyymm) %>%
        dplyr::select(lid, var_names) %>%
        dplyr::distinct(),
      by = c("info_id" = "lid")
    ) %>%
    tidy_IFdata_values()

  # Merge datasets
  df_values <- df_values %>%
    dplyr::inner_join(df_bankinfo, by = c("FinInst" = "c0", "Quarter" = "Quarter")) %>%
    dplyr::rename(CompleteName = c2,
                  Institution.Type = c3,
                  Control.Type = c6,
                  Control.Type.Name = c7,
                  BR.State = c10,
                  BR.City = c11,
                  Segment = c12,
                  Number.Branches = c16,
                  Number.BankServiceOutposts = c17) %>%
    dplyr::mutate(Name = stringr::str_replace(CompleteName, stringr::fixed(" - PRUDENCIAL"), ""))
  return(df_values)
}

#' Downloads IF.Data data for a given quarter and consolidation type
#'
#' @inheritParams download_IFdata_values
#' @return A data.frame with three columns: the financial institution identifier (FinInst), the variable code (info_id) and its value (value).
download_IFdata_bankdata <- function(yyyymm, consolidation_type, cache_json) {
  # Virtually all of the data are located in files with type = c(1, 3)
  # For some reason, it appears that consolidation type == 2 (financial conglomerates)
  # are represented in this file only by (consolidation) type = 3.
  file_name <- paste0("dados", yyyymm, "_", consolidation_type, ".json")
  json_path <- find_IFdata_json(yyyymm, file_name, cache_json = cache_json)
  json_data <- RJSONIO::fromJSON(json_path)$values

  df <- json_data %>%
    lapply(function(x) {
      df <- do.call(rbind.data.frame, x$v)
      colnames(df) <- c("info_id", "value")
      return(df)
    })
  names(df) <- Reduce(c, lapply(json_data, function(x) x$e))
  df <- df %>%
    dplyr::bind_rows(.id = "FinInst")
  return(df)
}

#' Downloads information on the IF.Data variable names
#'
#' @inheritParams download_IFdata_values
download_IFdata_variables <- function(yyyymm, cache_json) {
  # column 'id' of 'info' is the same as column 'ifd' of the 'trel' variables
  file_name <- paste0("info", yyyymm, ".json")
  json_path <- find_IFdata_json(yyyymm, file_name, cache_json = cache_json)
  json_info <- RJSONIO::fromJSON(json_path)

  info_names <- do.call(rbind.data.frame, json_info)
  return(info_names)
}

download_IFdata_bankinfo <- function(yyyymm, consolidation_type = 1, cache_json) {
  file_name <- paste0("cadastro", yyyymm, "_100", consolidation_type + 3, ".json")
  json_path <- find_IFdata_json(yyyymm, file_name, cache_json = cache_json)
  json_data <- RJSONIO::fromJSON(json_path)

  df_bankinfo <- json_data %>%
  lapply(function(x) x %>% as.list() %>% data.frame()) %>%
  Reduce(rbind, .) %>%
  dplyr::mutate(Quarter = yyyymm_to_Date(yyyymm)) %>%
  dplyr::relocate(Quarter, .after = c0)

  return(df_bankinfo)
}

tidy_IFdata_values <- function(df_values) {
  df_values <- df_values %>%
    dplyr::select(-info_id) %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = var_names, values_from = value)
  return(df_values)
}

#' Organises the relationship between variable codes and human-readable names
#'
#' @inheritParams get_bank_stats
#' @param reports_info A data.frame with information from all available IF.data reports for the selected dates
prepares_var_names <- function(yyyymm_start, yyyymm_end, download_IFdata_reportsreports_info, cache_json) {
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
  variables <- list()
  for (qtr in quarters) {
    variables[[as.character(qtr)]] <- download_IFdata_variables(qtr, cache_json = cache_json) %>%
      dplyr::rename(var_names = ni) %>%
      #dplyr::mutate(var_names = gsub("\\n.*", "", var_names, fixed = FALSE)) %>%
      #dplyr::mutate(var_names = gsub(" \\n", ": ", var_names, fixed = FALSE)) %>%
      dplyr::mutate(var_names = ifelse(var_names == "", lid, var_names))
  }
  variables <- variables %>%
    dplyr::bind_rows(.id = "Quarter")

  df_reports <- reports_info %>%
    lapply(function(x) reads_reports_json(x)) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(variables, by = c("c" = "id", "Quarter" = "Quarter")) %>%
    dplyr::select(-c(a, d, td, ty)) %>%
    dplyr::mutate(var_names = paste(ni, var_names, sep = "_"))

  return(df_reports)
}

#' Downloads information about the IF.Data reports starting from yyyymm_start and the last available one if yyyymm_end is NULL
#'
#' @inheritParams get_bank_stats
#' @return List with
download_IFdata_reports <- function(yyyymm_start, yyyymm_end = NULL, cache_json) {
  json_data <- download_IFdata_reports_info(cache_json = cache_json)
  first_idx <- which_idx(yyyymm_start)
  last_idx <- ifelse(is.null(yyyymm_end), length(json_data), which_idx(yyyymm_end))
  return(json_data[first_idx:last_idx])
}

#' Calculated the index in the list of reports coming from the BCB API
#' @return A scalar that indexes the list of reports
which_idx <- function(yyyymm) {
  y <- as.numeric(substr(yyyymm, 1, 4))
  qtr <- as.numeric(substr(yyyymm, 5, 6))
  idx <- (y - 2000) * 4 + (qtr / 3)
  return(idx)
}

#' Downloads the information about all available dates directly from the BCB API
#'
#' @inheritParams get_bank_stats
#' @return List with the full result from the BCB IF.data "relatorios" API endpoint.
download_IFdata_reports_info <- function(cache_json) {
  json_path <- find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = cache_json)
  return(RJSONIO::fromJSON(json_path))
}

    # download_IFdata_reports <- function(yyyymm_start, cache_json) {
    #   json_path <- find_IFdata_json(yyyymm = NULL, file_name = "relatorios", cache_json = cache_json)
    #   json_data <- RJSONIO::fromJSON(json_path)
    #
    #   # Finds the first index of the `relatorios` file that
    #   # will be used (otherwised it will unnecessarily
    #   # take up memory with databased that will not be used).
    #   # The first index, ie json_data[[1]] refers to March 2020.
    #
    #   first_idx <- (round(yyyymm_start / 100, 0) - 2000) * 4 + 1
    #   last_idx <- length(json_data)
    #   json_data <- json_data[first_idx:last_idx]
    #
    #   # * * *
    #
    #   return(json_data)
    #  }

reads_reports_json <- function(df_reports_element) {

  files_subelement <- df_reports_element$files %>%
    dplyr::bind_rows() %>%
    tidyr::separate(col = f, into = c("Quarter", "report"), sep = "/") %>%
    dplyr::select(-sel) %>%
    dplyr::filter(grepl("trel", report)) %>%
    dplyr::mutate(name_trel = names(trel)) %>%
    dplyr::filter(name_trel %in% c(
      "ni",
      #"ci", # longer version of the template name
      "ifd",
      "c",
      "ip"
      )) %>%
    tidyr::pivot_wider(
      names_from = name_trel,
      values_from = trel
      ) %>%
    dplyr::select(-report) %>%
    dplyr::mutate(
      ni = sapply(ni, "[", 1),
      #ci = sapply(ci, "[", 1),
      ifd = sapply(ifd, "[", 1)
      ) %>%
    tidyr::unnest(c) %>%
    dplyr::mutate(c = sapply(c, "[[", 3))

  return(files_subelement)
}


# calculations on the variables -------------------------------------------

#' Adds columns with (one period) lagged information for numeric columns
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
lag_numericvars <- function(dataframe) {
  dataframe <- dataframe %>%
    dplyr::group_by(FinInst) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect:::where(is.numeric),
        .fns = list(lag_var = ~ dplyr::lag(.x, order_by = Quarter)),
        .names = "lag_{col}"
      )
    ) %>%
    dplyr::ungroup()
  return(dataframe)
}

#' Adds columns with quarter-on-quarter growth for numeric columns
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
growthrate <- function(dataframe) {
  dataframe <- dataframe %>%
    dplyr::group_by(FinInst) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect:::where(is.numeric),
        .fns = list(QoQ_growth = ~ ifelse(
          is.na(dplyr::lag(.x, order_by = Quarter)) |
            dplyr::lag(.x, order_by = Quarter) == 0,
          NA,
          (.x / dplyr::lag(.x, order_by = Quarter)) - 1)),
        .names = "QoQ_growth_rate_{col}"
      )
    )
  return(dataframe)
}

#' Adds columns with the loan share of each risk level
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
loans_share_by_risk_level <- function(dataframe) {
  total_var <- "Loans.Portfolio.by.risk.level_Grand.Total"
  dataframe <- dataframe %>%
    dplyr::mutate(
      loan_share_risk_AA = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_AA / .data[[total_var]], NA),
      loan_share_risk_A = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_A / .data[[total_var]], NA),
      loan_share_risk_B = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_B / .data[[total_var]], NA),
      loan_share_risk_C = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_C / .data[[total_var]], NA),
      loan_share_risk_D = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_D / .data[[total_var]], NA),
      loan_share_risk_E = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_E / .data[[total_var]], NA),
      loan_share_risk_F = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_F / .data[[total_var]], NA),
      loan_share_risk_G = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_G / .data[[total_var]], NA),
      loan_share_risk_H = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.risk.level_H / .data[[total_var]], NA)
    )
  return(dataframe)
}

#' Adds columns with the loan share of each geographical region
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @return A `tibble` with the bank-level information plus the added columns.
loans_share_by_geographical_region <- function(dataframe) {
  total_var <- "Loans.Portfolio.by.geographical.region_Grand.Total"
  dataframe <- dataframe %>%
    dplyr::mutate(
      loan_share_geo_North = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_North / .data[[total_var]], NA),
      loan_share_geo_Northeast = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Northeast / .data[[total_var]], NA),
      loan_share_risk_geo_CenterWest = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Center.west / .data[[total_var]], NA),
      loan_share_risk_geo_Southeast = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Southest / .data[[total_var]], NA),
      loan_share_risk_geo_South = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_South / .data[[total_var]], NA),
      loan_share_risk_geo_overseas = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Overseas.Loans.Total / .data[[total_var]], NA),
      loan_share_risk_geo_notinformed = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Local.not.Informed / .data[[total_var]], NA)
    )
  return(dataframe)
}

#' Adds a column with the stable proportionality segment
#'
#' @param dataframe A `tibble` containing the bank-level information.
#' @param last_segment Boolean. If TRUE, the stable segmentation is taken from the last period. If FALSE, from the first period.
#' @return A `tibble` with the bank-level information plus the added stable proportionality segment column.
stable_segmentation <- function(dataframe, last_segment = TRUE) {
  # TODO.......
  return(dataframe)
}

#' Adds a column with each firm's D-SIB status (ie, whether they are one of the domestic systemically important banks) and excess capital
#'
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

  # OBS.: Currently, the identification of D-SIBs is explicit.
  # Ideally it should be automatic by following the regulatory definition.
  dataframe <- dataframe %>%
    dplyr::mutate(
      DSIB = ifelse(FinInst %in% c(
        1000080075, # Bradesco
        1000080099, # Itaú
        1000080185, # Santander
        1000080738, # Caixa
        1000080329  # Banco do Brasil
    ), 1, 0)) %>%
    dplyr::left_join(min_K_req, by = "Quarter")

  # Now the excess capital is calculated
  dataframe <- dataframe %>%
    dplyr::rename(
      CET1_ratio = Capital.Information_Common.Equity.Tier.I.Ratio...k.....a.....i.,
      Tier1_ratio = Capital.Information_Tier.I.Capital.Ratio...l.....c.....i.,
      Total_Capital_ratio = Capital.Information_Regulatory.Capital.Ratio...m.....e.....i.
    ) %>%
    dplyr::mutate(
      CET1_above_min = CET1_ratio - CET1_req,
      Tier1_above_min = Tier1_ratio - Tier1_req,
      TotalCap_above_min = Total_Capital_ratio - Total_Capital_req,
      DSIB_effective_req = DSIB_req * DSIB,
      CET1_req_buffers = CET1_req + CCoB + effectiveCCyB + DSIB_effective_req,
      CET1_above_buffers = CET1_ratio - CET1_req_buffers
    )
  return(dataframe)
}

#' Returns a look-up table with the capital requirements for each quarter
#'
#' @inheritParams get_bank_stats
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
