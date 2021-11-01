
# fetching and preparing the data -----------------------------------------



download_IFdata_values <- function(yyyymm, consolidation_type, cache_json) {

  # Downloads files
  df_values_1 <- download_IFdata_bankdata(yyyymm = yyyymm, 1, cache_json)
  df_values_3 <- download_IFdata_bankdata(yyyymm = yyyymm, 3, cache_json)
  df_values_4 <- download_IFdata_bankdata(yyyymm = yyyymm, 4, cache_json)
  df_values <- rbind(df_values_1, df_values_3, df_values_4)
  rm(df_values_1, df_values_3, df_values_4)
  gc()
  df_bankinfo <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type, cache_json)

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
  df <- df_values %>%
    dplyr::inner_join(df_bankinfo, by = c("FinInst" = "c0", "Quarter" = "Quarter"))
  return(df)
}

download_IFdata_bankdata <- function(yyyymm, type, cache_json) {
  # Virtually all of the data are located in files with type = c(1, 3)
  # For some reason, it appears that consolidation type == 2 (financial conglomerates)
  # are represented in this file only by (consolidation) type = 3.
  file_name <- paste0("dados", yyyymm, "_", type, ".json")
  json_path <- find_json(yyyymm, file_name)
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

download_IFdata_variables <- function(yyyymm, cache_json) {
  file_name <- paste0("info", yyyymm, ".json")
  json_path <- find_json(yyyymm, file_name)
  json_info <- RJSONIO::fromJSON(json_path)

  info_names <- do.call(rbind.data.frame, json_info)
  return(info_names)
}

download_IFdata_bankinfo <- function(yyyymm, consolidation_type = 1, cache_json) {
  file_name <- paste0("cadastro", yyyymm, "_100", consolidation_type + 3, ".json")
  json_path <- find_json(yyyymm, file_name)
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

prepares_var_names <- function(yyyymm_start, yyyymm_end) {
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
  variables <- list()
  for (qtr in quarters) {
    variables[[as.character(qtr)]] <- download_IFdata_variables(qtr) %>%
      dplyr::rename(var_names = ni) %>%
      #dplyr::mutate(var_names = gsub("\\n.*", "", var_names, fixed = FALSE)) %>%
      dplyr::mutate(var_names = gsub(" \\n", ": ", var_names, fixed = FALSE)) %>%
      dplyr::mutate(var_names = ifelse(var_names == "", lid, var_names))
  }
  variables <- variables %>%
    dplyr::bind_rows(.id = "Quarter")

  df_reports <- download_IFdata_reports(yyyymm_start) %>%
    lapply(function(x) reads_reports_json(x)) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(variables, by = c("c" = "id", "Quarter" = "Quarter")) %>%
    dplyr::select(-c(a, d, td, ty)) %>%
    dplyr::mutate(var_names = paste(ni, var_names, sep = "_"))

  return(df_reports)
}

download_IFdata_reports <- function(yyyymm_start) {
  json_path <- find_json(yyyymm = NULL, file_name = "relatorios")
  json_data <- RJSONIO::fromJSON(json_path)

  # Finds the first index of the `relatorios` file that
  # will be used (otherwised it will unnecessarily
  # take up memory with databased that will not be used).
  # The first index, ie json_data[[1]] refers to March 2020.

  first_idx <- (round(yyyymm_start / 100, 0) - 2000) * 4 + 1
  last_idx <- length(json_data)
  json_data <- json_data[first_idx:last_idx]

  # * * *

  return(json_data)
 }

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
      "c"
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

lag_numericvars <- function(df) {
  df <- df %>%
    group_by(FinInst) %>%
    dplyr::mutate(
      dplyr::across(
        is.numeric,
        .fns = list(lag_var = ~ dplyr::lag(.x, order_by = Quarter)),
        .names = "lag_{col}"
      )
    )
  return(df)
}

# growthrate <- function(df) {
#   df <- df %>%
#     group_by(FinInst) %>%
#     dplyr::mutate(
#       dplyr::across(
#         is.numeric,
#         .fns = list(lag_var = ~ dplyr::lag(.x, order_by = Quarter)),
#         .names = "QoQ_growth_rate_{col}"
#       )
#     )
#   return(df)
# }

loans_share_by_risk_level <- function(df) {
  total_var <- "Loans.Portfolio.by.risk.level_Grand.Total"
  df <- df %>%
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
  return(df)
}

loans_share_by_geographical_region <- function(df) {
  total_var <- "Loans.Portfolio.by.geographical.region_Grand.Total"
  df <- df %>%
    dplyr::mutate(
      loan_share_geo_North = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_North / .data[[total_var]], NA),
      loan_share_geo_Northeast = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Northeast / .data[[total_var]], NA),
      loan_share_risk_geo_CenterWest = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Center.west / .data[[total_var]], NA),
      loan_share_risk_geo_Southeast = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Southest / .data[[total_var]], NA),
      loan_share_risk_geo_South = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_South / .data[[total_var]], NA),
      loan_share_risk_geo_overseas = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Overseas.Loans.Total / .data[[total_var]], NA),
      loan_share_risk_geo_notinformed = ifelse(.data[[total_var]] != 0, Loans.Portfolio.by.geographical.region_Local.not.Informed / .data[[total_var]], NA)
    )
  return(df)
}

