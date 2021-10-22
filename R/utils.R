download_IFdata_values <- function(yyyymm, consolidation_type = 1) {
  # TODO: create argument `firm_list` where a list of identifiers could be passed
  # to be filtered already at this stage.

  # Downloads files
  df_values <- download_IFdata_date_data(yyyymm = yyyymm, consolidation_type)
  df_bankinfo <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type)

  # Downloads variable names and information
  var_codes <- download_IFdata_date_variables(yyyymm) %>%
    dplyr::rename(var_names = ni) %>%
    dplyr::mutate(var_names = gsub("\\n.*", "", var_names, fixed = FALSE)) %>%
    dplyr::mutate(var_names = ifelse(var_names == "", lid, var_names))

  # Prepare datasets
  df_values <- df_values %>%
    dplyr::mutate(Quarter = yyyymm_to_Date(yyyymm)) %>%
    dplyr::left_join(
      var_codes %>% dplyr::select(lid, var_names),
      by = c("info_id" = "lid")
      ) %>%
    tidy_IFdata_values()

  # Merge datasets
  df <- df_values %>%
    dplyr::inner_join(df_bankinfo, by = c("FinInst" = "c0", "Quarter" = "Quarter"))
  return(df)
}

download_IFdata_date_data <- function(yyyymm, consolidation_type = 1) {
  url_files_dados <- paste0(ifdata_url_base, yyyymm, "/dados", yyyymm, "_", consolidation_type, ".json")
  json_data <- RJSONIO::fromJSON(url_files_dados)$values
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

download_IFdata_date_variables <- function(yyyymm) {
  url_files_info <- paste0(ifdata_url_base, yyyymm, "/info", yyyymm, ".json")
  json_info <- RJSONIO::fromJSON(url_files_info)
  info_names <- do.call(rbind.data.frame, json_info)
  return(info_names)
}

download_IFdata_bankinfo <- function(yyyymm, consolidation_type = 1) {
  url_files_cadastro <- paste0(ifdata_url_base, yyyymm, "/cadastro", yyyymm, "_100", consolidation_type + 3, ".json")
  json_data <- RJSONIO::fromJSON(url_files_cadastro)
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
    dplyr::filter(complete.cases(.)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = var_names, values_from = value)
  return(df_values)
}

yyyymm_to_Date <- function(yyyymm, end_of_month = TRUE) {
  newDate <- as.Date(paste0(yyyymm, "01"), format = "%Y%m%d")
  if (end_of_month)
    newDate <- newDate %>% lubridate::ceiling_date("month") - 1
  return(newDate)
}

all_quarters_between <- function(yyyymm_start = 201703, yyyymm_end = 202106) {
  quarters <- yyyymm_start:yyyymm_end
  quarters <- quarters[substr(as.character(quarters), 5, 6) %in% c("03", "06", "09", "12")]
  return(quarters)
}
