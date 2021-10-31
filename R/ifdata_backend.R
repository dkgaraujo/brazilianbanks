download_IFdata_values <- function(yyyymm, consolidation_type, cache_json) {

  # Downloads files
  df_values <- download_IFdata_bankdata(yyyymm = yyyymm, consolidation_type, cache_json)
  df_bankinfo <- download_IFdata_bankinfo(yyyymm = yyyymm, consolidation_type, cache_json)

  # Downloads variable names and information
  var_codes <- download_IFdata_variables(yyyymm) %>%
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

download_IFdata_bankdata <- function(yyyymm, consolidation_type, cache_json) {
  file_name <- paste0("dados", yyyymm, "_", consolidation_type, ".json")
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
