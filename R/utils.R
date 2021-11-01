prepare_data <- function(df_list, banks_only, verbose = TRUE) {
  if (verbose) {
    print("Preparing the dataset...")
  }

  # first step is to stack the information from the various dates
  columns <- lapply(df_list, colnames)
  common_cols <- Reduce(intersect, columns)
  df <- df_list %>%
    lapply(function(x) x[, common_cols]) %>%
    Reduce(rbind, .)

  # now we filter
  if (banks_only) {
    df <- df %>%
      dplyr::filter(c3 %in% c("b1", "b2"))
  }

  # finally, we ensure column names are syntactically valid names in R
  df <- df %>%
    dplyr::rename_with(make.names)

  if (verbose) {
    print("`prepare_data` is completed!")
  }
  return(df)
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

find_json <- function(yyyymm, file_name, cache_folder_name = "cache_json") {
  cached_file_name <- file.path(cache_folder_name, file_name)
  if (file.exists(cached_file_name) & file.size(cached_file_name) > 1000) {
    json_path <- cached_file_name
  } else {
    if (is.null(yyyymm) & file_name == "relatorios") {
      json_url <- "https://www3.bcb.gov.br/ifdata/rest/relatorios"
    } else {
      json_url <- paste0(ifdata_url_base, yyyymm, "/", file_name)
    }
    if (cache_json) {
      if (!dir.exists(cache_folder_name)) {
        dir.create(cache_folder_name)
      }
      try(
        download.file(json_url, cached_file_name)
      )
      if (file.exists(cached_file_name) & file.size(cached_file_name) > 1000) {
        json_path <- cached_file_name
      }
    } else {
      json_path <- json_url
    }
  }
  return(json_path)
}
