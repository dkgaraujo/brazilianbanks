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
      dplyr::filter(Institution.Type %in% c("b1", "b2"))
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

#' Lists all quarters between the starting and the end quarter
#'
#' @inheritParams get_bank_stats
all_quarters_between <- function(yyyymm_start = 201803, yyyymm_end = 202106) {
  quarters <- yyyymm_start:yyyymm_end
  quarters <- quarters[substr(as.character(quarters), 5, 6) %in% c("03", "06", "09", "12")]
  return(quarters)
}

find_json <- function(yyyymm, file_name, cache_folder_name = "cache_json") {
  cached_file_name <- file.path(cache_folder_name, file_name)
  if (file.exists(cached_file_name) & file.size(cached_file_name) > 1000) {
    # the file.size() > 1000 condition is necessary because sometimes
    # the file is wrongfully downloaded, and in these cases it is very small
    # while the correctly downloaded files are substantially bigger than this limit
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
        utils::download.file(json_url, cached_file_name)
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


#' Downloads GDP data from the Brazilian Institute of Geography and Statistics
#'
#' @inheritParams get_bank_stats
#' @return A data.frame with quarters and the corresponding annual nominal GDP amounts in BRL million up to (and including) each quarter.
download_GDP_data <- function(yyyymm_start, yyyymm_end) {
  # Get all quarters for the GDP data (which uses the YYYYQQ format),
  # including an extra 3 quarters before yyyymm_start to enable the
  # calculation of an annual GDP for the first quarter.
  yyyymm_qtrs <- all_quarters_between(yyyymm_start, yyyymm_end)
  qtrs <- yyyymm_qtrs %>%
    c(all_quarters_between(.[1] - 100, .[1])[-1], .) %>%
    sapply(function(x) paste(substr(x, 1, 4), as.integer(substr(x, 5, 6)) / 3, sep = "0")) %>%
    paste(collapse = "|")

  gdp <- RJSONIO::fromJSON(paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1846/periodos/",qtrs ,"/variaveis/585?localidades=N1[all]&classificacao=11255[90707]"))
  gdp <- as.integer(gdp[[1]]$resultados[[1]]$series[[1]]$serie) %>%
    zoo::rollapply(4, sum)

  # note: GDP values are in BRL millions
  gdp <-
    data.frame(
      Quarter = yyyymm_qtrs %>% yyyymm_to_Date(),
      AnnualGDP = gdp
    )
  return(gdp)
}
