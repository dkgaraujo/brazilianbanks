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

#' Formats the path to a local or remote IF.data JSON file from the respective quarter.
#'
#' @inheritParams get_bank_stats
#' @param yyyymm Quarter
#' @param file_name The name of the JSON file (including the extension) to be retrieved locally or remotely.
#' @param cache_folder_name The local folder where the JSON files are (to be) cached.
#' @return The path to the JSON file saved locally (if `cache_json` is TRUE) or to its URL if FALSE.
find_IFdata_json <- function(yyyymm = NULL, file_name, cache_folder_name = "cache_json", cache_json = TRUE) {
  cached_file_name <- ifelse(is.null(yyyymm),
                             file.path(cache_folder_name, stringr::str_split(file_name, "/")[[1]][2]),
                             file.path(cache_folder_name, file_name))
  if (cache_json & file.exists(cached_file_name) & file.size(cached_file_name) > 500) {
    # the file.size() > 1000 condition is necessary because sometimes
    # the file is wrongfully downloaded, and in these cases it is very small
    # while the correctly downloaded files are substantially bigger than this limit
    json_path <- cached_file_name
  } else {
    if (is.null(yyyymm)) {
      if (file_name == "relatorios") {
        json_url <- "https://www3.bcb.gov.br/ifdata/rest/relatorios"
      } else {
        json_url <- paste0("https://www3.bcb.gov.br/ifdata/rest/arquivos?nomeArquivo=", file_name)
      }
    } else {
      json_url <- paste0(ifdata_url_base, yyyymm, "/", file_name)
    }
    json_path <- path_to_json(json_url, cache_folder_name, cached_file_name, cache_json)
  }
  return(json_path)
}

#' Retrieves the path to a JSON file in the local system or in the internet.
#'
#' @inheritParams find_IFdata_json
#' @inheritParams get_bank_stats
#' @param json_url the URL to the JSON file
#' @param cached_file_name The full path to the file name where the JSON file is to be saved locally.
#' @return The path to the JSON file saved locally (if `cache_json` is TRUE) or to its URL if FALSE.
path_to_json <- function(json_url, cache_folder_name = "cache_json", cached_file_name = NULL, cache_json) {
  if (cache_json) {
    if (!dir.exists(cache_folder_name)) {
      dir.create(cache_folder_name)
    }
    if (is.null(cached_file_name)) {
      cached_file_name <- stringr::str_split(json_url, "/")[[1]] %>% tail(1)
    }
    try(utils::download.file(json_url, cached_file_name))
    if (file.exists(cached_file_name) & file.size(cached_file_name) > 1000) {
      return(cached_file_name)
    }
  } else {
    return(json_url)
  }
}

#' Lists all quarters between the starting and the end quarter
#'
#' @inheritParams get_bank_stats
all_quarters_between <- function(yyyymm_start = 201703, yyyymm_end = 202109) {
  quarters <- yyyymm_start:yyyymm_end
  quarters <- quarters[substr(as.character(quarters), 5, 6) %in% c("03", "06", "09", "12")]
  return(quarters)
}

yyyymm_to_Date <- function(yyyymm, end_of_month = TRUE) {
  newDate <- as.Date(paste0(yyyymm, "01"), format = "%Y%m%d")
  if (end_of_month)
    newDate <- newDate %>% lubridate::ceiling_date("month") - 1
  return(newDate)
}

clean_col_names <- function(string) {
  result <- string %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("Aapital", "Capital") %>%
    stringr::str_replace_all("mediun", "medium") %>%
    stringr::str_remove_all("(\\n).*") %>%
    stringr::str_remove("_$") %>%
    make.names() %>%
    stringr::str_replace_all("\\.", "_")
  return(result)
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

  gdp <- RJSONIO::fromJSON(paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1846/periodos/", qtrs,"/variaveis/585?localidades=N1[all]&classificacao=11255[90707]"))
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
