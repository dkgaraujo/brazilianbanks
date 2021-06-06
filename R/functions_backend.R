#' Download bank-level data from the BCB 'IF.data' (\url{https://www3.bcb.gov.br/ifdata/})
#'
#' @param yyyymm A date in the format of four-digit year followed by two-digit month. Months must be either 03, 06, 09 or 12.
#' @return A list of databases for the date chosen. The list has four elements, corresponding to each of the four types of institution-level data available in the 'IF.data' system
#' @examples
#' download_IFdata_data(202012)
#' download_IFdata_data(202103)
download_IFdata_data <- function(yyyymm) {
    url_base <- "https://www3.bcb.gov.br/ifdata/rest/arquivos?nomeArquivo="

    url_files_dados <- paste0(url_base, yyyymm, "/dados", yyyymm, "_", 1:4, ".json")
    json_data <- lapply(url_files_dados, RJSONIO::fromJSON)

    # the codes below are either the tax identification 'CNPJ' or a code akin to it used by the Central Bank of Brazil to identify
    # conglomerates.
    id_codes <- Reduce(c, lapply(json_data[[1]]$values, function(x) x$e))
    id_codes2 <- Reduce(c, lapply(json_data[[2]]$values, function(x) x$e))

    dados <- lapply(json_data[[1]]$values, function(x) x$v)

    IF_data <- list()
    for (i in 1:length(id_codes)) {
        id_code <- id_codes[i]
        IF_data[[as.character(id_code)]] <- Reduce(c, lapply(dados[[i]], function(x) x["v"]))
    }
}

#' Auxiliary function to download registered information about banks and other financial institutions.
#' @param yyyymm WIP adapt inheritParam
#' @return A `tibble` with information about the banks and other financial institutions at the following consolidation levels, in order: prudential conglomerates, financial conglomerates, individual institutions, and foreign exchange institutions.
#' @examples
#' bank_register(202003)
#' bank_register(201812)
bank_register <- function(yyyymm) {
    load_cad_data <- . %>%
        RJSONIO::fromJSON() %>%
        Reduce(rbind, .) %>%
        tibble::as_tibble()

    url_cadastro_conglprud <- paste0(url_base, yyyymm, "/cadastro", yyyymm, "_1004.json")
    cadastro_dados_conglprud <- url_cadastro_conglprud %>%
        load_cad_data() %>%
        dplyr::mutate(dplyr::across(c(1, 2, 9, 10, 14:19, 21, 22, 30, 31), as.integer), c28 = as.logical(toupper(c28)))

    url_cadastro_conglfin <- paste0(url_base, yyyymm, "/cadastro", yyyymm, "_1005.json")
    cadastro_dados_conglfin <- url_cadastro_conglfin %>%
        load_cad_data() %>%
        dplyr::mutate(dplyr::across(c(1, 2, 7, 9, 10, 14:19, 22, 25, 26, 30, 31), as.integer), c28 = as.logical(toupper(c28)))

    url_cadastro_indivinst <- paste0(url_base, yyyymm, "/cadastro", yyyymm, "_1006.json")
    cadastro_dados_indivinst <- url_cadastro_indivinst %>%
        load_cad_data() %>%
        dplyr::mutate(dplyr::across(c(1, 2, 7, 9, 10, 14:19, 26, 30, 31), as.integer), c28 = as.logical(toupper(c28)))

    url_cadastro_instcambio <- paste0(url_base, yyyymm, "/cadastro", yyyymm, "_1007.json")
    cadastro_dados_instcambio <- url_cadastro_instcambio %>%
        load_cad_data() %>%
        dplyr::mutate(dplyr::across(c(1, 2, 7, 9, 10, 14:19, 21, 22, 26, 30), as.integer), c28 = as.logical(toupper(c28)))

    return(list(PrudentialConglomerates = cadastro_dados_conglprud, FinancialConglomerates = cadastro_dados_conglfin, IndividualInstitutions = cadastro_dados_indivinst,
        FXInstitutions = cadastro_dados_instcambio))
}

#' Returns all quarters between two quarters
#'
#'
#' @inheritParams bank_time_series
#' @return A vector of quarters in the YYYYMM format.
#' @examples
#' all_quarters_between(201803, 202012)
#' all_quarters_between(as.Date('2018-03-30'), as.Date('2020-12-31'))
all_quarters_between <- function(start_quarter, end_quarter) {
    print(start_quarter)
    print(end_quarter)
    return("WIP Returns all quarters between the start and the end quarter...")
}
