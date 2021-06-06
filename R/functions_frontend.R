#' Retrieves bank-level times series.
#'
#' @param banks The banks for which to download data. Use `NULL` for all banks available. For a list of available banks, please use `list_banks`.
#' @param consolidation_type The type of institution to be used. Options are: 'prudential conglomerate' (default), 'financial conglomerate', 'individual institutions' and 'foreign exchange institutions'. More than one option is possible if passed in a vector.
#' @param series The series to be downloaded for each bank. Use `NULL` for all series available. For a list of available series, please use `list_series`
#' @param start_quarter Start quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param end_quarter End quarter for the time series. Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @param consistent_sample Boolean. If set to `TRUE`, it will return only those banks for which data is available for all quarters in the time series.
#' @return A list containing the function call and a `tibble` with the requested time series. Monetary values are denominated in Brazilian Reais (BRL), except for series that have a different currency code in the name (eg, 'USD_ForeignExposure' is denominated in US Dollars).
#' @examples
#' bank_time_series(banks = c('Bradesco', 'Itau'), consolidation_type = 'prudential conglomerate', series = 'TotalAssets', start_quarter = 201803, end_quarter = 202103)
#' @export
bank_time_series <- function(banks = NULL, consolidation_type = c("prudential conglomerate", "financial conglomerate", "individual institutions",
    "foreign exchange institutions"), series = NULL, start_quarter = NULL, end_quarter = NULL, consistent_sample = TRUE) {
    return_list <- list()
    # TO-DO: assess if, and how, this can be made consistent with packages such as {broom}

    func_call <- list(banks, consolidation_type, series, start_quarter, end_quarter, consistent_sample)
    names(func_call) <- c("banks", "consolidation_type", "series", "start_quarter", "end_quarter", "consistent_sample")
    return_list[["call"]] <- func_call

    return_list[["series"]] <- NULL

    all_quarters <- all_quarters_between(start_quarter, end_quarter)

    if (is.null(banks)) {
        banks = list_banks(consolidation_type = consolidation_type, quarter = all_quarters)
    }

    return(return_list)
}

#' Lists all banks and other financial institutions available for the selected quarter and consolidation type(s).
#'
#' @inheritParams bank_time_series
#' @param quarter Accepted formats are: a six-digit integer representing YYYYMM, or a 'Date' class string. Multiple values are accepted if passed as a vector. Use `NULL` for all available dates. For a list of available series, please use `list_dates`.
#' @return A list of tibbles, where each element of the list corresponds to a consolidation type.
#' @export
list_banks <- function(consolidation_type = c("prudential conglomerate", "financial conglomerate", "individual institutions", "foreign exchange institutions"),
    quarter, consistent_sample = TRUE) {
    return(NULL)
}


#' Lists all dates available for the selected consolidation type(s).
#'
#' @inheritParams bank_time_series
#' @return A list of vectors, where each element of the list corresponds to a consolidation type.
#' @example
#' list_data(c("prudential conglomerate", "financial conglomerate"))
#' @export
list_dates <- function(consolidation_type = c("prudential conglomerate", "financial conglomerate", "individual institutions", "foreign exchange institutions")) {
    return(NULL)
}


#' Lists all time series available for the selected consolidation type(s).
#'
#' @inheritParams bank_time_series
#' @return A list of tibbles, where each element of the list corresponds to a consolidation type.
#' @example
#' list_series(c("prudential conglomerate", "financial conglomerate"))
#' @export
list_series <- function(consolidation_type = c("prudential conglomerate", "financial conglomerate", "individual institutions", "foreign exchange institutions")) {
    return(NULL)
}
