#' @export
prepare_data <- function(df_list, banks_only = TRUE, verbose = TRUE) {
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

#' @export
get_data <- function(yyyymm_start, yyyymm_end, verbose = TRUE) {
  # TODO: include an option to cache the data
  # TODO: change API for a single `get_data` function, where the data source
  #       would be specified as a parameter, eg `source = "IFdata"`, thus enabling
  #       future data sources such as Pillar 3 information to be added without
  #       changing the API.
  if (verbose) {
    print("Getting the dataset...")
  }
  quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
  results <- list()
  for (qtr in quarters) {
    if (verbose) {
      print(paste("Getting results for", qtr))
    }
    results[[as.character(qtr)]] <- download_IFdata_values(qtr, consolidation_type = 1)
    #download_IFdata_values(qtr, consolidation_type = 2)
  }
  if (verbose) {
    print("`get_data` is completed!")
  }
  return(results)
}
