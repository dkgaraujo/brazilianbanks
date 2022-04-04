# results <- list()
#
# reports_info <- download_IFdata_reports(yyyymm_start, yyyymm_end, cache_json = cache_json)
#
# var_codes <- prepares_var_names(yyyymm_start, yyyymm_end, reports_info, cache_json)
#
# quarters <- all_quarters_between(yyyymm_start = yyyymm_start, yyyymm_end = yyyymm_end)
# #####
# ifdata <- list()
# for (qtr in quarters) {
#   if (verbose) {
#     print(paste("Fetching data for quarter", qtr))
#   }
#
#   # the `reports_info` list should be ordered by quarter, but below I access each
#   # quarter's data programatically just in case.
#   qtr_idx <- which(Reduce(c, lapply(reports_info, function(x) x$d == qtr)))
#   qtr_reports <- reports_info[[qtr_idx]]$files
#
#   # find out which data is available for that quarter and download it
#   data_idx <- which(Reduce(c, lapply(qtr_reports, function(x) grepl("/dados", x))))
#   avail_cons_types <- Reduce(c, qtr_reports[data_idx]) %>%
#     stringr::str_extract(pattern = "_\\d", string = .) %>%
#     stringr::str_replace("_", "")
#   qtr_data <- list()
#   for (cons_type in avail_cons_types) {
#     qtr_data[[cons_type]] <- download_IFdata_bankdata(yyyymm = qtr, consolidation_type = cons_type, cache_json = cache_json)
#   }
#   qtr_data <- qtr_data %>%
#     dplyr::bind_rows(.id = "FileExt") %>%
#     dplyr::mutate(Quarter = yyyymm_to_Date(qtr))
#
#   # find out which bank information ("cadastro") data is available for that quarter and download it
#   bankinfo_idx <- which(Reduce(c, lapply(qtr_reports, function(x) grepl("/cadastro", x))))
#   bankinfo_urls <- paste0("https://www3.bcb.gov.br/ifdata/rest/arquivos?nomeArquivo=",
#                           Reduce(c, qtr_reports[bankinfo_idx]))
#   bankinfo <- list()
#   for (bankinfo_url in bankinfo_urls) {
#     bankinfo[[as.character(which(bankinfo_url == bankinfo_urls))]] <- RJSONIO::fromJSON(path_to_json(bankinfo_url, cache_json = FALSE))
#   }
#   bankinfo <- bankinfo %>%
#     lapply(function(x) x %>% dplyr::bind_rows()) %>%
#     dplyr::bind_rows(.id = "Type") %>%
#     dplyr::mutate(Quarter = yyyymm_to_Date(c1))
#
#   ifdata[[as.character(qtr)]] <- qtr_data %>%
#     dplyr::left_join(bankinfo %>%
#                        dplyr::select(Quarter, c0, c2, c14, c15, c10, c11, c7) %>%
#                        dplyr::distinct(),
#                      by = c("FinInst" = "c0", "Quarter" = "Quarter")) %>%
#     dplyr::mutate(ConglPrud = FinInst == paste0("10000", c15),
#                   ConglFin = FinInst == c14) %>%
#     dplyr::rename(Name = c2,
#                   ConglFinCode = c14,
#                   ConglPrudCode = c15,
#                   State = c10,
#                   City = c11,
#                   Ownership = c7)
#
#   # download available variable names and description
#   var_names <- download_IFdata_variables(yyyymm = qtr, cache_json = cache_json)
# }
# ifdata <- dplyr::bind_rows(ifdata)
#
# #####
