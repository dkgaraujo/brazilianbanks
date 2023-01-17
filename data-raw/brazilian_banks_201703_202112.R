library(brazilianbanks)

brazilian_banks_201703_202112 <- get_bank_stats(yyyymm_start = 201703,
                                                yyyymm_end = 202112,
                                                banks_only = FALSE,
                                                adjust_income_data = TRUE,
                                                include_growthrate = FALSE,
                                                cache_json = TRUE,
                                                verbose = TRUE)

usethis::use_data(brazilian_banks_201703_202112, overwrite = TRUE)

brazilian_banks_201703_onwards <- get_bank_stats(yyyymm_start = 201703,
                                                 yyyymm_end = tail(all_available_quarters(), n = 1),
                                                 banks_only = FALSE,
                                                 adjust_income_data = TRUE,
                                                 include_growthrate = FALSE,
                                                 cache_json = TRUE,
                                                 verbose = TRUE)

usethis::use_data(brazilian_banks_201703_onwards, overwrite = TRUE)
