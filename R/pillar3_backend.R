# download_Pillar3_urls <- function() {
#   Pillar3url <- "https://olinda.bcb.gov.br/olinda/servico/DASFN/versao/v1/odata/Recursos?$top=10000&$filter=Api%20eq%20'pilar3'&$format=json"
#
#   p3_result <- RJSONIO::fromJSON(Pillar3url)
#   p3_urls <- lapply(p3_result$value, function(x) x[c("CnpjInstituicao", "NomeInstituicao", "Recurso", "Argumento", "URLDados")]) %>%
#     dplyr::bind_rows() %>%
#     dplyr::mutate(Recurso = substring(Recurso, 2)) %>%
#     tidyr::separate(Recurso, c("P3Table", "Frequency"), sep = "/") %>%
#     dplyr::mutate(Frequency = dplyr::case_when(
#       Frequency == "{ano}" ~ "Yearly",
#       Frequency == "{semestre}" ~ "Semiannually",
#       Frequency == "{trimestre}" ~ "Quarterly"
#     ))
#   return(p3_urls)
#
#   available_P3Tables <- p3_urls %>% dplyr::select(P3Table) %>% unique()
#
#   p3t <- "km1"
#
#   reports <-
#   p3_urls %>%
#     dplyr::filter(
#       P3Table == p3t &
#       Argumento == "2021-2"
#     ) %>%
#     dplyr::select(
#       URLDados
#     ) %>%
#     apply(1, FUN = function(x) try(RJSONIO::fromJSON(x)))
# }
