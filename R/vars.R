#' @import utils
utils::globalVariables(
  c(
    ".", "Quarter", "c0", "ni",
    "var_names", "lid", "c3",
    "info_id", "value", "where",
    "var_codes", "a", "d", "td",
    "ty", "f", "sel", "report",
    "trel", "name_trel", "ifd",
    "where"
    )
  )

ifdata_url_base <- "https://www3.bcb.gov.br/ifdata/rest/arquivos?nomeArquivo="

capital_requirements <- tibble::tribble(
  # source: https://www.bcb.gov.br/pre/normativos/res/2013/pdf/res_4193_v1_O.pdf
  ~min_date, ~max_date, ~CET1_req, ~Tier1_req, ~Total_Capital_req, ~CCoB, ~max_CCyB, ~effectiveCCyB, ~DSIB_req, ~LevRatio_req,
  as.Date("2013-10-01"), as.Date("2014-12-31"), 0.045, 0.055, 0.11, 0.00625, 0.00625, 0, 0, NA,
  as.Date("2015-01-01"), as.Date("2015-12-31"), 0.045, 0.06, 0.11, 0.00625, 0.00625, 0, 0, NA,
  as.Date("2016-01-01"), as.Date("2016-12-31"), 0.045, 0.06, 0.09875, 0.00625, 0.00625, 0, 0, NA,
  as.Date("2017-01-01"), as.Date("2017-12-31"), 0.045, 0.06, 0.0925, 0.0125, 0.0125, 0, 0.0025, NA,
  as.Date("2018-01-01"), as.Date("2018-12-31"), 0.045, 0.06, 0.08625, 0.01875, 0.01875, 0, 0.005, NA,
  as.Date("2019-01-01"), NA, 0.045, 0.06, 0.08, 0.025, 0.025, 0, 0.01, NA
)
