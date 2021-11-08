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

series_names_PT <- c(
  c1 = "Ativo total",
  c2 = "ClasCart",
  c3 = "PCEL",
  c4 = "Captacoes",
  c5 = "PL",
  c6 = "Lucro liquido",
  c7 = "Disponibilidades",
  c8 = "AplicInterfinLiquidez",
  c9 = "TVM e deriv",
  c10 = "Gross loans",
  c11 = "Loan loss provisions",
  c12 = "Net loans",
  c13 = "Leasing outstanding",
  c80 = "80-Reg capital CET I",
  c81 = "81-Reg capital Add Tier I",
  c82 = "82-Reg capital All Tier I",
  c83 = "83-Reg capital Tier II",
  c84 = "84-Reg capital",
  c85 = "85-RWA Cred",
  c86 = "86-RWA Mkt",
  c87 = "87-RWA Mkt Cam",
  c88 = "88-RWA Mkt Com",
  c89 = "89-RWA Mkt Jur",
  c90 = "90-RWA Mkt Acs",
  c91 = "91-RWA Op Risk",
  c92 = "92-Exposicao alavanc.",
  c93 = "93-CET1 ratio",
  c94 = "94-indice Nivel 1",
  c95 = "95-Razao de alavancagem",
  c96 = "96-indice de imob",
  c97 = "97-indice de Basileia",
  c98 = "98-RWA")

institution_types_PT <- c(
  U = "Bancos mutiplos",
  B = "Bancos Comerciais e Bancos de Cambio",
  D = "Bancos de Desenvolvimento",
  K = "Agencias de Fomento ou de Desenvolvimento",
  I = "Bancos de Investimento",
  F = "Sociedades de Credito, Financiamento e Investimento",
  J = "Sociedades de Credito ao Microempreendedor, sociedades de credito direto e sociedades de emprestimo entre pessoas",
  A = "Sociedades de Arrendamento Mercantil",
  C = "Sociedades Corretoras de Titulos e Valores Mobiliarios e Cambio",
  T = "Sociedades Distribuidoras de Titulos e Valores Mobiliarios",
  S = "Sociedades de Credito Imobiliario e Associacoes de Poupanca e Emprestimo",
  W = "Companhias Hipotecarias",
  E = "Caixas Economicas",
  R = "Cooperativas de Credito",
  O = "Fundos de Investimento",
  L = "Banco do Brasil S.A.",
  M = "Caixa Economica Federal",
  N = "Banco Nacional de Desenvolvimento Economico e Social",
  H = "Administradoras de Consorcio",
  P = "Grupos de Consorcio",
  Y = "Instituicoes de Pagamento",
  Z = "Empresas em Liquidacao Extrajudicial"
)

register_colnames_PT <- c(
  c0 = "CodCongl",
  c1 = "Data",
  c2 = "Nome",
  c3 = "TipoInstituicao", # "b1" "b2" "b3C" "b3S" "b4" "n1" "n2" "n4"
  c4 = "TipoConsolidacao", # 'I' is for independent institution; 'C' is for conglomerates
  c5 = "TipoConsolidacao_extenso", # 'Independent institution' or 'Conglomerate'
  c6 = "TipoControle", # 1; 2; 3
  c7 = "TipoControle_extenso", # "Publico (1)" "Privado nacional (2)" "Estrangeiro (3)"
  c8 = "c8", #  "8" "9" "14" "43" "194" "196" "197" "198" "199" "200"
  c9 = "c9", # "102" "104" ""    "107" "103" "101" "106" "105" "100" "58"  "168"  "51"  "64"  "59"  "53"  "56"  "213" "57"  "162" "55"  "62"  "212" "216" "211" "54"  "60"  "63"  "61"  "165" "164" "52"  "166" "218"  "217" "161" "163" "214" "215" "181" "182" "183" "169"
  c10 = "UF",
  c11 = "Cidade",
  c12 = "SegmentoPrudencial", # "S1" "S2" "S3" "S4" "S5" ""
  c13 = "SegmentoNegocios", # "9"   "8"   "14"  "43"  "199" "198" "196" "197" "200" "194"
  c14 = "Conglomerado Financeiro?", # ""      "10045" "10069" "10083" "20107" "20152" "30159" "30173" "30290" "30379" "30403" "30771" "30829" "30881" "31323" "31859" "31873" "31976" "32119" "41856" "49906" "49944" "50122" "50201" "50304" "50524" "50531" "52034" "50940" "50988" "51011" "51066" "51073" "51248" "51255" "51262" "51293" "51341" "51396" "51413" "51420" "51444" "51468" "51482" "51516" "51523" "51554" "51561" "51585" "51671" "51688" "51743" "51750" "51767" "51774" "51781" "51808" "51815" "51839" "51877" "51884" "51949" "51963" "51987" "52120" "52027" "52089" "52137" "52010" "51956" "52041" "52209" "52113" "52003" "52058" "52065" "52072" "52151"
  c15 = "Conglomerado Prudential?",
  c16 = "NumeroAgencias",
  c17 = "NumerosPostosAtendimento",
  c18 = "c18",
  c19 = "RegimeCapital", # "RPC", "RPSd"
  c20 = "UltimaMudancaSegmentacao",
  c21 = "c21",
  c22 = "NomeConglPrudencial",
  c23 = "NomeConglFinanc",
  c24 = "c24",
  c25 = "c25", # 1 or ""
  c26 = "c26", # R (cooperativas), U (bancos multiplos), F (Financeiras) J (Stone pgmtos), X (outros)
  c27 = "AtributosCOSIF",
  c28 = "c28", # BOOL
  c29 = "c29",
  c30 = "c30" # 0, 1, 2
)

AtributosCOSIF <- c(
  "U" = "Bancos multiplos;",
  "B" = "Bancos Comerciais e Bancos de Cambio;",
  "D" = "Bancos de Desenvolvimento;",
  "K" = "Agencias de Fomento ou de Desenvolvimento;",
  "I" = "Bancos de Investimento;",
  "F" = "Sociedades de Credito, Financiamento e Investimento;",
  "J" = "Sociedades de Credito ao Microempreendedor, sociedades de credito direto e sociedades de emprestimo entre pessoas.",
  "A" = "Sociedades de Arrendamento Mercantil;",
  "C" = "Sociedades Corretoras de Titulos e Valores Mobiliarios e Cambio;",
  "T" = "Sociedades Distribuidoras de Titulos e Valores Mobiliarios;",
  "S" = "Sociedades de Credito Imobiliario e Associacoes de Poupanca e Emprestimo;",
  "W" = "Companhias Hipotecarias;",
  "E" = "Caixas Economicas;",
  "R" = "Cooperativas de Credito;",
  "O" = "Fundos de Investimento;",
  "L" = "Banco do Brasil S.A.;",
  "M" = "Caixa Economica Federal;",
  "N" = "Banco Nacional de Desenvolvimento Economico e Social;",
  "H" = "Administradoras de Consorcio;",
  "P" = "Grupos de Consorcio;",
  "Y" = "Instituicoes de Pagamento;",
  "Z" = "Empresas em Liquidacao Extrajudicial."
)

ifdata_url_base <- "https://www3.bcb.gov.br/ifdata/rest/arquivos?nomeArquivo="

