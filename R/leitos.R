
#' Cria a base de dados dos leitos
#'
#' @description Cria a base de dados dos leitos dos estabelecimentos EBSERH
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#'
#' @return Um DataFrame com os dados das leitos
#'
#'
#' @examples
#' \dontrun{
#'   dados = leitos(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 1
#'  )
#' }
#'
#' @export
leitos <-
  function(year_start,
           month_start,
           year_end,
           month_end) {
  `%>%` <- dplyr::`%>%`

  CNES_EBSERH = c(
    "0000396","0002534","0003816","0004731","0009709",
    "0010510","0012505","0027049","2006197","2017644",
    "2146339","2146355","2146371","2206595","2218798",
    "2244306","2252694","2295415","2332981","2338424",
    "2384299","2400243","2409208","2481286","2504502",
    "2561492","2640244","2653982","2655411","2676060",
    "2694751","2707675","2710935","2726653","3157245",
    "3285391","3432076","3654826","4014111","4044916",
    "5586348","6042414","6568343"
  )

  estados = c(
    "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
    "RS","PA","GO","PR","PB","RN","CE","MT","MA",
    "SC","PI","AP","TO","ES","SP"
  )

  data_cnes = create_data_raw_cnes(
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    type_data = "LT",
    save_path = tempdir(),
    state_abbr = estados
  )

  # Filtra apenas os estabelecimentos da EBSERH
  data_cnes = data_cnes %>% dplyr::filter(CNES %in% CNES_EBSERH)

  # Data frame de referência com tipo de leito
  ref = tibble::tibble(
    TP_LEITO = as.character(1:6),
    NOME_TP_LEITO = c("Cirúrgico", "Clínico", "Complementar", "Obstétrico", "Pediátrico", "Outras Especialidades")
  )

  # Junta com a tabela principal e remove colunas desnecessárias
  data_cnes = data_cnes %>%
    dplyr::filter(TP_LEITO!="7") %>%
    dplyr::left_join(ref, by = "TP_LEITO") %>%
    dplyr::select(-CODUFMUN, -TP_LEITO)

  return(data_cnes)
}
