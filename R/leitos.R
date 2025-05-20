
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
#' @examples
#' \dontrun{
#'   dados = leitos(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 2
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

  estados <- c(
    "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
    "RS","PA","GO","PR","PB","RN","CE","MT","MA",
    "SC","PI","AP","TO","ES","SP"
  )

  data_cnes <- get_data_CNES(
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    type_data = "LT",
    save_path = tempdir(),
    state_abbr = estados
  )

  #Junta com a tabela principal e remove colunas desnecessárias
  data_cnes <- data_cnes %>% dplyr::filter(TP_LEITO!="7")

  #Recodifica os valores da variavel, atribuindo os respectivos rotulos descritivos
  data_cnes <- data_cnes %>%
    dplyr::mutate(
      TP_LEITO = dplyr::case_match(
        TP_LEITO,
        "1" ~ "Cir\u00fargico",
        "2" ~ "Cl\u00ednico",
        "3" ~ "Complementar",
        "4" ~ "Obst\u00e9trico",
        "5" ~ "Pedi\u00e1trico",
        "6" ~ "Outras Especialidades",
        .default = TP_LEITO
      )
    )
  return(data_cnes)
}
