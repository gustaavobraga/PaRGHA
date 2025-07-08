
#' Cria a base de dados dos leitos
#'
#' @description Cria a base de dados dos leitos dos estabelecimentos EBSERH
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param labels Logical. O padrao é TRUE. Cria colunas com os rotulos das variaveis categoricas.
#'
#' @return Um DataFrame com os dados das leitos
#'
#' @examples
#' \dontrun{
#'   dados = leitos(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 1,
#'     labels = TRUE
#'  )
#' }
#'
#' @export
leitos <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           labels = TRUE) {
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
  #data_cnes <- data_cnes %>% dplyr::filter(TP_LEITO!="7")

  names(data_cnes)[names(data_cnes) == "CNES"] <- "cnes"
  #ADD colunas com os rotulos das variaveis categoricas.
  if(labels){
    data_cnes <- labels(data_cnes,'leitos')
    data_cnes <- labels(data_cnes,'cnes')
  }

  #Cria a coluna qt_exist_uti
  leitos_uti <- c(51,52,61,62,63,74,75,76,77,78,79,80,81,82,83,85,86)
  data_cnes <- data_cnes %>%
    dplyr::mutate(
      QT_EXIST_UTI = dplyr::if_else(
        cod_especialidade_leito %in% leitos_uti, QT_EXIST, 0
      )
    )

  #Converte as colunas para inteiro
  data_cnes <- data_cnes %>%
    dplyr::mutate(
      dplyr::across(
        c(cod_tipo_leito, cod_especialidade_leito, QT_EXIST_UTI),
        as.integer
      )
    )
  #Coloca o nome das colunas em minusculo
  names(data_cnes) <- tolower(names(data_cnes))

  return(data_cnes)
}
