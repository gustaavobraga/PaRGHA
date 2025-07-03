
#' Cria a base de dados das Habilitacoes
#'
#' @description Cria a base de dados das Habilitacoes dos estabelecimentos EBSERH
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param labels Logical. O padrao é TRUE. Cria colunas com os rotulos das variaveis categoricas.
#'
#' @return Um DataFrame com os dados das habilitacoes
#'
#' @examples
#' \dontrun{
#'   dados = habilitacoes(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 1,
#'     labels = TRUE
#'  )
#' }
#'
#' @export
habilitacoes <-
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
      type_data = "HB",
      save_path = tempdir(),
      state_abbr = estados
    )

    names(data_cnes)[names(data_cnes) == "CNES"] <- "cnes"
    #ADD colunas com os rotulos das variaveis categoricas.
    if(labels){
      data_cnes <- labels(data_cnes,'habilitacao')
      data_cnes <- labels(data_cnes,'cnes')
    }
    #Converte a coluna para inteiro
    data_cnes <- data_cnes %>%
      dplyr::mutate(
        dplyr::across(c(cod_habilitacao),as.integer)
      )

    return(data_cnes)
}
