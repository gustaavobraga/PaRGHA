

#' Cria a base de dados dos Equipamentos
#'
#' @description Cria a base de dados dos equipamentos dos estabelecimentos EBSERH
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param labels Logical. O padrao é TRUE. Cria colunas com os rotulos das variaveis categoricas.
#'
#' @return Um DataFrame com os dados dos equipamentos
#'
#'
#' @examples
#' \dontrun{
#'   dados = equipamentos(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 2,
#'     labels = TRUE
#'  )
#' }
#'
#' @export
equipamentos <-
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

    #Donwload dos microdados CNES-EQ
    data_cnes <- get_data_CNES(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      type_data = "EQ",
      save_path = tempdir(),
      state_abbr = estados
    )

    #Renomeando o nome da coluna
    names(data_cnes)[4] <- 'Tipo_Equipamento'
    names(data_cnes)[5] <- 'Cod_Equipamento'

    #Cria a coluna da quantidade de equipamentos em nao uso
    data_cnes <- data_cnes %>% dplyr::mutate(QT_N_USO = QT_EXIST - QT_USO)

    #ADD colunas com os rotulos das variaveis categoricas.
    if(labels){
      data_cnes <- labels(data_cnes,'equipamentos')
      data_cnes <- labels(data_cnes,'cnes')
    }

    return(data_cnes)
}
