
#' Download dos dados do DataSUS
#'
#' @description Realiza o download automático dos arquivos DBC dos sistemas SIH-RD, SIH-RJ e SIH-SP, de acordo com os estados especificados em state_abbr e os meses definidos pelo usuário. Os arquivos obtidos são salvos no diretório indicado pelo argumento save_path.
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é ".\\".
#'
#' @examples
#' \dontrun{
#'   download_data(
#'     year_start = 2023,
#'     month_start = 1,
#'     year_end = 2023,
#'     month_end = 3,
#'     state_abbr = "CE",
#'     save_path = tempdir()
#'   )
#' }
#'
#' @export
download_data <- function(year_start,
                          month_start,
                          year_end,
                          month_end,
                          state_abbr,
                          save_path = tempdir()) {

  #Remove arquivos DBC antigos antes de iniciar o download
  SIS = c("RD", "RJ", "SP")
  dirs <- fs::path(save_path, "file_DBC", glue::glue("SIH-{SIS}"))
  purrr::walk(dirs, ~{
    if (dir.exists(.x)) {
      unlink(list.files(.x, full.names = TRUE), recursive = TRUE)
    }
  })

  #Obtendo as bases dbc do RD
  tempo_inicio_RD <- system.time({
    PaPAHR::download_dbc(
      information_system = "SIH-RD",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
  })
  cat("Tempo para baixar os dados do RD:", round(tempo_inicio_RD[3]/60,4), "minutos\n")

  #Obtendo as bases dbc do RJ
  tempo_inicio_RJ <- system.time({
    PaPAHR::download_dbc(
      information_system = "SIH-RJ",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
  })
  cat("Tempo para baixar os dados do RJ:", round(tempo_inicio_RJ[3]/60,4), "minutos\n")

  #Obtendo as bases dbc do SP
  tempo_inicio_SP <- system.time({
    PaPAHR::download_dbc(
      information_system = "SIH-SP",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
  })
  cat("Tempo para baixar os dados do SP:", round(tempo_inicio_SP[3]/60,4), "minutos\n")
}
