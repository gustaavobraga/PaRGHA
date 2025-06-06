
#' Cria tabelas e salva no banco de dados
#'
#' @description Cria as tabelas de Leitos, Habilitacoes, Equipamentos e do Numerador/Denominador dos indicadores, e salva cada tabela no banco de dados remoto
#'
#' @param user String. Nome do usuario que tem acesso ao banco
#' @param password String. Senha do usuario para acessa o banco
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os indicadores
#'
#' @examples
#' \dontrun{
#'   auto_TCC(
#'     user = "gustavo",
#'     password = "-",
#'     year_start = 2021,
#'     month_start = 1,
#'     year_end = 2021,
#'     month_end = 1
#'   )
#' }
#'
#' @export
auto_TCC = function(
    user,
    password,
    year_start,
    month_start,
    year_end,
    month_end,
    save_path = tempdir()
){
  `%>%` <- dplyr::`%>%`

  tempo_inicio <- system.time({
    #>Leitos -------------------
    leitos = leitos(year_start, month_start, year_end, month_end)

    #Armazena os dados no banco de dados
    DB_Azure(user, password, "leitos", leitos)

    #Apaga a pasta temporaria com os dados dos leitos
    unlink(fs::path(tempdir(), "CNES", "LT"),
           recursive = TRUE)
    rm(leitos)
    gc()

    #>Habilitacoes -------------------
    habilitacoes = habilitacoes(year_start, month_start, year_end, month_end)

    #Armazena os dados no banco de dados
    DB_Azure(user, password, "habilitacao", habilitacoes)

    #Apaga a pasta temporaria com os dados das habilitacoes
    unlink(fs::path(tempdir(), "CNES", "HB"),
           recursive = TRUE)
    rm(habilitacoes)
    gc()

    #>Equipamentos -------------------
    equipamentos = equipamentos(year_start, month_start, year_end, month_end)

    #Armazena os dados no banco de dados
    DB_Azure(user, password, "equipamentos", equipamentos)

    #Apaga a pasta temporaria com os dados dos equipamentos
    unlink(fs::path(tempdir(), "CNES", "EQ"),
           recursive = TRUE)
    rm(equipamentos)
    gc()

    #>Numerador Denominador -------------------
    data_SIH =
      get_data_SIH(year_start,month_start,year_end,month_end,save_path)

    #Apaga a pasta temporaria com os dados do SIH
    unlink(fs::path(tempdir(), "file_DBC"),
           recursive = TRUE)

    state_abbr = c(
      "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
      "RS","PA","GO","PR","PB","RN","CE","MT","MA",
      "SC","PI","AP","TO","ES","SP"
    )
    data_CNES = get_data_CNES(
      year_start,month_start,year_end,month_end,state_abbr,"LT",save_path)

    #Apaga a pasta temporaria com os dados do CNES
    unlink(fs::path(tempdir(), "CNES", "LT"),
           recursive = TRUE)

    indicadores = indicadores(data_SIH = data_SIH,
                                   data_CNES = data_CNES)

    indicadores = dplyr::ungroup(indicadores)
    #Numerador e denominador
    num_den = indicadores %>% dplyr::select(
      -c(
        "i_taxa_de_cesarea",
        "i_taxa_ocupacao_H",
        "i_taxa_ocupacao_UTI",
        "i_tempo_medio_permanencia_cirurgica",
        "i_tempo_medio_permanencia_clinica",
        "i_tempo_medio_permanencia_hospitalar",
        "i_giro_de_leito"
      )
    )

    #Armazena os dados no banco de dados
    DB_Azure(user, password, "numerador_denominador", num_den)

    rm(data_CNES, data_SIH, indicadores, num_den, state_abbr)
    gc()
  })
  cat("Tempo de execução da função auto_TCC:",round(tempo_inicio[3]/60,4), "minutos\n")
}
