
#' Retorna a base de dados com todos os indicadores
#'
#' @description Realiza o download dos arquivos DBC ('SIH-RD', 'SIH-RJ' e 'SIH-SP'), combina as três bases de dados em uma única estrutura, seleciona apenas as variáveis relevantes, cria novas variáveis e retorna uma base de dados contendo exclusivamente as variáveis que serão utilizadas nos cálculos dos indicadores
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os indicadores
#'
#'
#' @examples
#' \dontrun{
#'   dados = indicadores(
#'     year_start = 2021,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 12,
#'     save_path = tempdir()
#'  )
#' }
#'
#' @export
indicadores <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           save_path = tempdir()){

    `%>%` <- dplyr::`%>%`

    # Se as bases ja tiverem sido criadas, basta carrega-las."
    # data_raw = read.csv('./inst/extdata/Dados/base_final/DF_Final.csv',
    #                     colClasses = c("CNES" = "character",
    #                                    "ANO_CMPT" = "character",
    #                                    "MES_CMPT" = "character"))
    # data_cnes = read.csv('./inst/extdata/Dados/base_final/data_cnes.csv',
    #                      colClasses = c("CNES" = "character",
    #                                     "ANO_CMPT" = "character",
    #                                     "MES_CMPT" = "character",
    #                                     "TP_LEITO" = "character",
    #                                     "CODLEITO" = "character"))

    data_raw = create_data_raw(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      save_path = save_path
    )
    data_cnes = create_data_raw_cnes(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      type_data = "LT",
      save_path = save_path
    )

    data = create_numerador_denominador(data_raw,data_cnes)

    #> Número de Cirurgias Apresentadas por HUF ----
    i_1 = i_N_cirurgias_apresentadas(data_raw)
    #> Número de Internações Hospitalares por HUF ----
    i_2 = i_N_internacoes_hospitalares(data_raw)
    #> Taxa de partos Cesareos por HUF ----
    i_3 = i_taxa_de_partos_cesareos(data)
    #> Taxa de Ocupação Hospitalar
    i_4 = i_taxa_ocupacao_H(data)
    #> Taxa de Ocupação em UTI
    i_5 = i_taxa_ocupacao_UTI(data)
    #> Tempo Medio de Permanencia Cirurgica por HUF ----
    i_6 = i_tempo_medio_permanencia_cirurgica(data)
    #> Tempo Medio de Permanencia Clinica por HUF ----
    i_7 = i_tempo_medio_permanencia_clinica(data)
    #> Tempo Medio de Permanencia Hospitalar por HUF ----
    i_8 = i_tempo_medio_permanencia_hospitalar(data)
    #> Giro de Leito da Produção Hospitalar por HUF ----
    i_9 = i_giro_de_leito(data)

    #---------------------------------
    #Cria uma tabela onde cada coluna é um indicador
    # Lista com as suas tabelas
    tabelas = list(i_1,i_3,i_2,i_4,i_5,i_6,i_7,i_8,i_9)

    # full join de todas as tabelas com base em ano, mes e CNES
    tabela_indicadores = purrr::reduce(tabelas, dplyr::full_join,
                               by = c("ANO_CMPT", "MES_CMPT", "CNES"))

    tabela_indicadores_num_den = dplyr::left_join(tabela_indicadores, data,
                              by = c("ANO_CMPT", "MES_CMPT", "CNES"))

    #ADD a coluna com o nome dos Hospitais
    caminho_pasta <- system.file("extdata", package = "TCC")
    path = stringr::str_glue(
      "{caminho_pasta}\\CNES_EBSERH.rds")
    CNES_EBSERH <- readRDS(path)

    tabela_indicadores_num_den = tabela_indicadores_num_den %>%
      dplyr::left_join(CNES_EBSERH, by = c("CNES" = "Codigo_CNES"))

    #Salva a base em um diretorio
    # write.csv(
    #   tabela_indicadores_empilhada,
    #   "./inst/extdata/dados2.csv",
    #   row.names = FALSE,
    #   fileEncoding = "UTF-8"
    # )

    # if (dir.exists(here::here("data-raw"))) {
    #   unlink(here::here("data-raw"), recursive = TRUE)
    # }
    return(tabela_indicadores_num_den)
  }
