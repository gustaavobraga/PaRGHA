
#' Retorna a base de dados com todos os indicadores
#'
#' @description Realiza o download dos arquivos DBC ('SIH-RD', 'SIH-RJ' e 'SIH-SP'), combina as três bases de dados em uma única estrutura, seleciona apenas as variáveis relevantes, cria novas variáveis e retorna uma base de dados contendo exclusivamente as variáveis que serão utilizadas nos cálculos dos indicadores
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os indicadores
#'
#'
#' @examples
#' \dontrun{
#'   dados = indicadores(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 6,
#'     state_abbr = "CE",
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
           state_abbr,
           save_path = tempdir()){

    `%>%` <- dplyr::`%>%`

    dados = create_data_raw(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
    data_cnes = create_data_raw_cnes(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )


    #> Número de Cirurgias Apresentadas por HUF ----
    i_1 = i_N_cirurgias_apresentadas(dados)
    #> Número de Internações Hospitalares por HUF ----
    i_2 = i_N_internacoes_hospitalares(dados)
    #> Taxa de partos Cesareos por HUF ----
    i_3 = i_taxa_de_partos_cesareos(dados)

    #> i_taxa_ocupacao_H ----
    i_4 = i_taxa_ocupacao_H(dados,data_cnes)

    #> Tempo Medio de Permanencia Cirurgica por HUF ----
    i_5 = i_tempo_medio_permanencia_cirurgica(dados)
    #> Tempo Medio de Permanencia Clinica por HUF ----
    i_6 = i_tempo_medio_permanencia_clinica(dados)
    #> Tempo Medio de Permanencia Hospitalar por HUF ----
    i_7 = i_tempo_medio_permanencia_hospitalar(dados)

    # Lista com as suas tabelas
    tabelas = list(i_1, i_2, i_3, i_4, i_5, i_6, i_7)

    # full join de todas as tabelas com base em ano, mes e CNES
    tabela_indicadores = purrr::reduce(tabelas, dplyr::full_join,
                                by = c("ANO_CMPT", "MES_CMPT", "CNES"))

    return(tabela_indicadores)
  }
