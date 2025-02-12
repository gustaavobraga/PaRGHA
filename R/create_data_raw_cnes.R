
#' Cria a base de dados do CNES que será utilizada pelos indicadores
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
#' @return Um DataFrame com os dados de RD, RJ e SP
#'
#'
#' @examples
#' \dontrun{
#'   create_data_raw_cnes(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 1,
#'     state_abbr = "CE",
#'     save_path = tempdir()
#'  )
#' }
#'
#' @export
create_data_raw_cnes <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           state_abbr = "all",
           save_path = tempdir()){

    `%>%` <- dplyr::`%>%`

    #Obtendo base CNES-LT .dbc
    FaturaSUS.AmbHosp::download_cnes_files(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      newer=F,
      state_abbr = state_abbr,
      type_data="LT")

    dbc_dir_path = stringr::str_glue("{save_path}\\CNES\\LT")
    dbf_files <- list.files(dbc_dir_path,
                            pattern = "\\.dbc$",
                            full.names = FALSE)

    output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

    #Carrega os dados
    raw_CNES_LT <- purrr::map_dfr(output_files_path,
                                 read.dbc::read.dbc,
                                 as.is=TRUE, .id="file_id")

    data_CNES = raw_CNES_LT %>%
      dplyr::select("CODUFMUN","CNES","COMPETEN","TP_LEITO","CODLEITO","QT_EXIST")
    #Formatando a coluna Data
    data_CNES = data_CNES %>%
      dplyr::mutate(
        ANO_CMPT = stringr::str_sub(COMPETEN, 1, 4),
        MES_CMPT = stringr::str_sub(COMPETEN, 5, 6)
      ) %>% dplyr::select(-COMPETEN)

    return(data_CNES)
  }
