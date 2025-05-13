
#' Download dos dados do CNES que será utilizada pelos indicadores
#'
#' @description Realiza o download dos dados do CNES para um dos sistemas especificados (LT = Leitos, HB = Habilitações, EQ = Equipamentos) e seleciona apenas as colunas necessárias.
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa
#' @param type_data string. O padrao e "LT". Valores aceitos ("LT", "HB", "EQ"). O tipo da base de dados do CNES que sera baixado.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os dados dos estabelecimentos de saude
#'
#'
#' @examples
#' \dontrun{
#'   dados = create_data_raw_cnes(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 2,
#'     state_abbr = "all",
#'     type_data = "LT",
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
           type_data = "LT",
           save_path = tempdir()){

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

    #Obtendo base CNES-LT .dbc
    PaPAHR::download_cnes_files(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      newer=F,
      state_abbr = state_abbr,
      type_data=type_data)


    dbc_dir_path = stringr::str_glue("{save_path}\\CNES\\{type_data}")
    dbf_files <- list.files(dbc_dir_path,
                            pattern = "\\.dbc$",
                            full.names = FALSE)

    output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

    #Carrega os dados
    raw_CNES <- purrr::map_dfr(output_files_path,
                                 read.dbc::read.dbc,
                                 as.is=TRUE, .id="file_id")


    #Selecionar as colunas com base no valor de type_datab
    data_CNES <- switch(
      type_data,
      "LT" = raw_CNES %>% dplyr::select(
        "CODUFMUN","CNES","COMPETEN","TP_LEITO","CODLEITO","QT_EXIST"
      ),
      "HB" = raw_CNES %>% dplyr::select("SGRUPHAB", "CNES", "COMPETEN"),
      "EQ" = raw_CNES %>% dplyr::select(
        "CNES","COMPETEN","TIPEQUIP","CODEQUIP","QT_EXIST","QT_USO"
      ),
    )

    #Formatando a coluna Data
    data_CNES = data_CNES %>%
      dplyr::mutate(
        ANO_CMPT = stringr::str_sub(COMPETEN, 1, 4),
        MES_CMPT = stringr::str_sub(COMPETEN, 5, 6)
      ) %>% dplyr::select(-COMPETEN)

    data_CNES = dplyr::filter(data_CNES, CNES %in% CNES_EBSERH)
    return(data_CNES)
  }
