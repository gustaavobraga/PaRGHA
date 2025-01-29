
#' Número de Cirurgias Apresentadas por HUF
#'
#' @description Cria o indicador Número total de Cirurgias/Produção Hospitalar Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: CO_GRUPO_PROC, ANO_CMPT, MES_CMPT, CNES, TOTAL_CIRURGIAS_APRESENTADAS.
#'
#' @return Um DataFrame com o número de procedimentos apresentados de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_taxa_ocupacao_H(data)}
#'
#' @export
i_taxa_ocupacao_H <- function(){
  `%>%` <- dplyr::`%>%`

  #Numero de dias de permanencia hospitalar
  N_dias_perman_hospitalar_apre = data %>%
    dplyr::mutate(ESPEC = as.numeric(ESPEC)) %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = sum( DIAS_PERM ),
      .groups = "keep"
    )

  #Numero de dias de permanencia clinica
  N_dias_perman_clinica_apre = data %>%
    dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
    dplyr::filter(CO_GRUPO_PROC == 3) %>% #(Procedimentos clinicos)
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = sum( DIAS_PERM ),
      .groups = "keep"
    )

  #Numero de dias de permanencia cirurgica”
  N_dias_perman_cirurgica_apre = data %>%
    dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
    dplyr::filter(CO_GRUPO_PROC == 4) %>% #(Procedimentos cirurgicos)
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = sum( DIAS_PERM ),
      .groups = "keep"
    )

  # numero total dos Motivos de saida hospitalar da Produção Hospitalar Apresentada
  N_motivos_saida_hospitalar = data %>%
    dplyr::mutate(ESPEC = as.numeric(ESPEC)) %>%
    dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = length( DIAS_PERM ),
      .groups = "keep"
    )
  #numero total dos Motivos de saida hospitalar clinica
  N_motivos_saida_hospitalar_clinica = data %>%
    dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
    dplyr::filter(CO_GRUPO_PROC == 3) %>%
    dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = length( DIAS_PERM ),
      .groups = "keep"
    )
  #numero total dos Motivos de saida hospitalar cirúrgica
  N_motivos_saida_hospitalar_clinica = data %>%
    dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
    dplyr::filter(CO_GRUPO_PROC == 4) %>%
    dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = length( DIAS_PERM ),
      .groups = "keep"
    )

  #Obtendo base CNES
  FaturaSUS.AmbHosp::download_cnes_files(year_start = 2024,
                                         month_start = 3,
                                         year_end = 2024,
                                         month_end = 3,
                                         newer=FALSE,
                                         state_abbr = "CE")

  dbc_dir_path = stringr::str_glue("{tempdir()}\\CNES\\ST")
  dbf_files <- list.files(dbc_dir_path,
                          pattern = "\\.dbc$",
                          full.names = FALSE)

  output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

  #Carrega os dados RD
  raw_SIH_RD <- purrr::map_dfr(output_files_path,
                               read.dbc::read.dbc,
                               as.is=TRUE, .id="file_id")
  #write.csv(data,'./inst/extdata/dados.csv',row.names = FALSE)
  return(raw_SIH_RD)
}



