#POP-19: Taxa de Ocupação Hospitalar = Número de dias de permanência hospitalar/ Número de leitos sem leitos de Hospital-dia


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
    dplyr::filter(CO_GRUPO_PROC == 3) %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = sum( DIAS_PERM ),
      .groups = "keep"
    )

  #Numero de dias de permanencia cirurgica”
  N_dias_perman_cirurgica_apre = data %>%
    dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
    dplyr::filter(CO_GRUPO_PROC == 4) %>%
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
  data_cnes = create_data_raw_cnes(
    year_start = 2024,
    month_start = 1,
    year_end = 2024,
    month_end = 2,
    state_abbr = "CE",
    save_path = tempdir()
  )

  #Número de leitos hospitalares, sem os leitos de Hospital-Dia
  N_leitos_hospitalar = data %>%
    dplyr::mutate(across(c(TP_LEITO), as.numeric))%>%
    dplyr::filter(TP_LEITO != 7) %>%
    dplyr::group_by(CNES) %>%
    dplyr::summarise(
      Frequencia = sum( QT_EXIST ),
      .groups = "keep"
    )


  #write.csv(data,'./inst/extdata/dados.csv',row.names = FALSE)
  return(raw_SIH_RD)
}



