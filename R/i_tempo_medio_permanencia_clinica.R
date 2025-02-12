
#' Tempo Medio de Permanencia Clinica por HUF
#'
#' @description Cria o indicador  Tempo Médio de Permanência clinica para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, DIAS_PERM, ESPEC
#'
#' @return Um DataFrame com o tempo medio de permanencia clinica de cada CNES, agrupado por ano e mês.  E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_tempo_medio_permanencia_clinica(data)}
#'
#' @export
i_tempo_medio_permanencia_clinica <-
  function(data){
    `%>%` <- dplyr::`%>%`

    #Numero de dias de permanencia clinica
    N_dias_perman_clinica = data %>%
      dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
      dplyr::filter(CO_GRUPO_PROC == 3) %>% #(Procedimentos clinicos)
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_dias_perman = sum( DIAS_PERM ),
        .groups = "keep"
      )
    #numero total dos Motivos de saida hospitalar clinica
    N_motivos_saida_clinica = data %>%
      dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
      dplyr::filter(CO_GRUPO_PROC == 3) %>%
      dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_motivos_saída_clinica = length( DIAS_PERM ),
        .groups = "keep"
      )

    TMP_clinica <- N_dias_perman_clinica %>%
      dplyr::left_join(N_motivos_saida_clinica, by = c("ANO_CMPT","MES_CMPT","CNES"))


    i_tempo_medio_permanencia_clinica = TMP_clinica %>%
      dplyr::summarise(
        num_dias_perman, num_motivos_saída_clinica,
        i_tempo_medio_permanencia_clinica = num_dias_perman/num_motivos_saída_clinica,
        .groups = "keep"
      )

    return(i_tempo_medio_permanencia_clinica)
  }
