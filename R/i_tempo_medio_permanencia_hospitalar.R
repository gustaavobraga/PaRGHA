
#' Tempo Medio de Permanencia Hospitalar por HUF
#'
#' @description Cria o indicador  Tempo Médio de Permanência Hospitalar para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, DIAS_PERM, ESPEC
#'
#' @return Um DataFrame com o tempo medio de permanencia hospitalar de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_tempo_medio_permanencia_hospitalar(data)}
#'
#' @export
i_tempo_medio_permanencia_hospitalar <-
  function(data){
    `%>%` <- dplyr::`%>%`

    #Numero de dias de permanencia hospitalar
    N_dias_permanencia_hospitalar = data %>%
      dplyr::mutate(ESPEC = as.numeric(ESPEC)) %>%
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_dias_perman = sum( DIAS_PERM ),
        .groups = "keep"
      )
    # numero total dos Motivos de saida hospitalar
    N_motivos_saida_hospitalar = data %>%
      dplyr::mutate(ESPEC = as.numeric(ESPEC)) %>%
      dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_motivos_saída_hospitalar = length( DIAS_PERM ),
        .groups = "keep"
      )

    TMP <- N_dias_permanencia_hospitalar %>%
      dplyr::left_join(N_motivos_saida_hospitalar, by = c("ANO_CMPT","MES_CMPT","CNES"))


    i_tempo_medio_permanencia_hospitalar = TMP %>%
      dplyr::summarise(
        i_tempo_medio_permanencia_hospitalar = num_dias_perman/num_motivos_saída_hospitalar,
        .groups = "keep"
      )

    return(i_tempo_medio_permanencia_hospitalar)
  }
