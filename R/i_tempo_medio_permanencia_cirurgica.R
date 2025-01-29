
#' Tempo Medio de Permanencia Cirurgica por HUF
#'
#' @description Cria o indicador Tempo Médio de Permanência Cirurgica para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, DIAS_PERM, ESPEC
#'
#' @return Um DataFrame com o tempo medio de permanencia cirurgica de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_tempo_medio_permanencia_cirurgica(data)}
#'
#' @export
i_tempo_medio_permanencia_cirurgica <-
  function(data){
    `%>%` <- dplyr::`%>%`

    #Numero de dias de permanencia cirurgica
    N_dias_perman_cirurgica = data %>%
      dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
      dplyr::filter(CO_GRUPO_PROC == 4) %>% #(Procedimentos clinicos)
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_dias_perman = sum( DIAS_PERM ),
        .groups = "keep"
      )

    #numero total dos Motivos de saida cirurgica
    N_motivos_saida_cirurgica = data %>%
      dplyr::mutate(across(c(CO_GRUPO_PROC, ESPEC), as.numeric))%>%
      dplyr::filter(CO_GRUPO_PROC == 4) %>%
      dplyr::filter(SELECAO_MOTIVO_SAIDA == 'sim') %>%
      dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
      dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
      dplyr::summarise(
        num_motivos_saída_cirurgica = length( DIAS_PERM ),
        .groups = "keep"
      )

    TMP_cirurgica <- N_dias_perman_cirurgica %>%
      dplyr::left_join(N_motivos_saida_cirurgica, by = c("ANO_CMPT","MES_CMPT","CNES"))


    i_tempo_medio_permanencia_cirurgica = TMP_cirurgica %>%
      dplyr::summarise(
        i_tempo_medio_permanencia_cirurgica = num_dias_perman/num_motivos_saída_cirurgica,
        .groups = "keep"
      )

    return(i_tempo_medio_permanencia_cirurgica)
  }
