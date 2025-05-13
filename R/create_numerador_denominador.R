
#' Cria os Numerador e Denominador dos Indicadores
#'
#' @description Cria o numerador e o denominador necessarios para o calculo dos indicadores.
#'
#' @param data_SIH DataFrame. Objeto retornado pela funcao get_data()
#' @param data_CNES DataFrame. Objeto retornado pela funcao create_data_raw_cnes()
#'
#' @return Um DataFrame com os numeradores e denominadores dos indicadores
#'
#' @examples \dontrun{create_numerador_denominador(data_SIH,data_CNES)}
#'
#' @export
create_numerador_denominador <- function(data_SIH, data_CNES) {
  `%>%` <- dplyr::`%>%`

  especialidade_leito =
    c("51","52","74","75","76","77","78","79","80","81","82","83","85")
  partos = c("0411010026","0411010034","0411010042",
             "0310010039","0310010047","0310010055")
  partos_cesareos = c("0411010026","0411010034","0411010042")

  output = data_SIH %>%
    dplyr::mutate(
      dplyr::across(c(CO_GRUPO_PROC, ESPEC, MARCA_UTI), as.numeric)
    ) %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT, MES_CMPT, CNES) %>%
    dplyr::summarise(
      #i_tempo_medio_permanencia_hospitalar
      num_dias_perman_hosp = sum(DIAS_PERM, na.rm = TRUE),
      num_motivos_saida_hosp = sum(SELECAO_MOTIVO_SAIDA == 'sim', na.rm = TRUE),
      #i_tempo_medio_permanencia_clinica
      num_dias_perman_clinica =
        sum(DIAS_PERM[CO_GRUPO_PROC == 3], na.rm = TRUE),
      num_motivos_saida_clinica =
        sum(SELECAO_MOTIVO_SAIDA[CO_GRUPO_PROC == 3] == 'sim', na.rm = TRUE),

      #i_tempo_medio_permanencia_cirurgica
      num_dias_perman_cirurgica =
        sum(DIAS_PERM[CO_GRUPO_PROC == 4], na.rm = TRUE),
      num_motivos_saida_cirurgica =
        sum(SELECAO_MOTIVO_SAIDA[CO_GRUPO_PROC == 4] == 'sim', na.rm = TRUE),
      .groups = "keep"
    )

  output2 = data_SIH %>%
    dplyr::group_by(ANO_CMPT, MES_CMPT, CNES) %>%
    dplyr::summarise(
      #i_taxa_de_partos_cesareos
      num_partos_cesareos = sum(PROC_REA %in% partos_cesareos, na.rm = TRUE),
      num_partos_total = sum(PROC_REA %in% partos, na.rm = TRUE),

      #i_taxa_ocupacao_UTI
      N_dias_em_UTI = sum(UTI_MES_TO[MARCA_UTI %in% especialidade_leito],
                          na.rm = TRUE),

      #Numero de Cirurgias Apresentadas por HUF
      N_CIRURGIAS_APRE = sum(TOTAL_CIRURGIAS_APRESENTADAS[CO_GRUPO_PROC == "4"],
                             na.rm = TRUE),
      #Numero de Internacoes Hospitalares por HUF
      N_Internacoes_H = length(TIPO_AIH),
      .groups = "keep"
    )

  output_cnes = data_CNES %>%
    dplyr::group_by(ANO_CMPT, MES_CMPT, CNES) %>%
    dplyr::summarise(
      #i_taxa_ocupacao_UTI
      N_leitos_UTI =
        sum(QT_EXIST[CODLEITO %in% especialidade_leito], na.rm = TRUE),

      #i_taxa_ocupacao_H, i_giro_de_leito
      N_leitos_hosp_sem_leitos_hosp_dia = sum(QT_EXIST[TP_LEITO != "7"], na.rm = TRUE),
      .groups = "keep"
    ) %>%
    dplyr::mutate(
      #Substituir os valores de N_leitos_UTI iguais a 0 por NA
      N_leitos_UTI = dplyr::na_if(N_leitos_UTI, 0)
    )

  output = output %>%
    dplyr::left_join(output2, by = c("ANO_CMPT", "MES_CMPT", "CNES")) %>%
    dplyr::left_join(output_cnes, by = c("ANO_CMPT", "MES_CMPT", "CNES"))

  return(output)
}
