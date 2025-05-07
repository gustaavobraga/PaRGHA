#' Numerador e Denominador dos Indicadores
#'
#' @description Cria o numerador e o denominador necessários para o cálculo dos indicadores.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#' @param data_cnes DataFrame obtido pela função create_data_raw_cnes(). Deve conter no mínimo as seguintes variáveis: TP_LEITO, ANO_CMPT, MES_CMPT, CNES, QT_EXIST
#'
#' @return Um DataFrame com os numeradores e denominadores dos indicadores
#'
#' @examples \dontrun{create_numerador_denominador(data,data_cnes)}
#'
#' @export
create_numerador_denominador <- function(data,data_cnes){
  `%>%` <- dplyr::`%>%`

  partos_cesareos = c("0411010026","0411010034","0411010042")

  partos = c("0411010026","0411010034","0411010042",
             "0310010039","0310010047","0310010055")

  output = data %>%
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

      #i_taxa_de_partos_cesareos
      num_partos_cesareos = sum(PROC_REA %in% partos_cesareos, na.rm = TRUE),
      num_partos_total = sum(PROC_REA %in% partos, na.rm = TRUE),

      #i_taxa_ocupacao_UTI
      N_dias_em_UTI =
        sum(UTI_MES_TO[MARCA_UTI %in%
                         c(51,52,74,75,76,77,78,79,80,81,82,83,85)],
            na.rm = TRUE),

      .groups = "keep"
    )

  especialidade_leito =
    c("51","52","74","75","76","77","78","79","80","81","82","83","85")

  output_cnes = data_cnes %>%
    dplyr::group_by(ANO_CMPT, MES_CMPT, CNES) %>%
    dplyr::summarise(
      #i_taxa_ocupacao_UTI
      N_leitos_UTI =
        sum(QT_EXIST[CODLEITO %in% especialidade_leito], na.rm = TRUE),

      #i_taxa_ocupacao_H, i_giro_de_leito
      N_leitos_hosp_sem_leitos_hosp_dia =
        sum(QT_EXIST[TP_LEITO != "7"], na.rm = TRUE),.groups = "keep"
    ) %>%
    dplyr::mutate(
      #Substituir os valores de N_leitos_UTI iguais a 0 por NA
      N_leitos_UTI = dplyr::na_if(N_leitos_UTI, 0)
    )

  output = dplyr::left_join(output, output_cnes,
                            by = c("ANO_CMPT", "MES_CMPT", "CNES"))
  return(output)

}
