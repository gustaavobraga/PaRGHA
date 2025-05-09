
#' Faz o calculo dos indicadores
#'
#' @description Cria o indicador Giro de Leito da Produção Hospitalar por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#'
#' @return um
#'
#' @examples \dontrun{create_indicador(data)}
#'
#' @export
create_indicador <- function(data){
  `%>%` <- dplyr::`%>%`

  indicadores <- data %>%
    summarise(
      i_taxa_de_cesarea = (num_partos_cesareos / num_partos_total) * 100,
      i_taxa_ocupacao_H = num_dias_perman_hosp / (N_leitos_hosp_sem_leitos_hosp_dia * 30),
      i_taxa_ocupacao_UTI = N_dias_em_UTI / (N_leitos_UTI * 30),
      i_tempo_medio_permanencia_cirurgica = num_dias_perman_cirurgica / num_motivos_saida_cirurgica,
      i_tempo_medio_permanencia_clinica = num_dias_perman_clinica / num_motivos_saida_clinica,
      i_tempo_medio_permanencia_hospitalar = num_dias_perman_hosp / num_motivos_saida_hosp,
      i_giro_de_leito = num_motivos_saida_hosp / N_leitos_hosp_sem_leitos_hosp_dia,
      i_n_internacoes_H = length( TIPO_AIH ),
      .groups = "keep"
    )


  i_n_cirurgias = data %>%
    dplyr::filter(CO_GRUPO_PROC==4) %>%
    dplyr::summarise(
      N_CIRURGIAS_APRE = sum( TOTAL_CIRURGIAS_APRESENTADAS ),
      .groups = "keep"
    )

  return(indicadores)
}
