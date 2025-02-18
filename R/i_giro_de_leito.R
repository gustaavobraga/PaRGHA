
#' Giro de Leito da Produção Hospitalar por HUF
#'
#' @description Cria o indicador Giro de Leito da Produção Hospitalar por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#' @param data_cnes DataFrame obtido pela função create_data_raw_cnes(). Deve conter no mínimo as seguintes variáveis: TP_LEITO, ANO_CMPT, MES_CMPT, CNES, QT_EXIST
#'
#' @return Um DataFrame com o Giro de Leito da Produção Hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_giro_de_leito(data,data_cnes)}
#'
#' @export
i_giro_de_leito <- function(data, data_cnes){
  `%>%` <- dplyr::`%>%`

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


  #número de leitos hospitalares, sem os leitos de Hospital-Dia
  N_leitos_hosp_sem_leitos_hosp_dia = data_cnes %>%
    dplyr::filter( TP_LEITO != "7" ) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_leitos_hosp_sem_leitos_hosp_dia = sum( QT_EXIST ),
      .groups = "keep"
    )

  dados = N_motivos_saida_hospitalar %>%
    dplyr::left_join(N_leitos_hosp_sem_leitos_hosp_dia,
                     by = c("ANO_CMPT","MES_CMPT","CNES"))

  i_giro_de_leito = dados %>%
    dplyr::summarise(
      num_motivos_saída_hospitalar,N_leitos_hosp_sem_leitos_hosp_dia,
      i_giro_de_leito = num_motivos_saída_hospitalar/N_leitos_hosp_sem_leitos_hosp_dia,
      .groups = "keep"
    )

  return(i_giro_de_leito)

}
