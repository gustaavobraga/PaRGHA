
#' Taxa de Ocupação Hospitalar por HUF
#'
#' @description Cria o indicador Taxa de Ocupação Hospitalar por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#' @param data_cnes DataFrame obtido pela função create_data_raw_cnes(). Deve conter no mínimo as seguintes variáveis: TP_LEITO, ANO_CMPT, MES_CMPT, CNES, QT_EXIST
#'
#' @return Um DataFrame com a taxa de ocupação hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_taxa_ocupacao_H(data,data_cnes)}
#'
#' @export
i_taxa_ocupacao_H <- function(data, data_cnes){
  `%>%` <- dplyr::`%>%`

  #Numero de dias de permanencia hospitalar
  N_dias_perman_hospitalar_apre = data %>%
    dplyr::mutate(ESPEC = as.numeric(ESPEC)) %>%
    dplyr::filter(ESPEC %in% c(1,2,3,4,5,6,7,87)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_dias_permanencia_hosp = sum( DIAS_PERM ),
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

  dados = N_dias_perman_hospitalar_apre %>%
    dplyr::left_join(N_leitos_hosp_sem_leitos_hosp_dia,
                     by = c("ANO_CMPT","MES_CMPT","CNES"))

  i_taxa_ocupacao_H = dados %>%
    dplyr::summarise(
      N_dias_permanencia_hosp, N_leitos_hosp_sem_leitos_hosp_dia,
      i_taxa_ocupacao_H =
        N_dias_permanencia_hosp / (N_leitos_hosp_sem_leitos_hosp_dia*30),
      .groups = "keep"
    )

  return(i_taxa_ocupacao_H)
}



