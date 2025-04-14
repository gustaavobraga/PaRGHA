
#' Giro de Leito da Produção Hospitalar por HUF
#'
#' @description Cria o indicador Giro de Leito da Produção Hospitalar por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#'
#' @return Um DataFrame com o Giro de Leito da Produção Hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_giro_de_leito(data)}
#'
#' @export
i_giro_de_leito <- function(data){
  `%>%` <- dplyr::`%>%`

  i_giro_de_leito = data %>%
    dplyr::summarise(
      i_giro_de_leito = num_motivos_saida_hosp/N_leitos_hosp_sem_leitos_hosp_dia,
      .groups = "keep"
    )

  return(i_giro_de_leito)

}
