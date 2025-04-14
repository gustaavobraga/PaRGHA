
#' Taxa de Ocupação Hospitalar por HUF
#'
#' @description Cria o indicador Taxa de Ocupação Hospitalar por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#'
#' @return Um DataFrame com a taxa de ocupação hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_taxa_ocupacao_H(data)}
#'
#' @export
i_taxa_ocupacao_H <- function(data){
  `%>%` <- dplyr::`%>%`

  i_taxa_ocupacao_H = data %>%
    dplyr::summarise(
      i_taxa_ocupacao_H =
        num_dias_perman_hosp / (N_leitos_hosp_sem_leitos_hosp_dia*30),
      .groups = "keep"
    )

  return(i_taxa_ocupacao_H)
}



