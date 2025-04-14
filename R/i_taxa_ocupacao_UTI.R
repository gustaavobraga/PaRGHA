
#' Taxa de Ocupação em UTI por HUF
#'
#' @description Cria o indicador Taxa de Ocupação em UTI por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#'
#' @return Um DataFrame com a taxa de ocupação hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_taxa_ocupacao_H(data,data_cnes)}
#'
#' @export
i_taxa_ocupacao_UTI <- function(data){
  `%>%` <- dplyr::`%>%`

  i_taxa_ocupacao_H = data %>%
    dplyr::summarise(
      i_taxa_ocupacao_UTI =
        N_dias_em_UTI / (N_leitos_UTI*30),
      .groups = "keep"
    )

  return(i_taxa_ocupacao_H)
}



