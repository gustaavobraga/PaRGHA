
#' Número de Cirurgias Apresentadas por HUF
#'
#' @description Cria o indicador Número total de Cirurgias/Produção Hospitalar Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: CO_GRUPO_PROC, ANO_CMPT, MES_CMPT, CNES, TOTAL_CIRURGIAS_APRESENTADAS.
#'
#' @return Um DataFrame com o número de procedimentos apresentados de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_N_cirurgias_apresentadas(data)}
#'
#' @export
i_N_cirurgias_apresentadas <-
  function(data){

    `%>%` <- dplyr::`%>%`

    i_n_cirurgias = data %>%
      dplyr::filter(CO_GRUPO_PROC==4 ) %>%
      dplyr::group_by( ANO_CMPT,MES_CMPT,CNES ) %>%
      dplyr::summarise(
        N_CIRURGIAS_APRE = sum( TOTAL_CIRURGIAS_APRESENTADAS ),
        .groups = "keep"
      )

    return(i_n_cirurgias)
}
