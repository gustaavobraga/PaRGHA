
#' Número de Internações Hospitalares por HUF
#'
#' @description Cria o indicador Número total de Internações Hospitalar Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, TIPO_AIH
#'
#' @return Um DataFrame com o número de Internações Hospitalares de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_N_internacoes_hospitalares(data)}
#'
#' @export
i_N_internacoes_hospitalares <-
  function(data){
    `%>%` <- dplyr::`%>%`

    i_n_internacoes_H = data %>%
      dplyr::group_by( ANO_CMPT,MES_CMPT,CNES ) %>%
      dplyr::summarise(
        N_Internacoes_H = length( TIPO_AIH ),
        .groups = "keep"
      )

    return(i_n_internacoes_H)
}
