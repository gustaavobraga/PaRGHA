
#' Taxa de Ocupação em UTI por HUF
#'
#' @description Cria o indicador Taxa de Ocupação em UTI por HUF Apresentadas para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ESPEC, ANO_CMPT, MES_CMPT, CNES, DIAS_PERM
#' @param data_cnes DataFrame obtido pela função create_data_raw_cnes(). Deve conter no mínimo as seguintes variáveis: TP_LEITO, ANO_CMPT, MES_CMPT, CNES, QT_EXIST
#'
#' @return Um DataFrame com a taxa de ocupação hospitalar de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_taxa_ocupacao_H(data,data_cnes)}
#'
#' @export
i_taxa_ocupacao_UTI <- function(data, data_cnes){
  `%>%` <- dplyr::`%>%`

  #número de dias de permanência em UTI
  N_dias_em_UTI = data %>%
    dplyr::mutate(MARCA_UTI = as.numeric(MARCA_UTI)) %>%
    dplyr::filter(MARCA_UTI %in% c(51,52,74,75,76,77,78,79,80,81,82,83,85)) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_dias_em_UTI = sum( UTI_MES_TO ),
      .groups = "keep"
    )

  #número de leitos de UTI
  especialidade_leito = c("74","75","76","77","78","79","80","81","82","84","85","86")

  N_leitos_UTI = data_cnes %>%
    dplyr::filter( CODLEITO %in% especialidade_leito ) %>%
    dplyr::group_by(ANO_CMPT,MES_CMPT,CNES) %>%
    dplyr::summarise(
      N_leitos_UTI = sum( QT_EXIST ),
      .groups = "keep"
    )

  dados = N_dias_em_UTI %>%
    dplyr::left_join(N_leitos_UTI,
                     by = c("ANO_CMPT","MES_CMPT","CNES"))

  i_taxa_ocupacao_H = dados %>%
    dplyr::summarise(
      N_dias_em_UTI, N_leitos_UTI,
      i_taxa_ocupacao_UTI =
        N_dias_em_UTI / (N_leitos_UTI*30),
      .groups = "keep"
    )

  return(i_taxa_ocupacao_H)
}



