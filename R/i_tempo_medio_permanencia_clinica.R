
#' Tempo Medio de Permanencia Clinica por HUF
#'
#' @description Cria o indicador  Tempo Médio de Permanência clinica para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, DIAS_PERM, ESPEC
#'
#' @return Um DataFrame com o tempo medio de permanencia clinica de cada CNES, agrupado por ano e mês.  E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_tempo_medio_permanencia_clinica(data)}
#'
#' @export
i_tempo_medio_permanencia_clinica <-
  function(data){
    `%>%` <- dplyr::`%>%`

    i_tempo_medio_permanencia_clinica = data %>%
      dplyr::summarise(
        i_tempo_medio_permanencia_clinica = num_dias_perman_clinica/num_motivos_saida_clinica,
        .groups = "keep"
      )

    return(i_tempo_medio_permanencia_clinica)
  }
