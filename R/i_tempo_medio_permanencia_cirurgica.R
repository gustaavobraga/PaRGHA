
#' Tempo Medio de Permanencia Cirurgica por HUF
#'
#' @description Cria o indicador Tempo Médio de Permanência Cirurgica para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, DIAS_PERM, ESPEC
#'
#' @return Um DataFrame com o tempo medio de permanencia cirurgica de cada CNES, agrupado por ano e mês.  E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_tempo_medio_permanencia_cirurgica(data)}
#'
#' @export
i_tempo_medio_permanencia_cirurgica <-
  function(data){
    `%>%` <- dplyr::`%>%`

    i_tempo_medio_permanencia_cirurgica = data %>%
      dplyr::summarise(
        i_tempo_medio_permanencia_cirurgica = num_dias_perman_cirurgica/num_motivos_saida_cirurgica,
        .groups = "keep"
      )

    return(i_tempo_medio_permanencia_cirurgica)
  }
