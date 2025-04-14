
#' Taxa de partos Cesareos por HUF
#'
#' @description Cria o indicador Taxa de partos Cesareos apresentados na produção hospitalar para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, PROC_REA
#'
#' @return Um DataFrame com a Taxa de partos Cesareos de cada CNES, agrupado por ano e mês. E as colunas que foram  utilizadas no calculo do indicador.
#'
#' @examples \dontrun{i_taxa_de_partos_cesareos(data)}
#'
#' @export
i_taxa_de_partos_cesareos <-
  function(data){
    `%>%` <- dplyr::`%>%`

    i_taxa_de_partos_cesareos = data %>%
      dplyr::summarise(
        i_taxa_de_cesarea = (num_partos_cesareos/num_partos_total)*100,
        .groups = "keep"
      )

    return(i_taxa_de_partos_cesareos)
  }
