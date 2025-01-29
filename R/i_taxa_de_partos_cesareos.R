
#' Taxa de partos Cesareos por HUF
#'
#' @description Cria o indicador Taxa de partos Cesareos apresentados na produção hospitalar para cada estabelecimento de saúde (CNES), agrupados por ano e mês.
#'
#' @param data DataFrame obtido pela função x. Deve conter no mínimo as seguintes variáveis: ANO_CMPT, MES_CMPT, CNES, PROC_REA
#'
#' @return Um DataFrame com a Taxa de partos Cesareos de cada CNES, agrupado por ano e mês.
#'
#' @examples \dontrun{i_taxa_de_partos_cesareos(data)}
#'
#' @export
i_taxa_de_partos_cesareos <-
  function(data){
    `%>%` <- dplyr::`%>%`

    partos_cesareos = c("0411010026","0411010034","0411010042")
    partos = c("0411010026","0411010034","0411010042",
               "0310010039","0310010047","0310010055")

    num_partos_cesareos = data %>%
      dplyr::group_by( ANO_CMPT,MES_CMPT,CNES ) %>%
      dplyr::filter(PROC_REA %in% partos_cesareos) %>%
      dplyr::summarise(
        num_partos_cesareos = length(CNES),
        .groups = "keep"
      )
    num_partos_total = data %>%
      dplyr::group_by( ANO_CMPT,MES_CMPT,CNES ) %>%
      dplyr::filter(PROC_REA %in% partos) %>%
      dplyr::summarise(
        num_partos_total = length(CNES),
        .groups = "keep"
      )

    partos <- num_partos_total %>%
      dplyr::left_join(num_partos_cesareos, by = c("ANO_CMPT","MES_CMPT","CNES"))


    i_taxa_de_partos_cesareos = partos %>%
      dplyr::summarise(
        i_taxa_de_cesarea = (num_partos_cesareos/num_partos_total)*100,
        .groups = "keep"
      )

    return(i_taxa_de_partos_cesareos)
  }
