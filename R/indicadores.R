
#' Cria os indicadores dos estabelecimentos da EBSERH
#'
#' @description Cria os indicadores dos estabelecimentos da EBSERH com base nos dados do SIH e do CNES-LT passados como parametro.
#'
#' @param data_SIH DataFrame. Objeto retornado pela funcao get_data()
#' @param data_CNES DataFrame. Objeto retornado pela funcao get_data_CNES(type_data = "LT",...)
#' @param labels_CNES Logical. O padrao é TRUE. Criar uma coluna com o nome dos estabelecimentos de saude.
#'
#' @return Um DataFrame com os indicadores
#'
#'
#' @examples
#' \dontrun{
#'   dados = indicadores(
#'     data_SIH = data_SIH,
#'     data_CNES = data_CNES,
#'     labels_CNES = TRUE
#'   )
#' }
#'
#' @export
indicadores <- function(data_SIH,data_CNES,labels_CNES=TRUE){
  `%>%` <- dplyr::`%>%`

  #Cria as variaveis que serao utilizadas no calculo dos indicadores
  dados = create_numerador_denominador(data_SIH, data_CNES)

  #Realiza o calculo dos indicadores
  indicadores = dados %>%
    dplyr::summarise(
      i_taxa_de_cesarea = (num_partos_cesareos / num_partos_total) * 100,
      i_taxa_ocupacao_H =
        num_dias_perman_hosp / (N_leitos_hosp_sem_leitos_hosp_dia * 30),
      i_taxa_ocupacao_UTI = N_dias_em_UTI / (N_leitos_UTI * 30),
      i_tempo_medio_permanencia_cirurgica =
        num_dias_perman_cirurgica / num_motivos_saida_cirurgica,
      i_tempo_medio_permanencia_clinica =
        num_dias_perman_clinica / num_motivos_saida_clinica,
      i_tempo_medio_permanencia_hospitalar =
        num_dias_perman_hosp / num_motivos_saida_hosp,
      i_giro_de_leito = num_motivos_saida_hosp / N_leitos_hosp_sem_leitos_hosp_dia,
      .groups = "keep"
    )

  #ADD a coluna com o nome dos Hospitais
  if(labels_CNES){
    indicadores = labels(indicadores,'cnes')
  }

  #Cria uma base com as colunas dos numeradores, denominadores e indicadores
  dados = dplyr::left_join(
    indicadores, dados,
    by = c("ano", "mes", "cnes")) %>%
  dplyr::ungroup()

  dados <- dados %>%
    dplyr::mutate(
      #Add um 0 entre ano e mes se mes tiver só um caracter
      mes_formatado = stringr::str_pad(mes, width = 2, side = "left", pad = "0"),
      key_data = as.integer(paste0(ano, mes_formatado))
    ) %>%
    dplyr::relocate(key_data, .before = ano) %>%
    dplyr::select(-mes_formatado)

  #Coloca o nome das colunas em minusculo
  names(dados) <- tolower(names(dados))

  #Salva a base em um diretorio
  # write.csv(
  #   tabela_indicadores_empilhada,
  #   "./inst/extdata/dados2.csv",
  #   row.names = FALSE,
  #   fileEncoding = "UTF-8"
  # )

  # if (dir.exists(here::here("data-raw"))) {
  #   unlink(here::here("data-raw"), recursive = TRUE)
  # }
  return(dados)
}
