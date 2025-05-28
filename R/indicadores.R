
#' Cria os indicadores dos estabelecimentos da EBSERH
#'
#' @description Cria os indicadores dos estabelecimentos da EBSERH com base nos dados do SIH e do CNES-LT passados como parametro.
#'
#' @param data_SIH DataFrame. Objeto retornado pela funcao get_data()
#' @param data_CNES DataFrame. Objeto retornado pela funcao get_data_CNES(type_data = "LT",...)
#'
#' @return Um DataFrame com os indicadores
#'
#'
#' @examples
#' \dontrun{
#'   dados = indicadores(
#'     data_SIH = data_SIH,
#'     data_CNES = data_CNES
#'   )
#' }
#'
#' @export
indicadores <- function(data_SIH,data_CNES){
  `%>%` <- dplyr::`%>%`

  #Cria as variaveis que serao utilizadas no calculo dos indicadores
  data = create_numerador_denominador(data_SIH, data_CNES)

  #Realiza o calculo dos indicadores
  indicadores = data %>%
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
  indicadores = indicadores %>%
    dplyr::mutate(
      Name_CNES = dplyr::case_match(
        CNES,
        "0" ~ NA,
        "0000396" ~ "HC da Univ. Federal de Pernambuco (HC-UFPE)",
        "0002534" ~ "HU da Universidade Federal de Sergipe (HU-UFS)",
        "0003816" ~ "HU Professor Edgard Santos (Hupes-UFBA)",
        "0004731" ~ "Maternidade Clim\u00e9rio de Oliveira (MCO-UFBA)",
        "0009709" ~ "HU Maria Aparecida Pedrossian (Humap-UFMS)",
        "0010510" ~ "HU da Universidade de Bras\u00edlia (HUB-UnB)",
        "0012505" ~ "HU Ant\u00f4nio Pedro (HUAP-UFF)",
        "0027049" ~ "HC da Univ. Federal de Minas Gerais (HC-UFMG)",
        "2006197" ~ "HU Professor Alberto Antunes (HUPAA-UFAL)",
        "2017644" ~ "HU Get\u00falio Vargas (HUGV-UFAM)",
        "2146339" ~ "Ambulat\u00f3rio Am\u00e9lio Marques (AAM-HC-UFU)",
        "2146355" ~ "Hospital de Cl\u00ednicas de Uberl\u00e2ndia (HC-UFU)",
        "2146371" ~ "Unidade Di\u00e1lise Hemodi\u00e1lise (UDH-HC-UFU)",
        "2206595" ~ "HC da Univ. Feder. do Tri\u00e2ngulo Mineiro (HC-UFTM)",
        "2218798" ~ "HU da Univ. Federal de Juiz de Fora (HU-UFJF)",
        "2244306" ~ "HU da Univ. Federal de Santa Maria (HUSM-UFSM)",
        "2252694" ~ "HE da Univ. Federal de Pelotas (HE-UFPel)",
        "2295415" ~ "HU Gaffree e Guinle (HUGG-Unirio)",
        "2332981" ~ "HU Jo\u00e3o de Barros Barreto (HUJBB-UFPA)",
        "2338424" ~ "HC da Universidade Federal de Goi\u00e1s (HC-UFG)",
        "2384299" ~ "Complexo do HC da Univ.Fed. do Paran\u00e1 (CHC-UFPR)",
        "2400243" ~ "HU Lauro Wanderley (HULW-UFPB)",
        "2409208" ~ "Maternidade Escola Janu\u00e1rio Cicco (Mejc-UFRN)",
        "2481286" ~ "Maternidade Escola Assis Chateaubriand (Meac-UFC)",
        "2504502" ~ "HU J\u00falio Maria Bandeira de Mello (HUJB-UFCG)",
        "2561492" ~ "HU Walter Cant\u00eddio (HUWC-UFC)",
        "2640244" ~ "Maternidade Victor F. do Amaral (MVFA-UFPR)",
        "2653982" ~ "Hospital Universit\u00e1rio Onofre Lopes (Huol-UFRN)",
        "2655411" ~ "Hospital Universit\u00e1rio J\u00falio Muller (HUJM-UFMT)",
        "2676060" ~ "Hospital Universit\u00e1rio Alcides Carneiro (HUAC-UFCG)",
        "2694751" ~ "HU Bettina Ferro de Souza (HUBFS-UFPA)",
        "2707675" ~ "HU Dr. Miguel Riet Correa Junior (HU-Furg)",
        "2710935" ~ "HU da Univ. Federal da Grande Dourados (HU-UFGD)",
        "2726653" ~ "HU da Universidade Federal do Maranh\u00e3o (HU-UFMA)",
        "3157245" ~ "HU da Univ. Federal de Santa Catarina (HU-UFSC)",
        "3285391" ~ "HU da Universidade Federal do Piau\u00ed (HU-UFPI)",
        "3432076" ~ "HU da Universidade Federal do Amap\u00e1 (HU-Unifap)",
        "3654826" ~ "Hospital de Doen\u00e7as Tropicais (HDT-UFT)",
        "4014111" ~ "Hospital Universit\u00e1rio Ana Bezerra (Huab-UFRN)",
        "4044916" ~ "HU Cassiano Ant\u00f4nio Moraes (Hucam-UFES)",
        "5586348" ~ "HU da Univ. Federal de S\u00e3o Carlos (HU-Ufscar)",
        "6042414" ~ "HU da Univ. Fed. Vale do S\u00e3o Francisco (HU-Univasf)",
        "6568343" ~ "Hospital Universit\u00e1rio de Lagarto (HUL-UFS)",
        .default = CNES
      )
    ) %>% dplyr::relocate(Name_CNES, .before = 4)

  tabela_indicadores_num_den =
    dplyr::left_join(indicadores, data, by = c("ANO_CMPT", "MES_CMPT", "CNES"))

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
  return(tabela_indicadores_num_den)
}
