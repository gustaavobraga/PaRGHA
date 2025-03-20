
#' Cria a base de dados será utilizada pelos indicadores
#'
#' @description Realiza o download dos arquivos DBC ('SIH-RD', 'SIH-RJ' e 'SIH-SP'), combina as três bases de dados em uma única estrutura, seleciona apenas as variáveis relevantes, cria novas variáveis e retorna uma base de dados contendo exclusivamente as variáveis que serão utilizadas nos cálculos dos indicadores
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os dados de RD, RJ e SP
#'
#'
#' @examples
#' \dontrun{
#'   dados = create_data_raw(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 12,
#'     save_path = tempdir()
#'  )
#' }
#'
#' @export
create_data_raw <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           save_path = tempdir()){

    `%>%` <- dplyr::`%>%`

    download_data(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      save_path = tempdir()
    )
    data_rd = readRDS(stringr::str_glue("{save_path}\\dados\\df_RD.rds"))
    data_rj = readRDS(stringr::str_glue("{save_path}\\dados\\df_RJ.rds"))
    data_sp = readRDS(stringr::str_glue("{save_path}\\dados\\df_SP.rds"))

    #Tratamento do Banco de Dados do RJ -----
    #ECLUINDO OS VALORES DUPLICADOS
    data_rj = data_rj %>% dplyr::group_by(N_AIH) %>%
      dplyr::slice_max(MES_CMPT, n = 1) %>%
      dplyr::ungroup()

    #Excluir as AIH rejeitadas que foram aprovadas posteriormente RD
    #Removendo AIH aprovada
    data_rj = data_rj %>% dplyr::anti_join(data_rd,by=c('N_AIH'='N_AIH'))

    #Tratamento do Banco de Dados do SP-----

    #Obter o total de procedimentos cirúrgicos registrados como
    #procedimentos secundários em AIH abertas com procedimentos
    #Subgrupo 0415 da Tabela SUS.

    data_cirurgias = data_sp %>%
      dplyr::filter(grepl("^04", SP_ATOPROF)) %>%
      dplyr::filter(SP_QT_PROC!=0) %>%
      dplyr::select(SP_NAIH,SP_QT_PROC) %>%
      dplyr::group_by(SP_NAIH) %>%
      dplyr::summarise(sum_SP_QT_PROC = sum(SP_QT_PROC))


    #consolidacao das informacoes-----
    data_rd$TIPO_AIH <- "Aprovada"
    data_rj$TIPO_AIH <- "Rejeitada"
    base_combinada <- rbind(data_rd,data_rj)

    #Cria a coluna TOTAL_CIRURGIAS_APRESENTADAS a partir da base data_cirurgias
    base_combinada2 = base_combinada %>%
      dplyr::left_join(data_cirurgias, by = c('N_AIH'="SP_NAIH")) %>%
      dplyr::mutate(TOTAL_CIRURGIAS_APRESENTADAS =
                      ifelse(is.na(sum_SP_QT_PROC), 1,
                             sum_SP_QT_PROC)) %>%
      dplyr::select(-sum_SP_QT_PROC)

    #Cria a coluna CO_GRUPO_PROC, que contem o número do grupo
    base_combinada2 = base_combinada2 %>%
      dplyr::mutate(CO_GRUPO_PROC = substr(PROC_REA, 2, 2))

    #cria coluna SELECAO_MOTIVO_SAIDA, que contem sim ou não
    base_combinada2$SELECAO_MOTIVO_SAIDA <- ifelse(base_combinada2$COBRANCA >= 21 & base_combinada2$COBRANCA <= 28, "não", "sim")

    dados = base_combinada2 %>% dplyr::select(
      "TIPO_AIH",
      "N_AIH",
      "ANO_CMPT",
      "MES_CMPT",
      "PROC_REA",
      "CNES",
      "DIAS_PERM",
      "CO_GRUPO_PROC",
      "COBRANCA",
      "TOTAL_CIRURGIAS_APRESENTADAS",
      "SELECAO_MOTIVO_SAIDA",
      "ESPEC",
      "MARCA_UTI",
      "UTI_MES_TO")


    return(dados)
}
