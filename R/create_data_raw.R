
#' Cria a base de dados será utilizada pelos indicadores
#'
#' @description Realiza o download dos arquivos DBC ('SIH-RD', 'SIH-RJ' e 'SIH-SP'), combina as três bases de dados em uma única estrutura, seleciona apenas as variáveis relevantes, cria novas variáveis e retorna uma base de dados contendo exclusivamente as variáveis que serão utilizadas nos cálculos dos indicadores
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa
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
#'     month_end = 6,
#'     state_abbr = "CE",
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
           state_abbr,
           save_path = tempdir()){

    `%>%` <- dplyr::`%>%`

    #Obtendo base RD .dbc
    FaturaSUS.AmbHosp::download_dbc(
      information_system = "SIH-RD",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
    #Obtendo base RJ .dbc
    FaturaSUS.AmbHosp::download_dbc(
      information_system = "SIH-RJ",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )
    #Obtendo base SP .dbc
    FaturaSUS.AmbHosp::download_dbc(
      information_system = "SIH-SP",
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      state_abbr = state_abbr,
      save_path = save_path
    )

    #RD-------------------------
    dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-RD")
    dbf_files <- list.files(dbc_dir_path,
                            pattern = "\\.dbc$",
                            full.names = FALSE)

    output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

    #Carrega os dados RD
    raw_SIH_RD <- purrr::map_dfr(output_files_path,
                                 read.dbc::read.dbc,
                                 as.is=TRUE, .id="file_id")

    data_rd = raw_SIH_RD %>%
      dplyr::select("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                    "CNES","COBRANCA","DIAS_PERM","ESPEC")
    #'N_AIH','ANO_CMPT','MES_CMPT','ESPEC',"DT_INTER",
    #"DT_SAIDA",'PROC_REA','VAL_TOT',"COBRANCA",'DIAS_PERM',
    #'CNES','COMPLEX',"FINANC","MARCA_UTI","UTI_MES_TO",
    #"ST_BLOQ","ST_MOT_BLO"
    #RJ-------------------------
    dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-RJ")
    dbf_files <- list.files(dbc_dir_path,
                            pattern = "\\.dbc$",
                            full.names = FALSE)

    output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

    #Carrega os dados RJ
    raw_SIH_RJ <- purrr::map_dfr(output_files_path,
                                 read.dbc::read.dbc,
                                 as.is=TRUE, .id="file_id")

    data_rj = raw_SIH_RJ %>%
      dplyr::select("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                    "CNES","COBRANCA","DIAS_PERM","ESPEC" )

    #'N_AIH','ANO_CMPT','MES_CMPT','ESPEC',"DT_INTER",
    #"DT_SAIDA",'PROC_REA','VAL_TOT',"COBRANCA",'DIAS_PERM',
    #'CNES','COMPLEX',"FINANC","MARCA_UTI","UTI_MES_TO",
    #"ST_BLOQ","ST_MOT_BLO"

    #SP-------------------------
    dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-SP")
    dbf_files <- list.files(dbc_dir_path,
                            pattern = "\\.dbc$",
                            full.names = FALSE)

    output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")

    #Carrega os dados SP
    raw_SIH_SP <- purrr::map_dfr(output_files_path,
                                 read.dbc::read.dbc,
                                 as.is=TRUE, .id="file_id")

    data_sp = raw_SIH_SP %>%
      dplyr::select("SP_AA","SP_MM","SP_CNES","SP_NAIH","SP_PROCREA",
             "SP_ATOPROF","SP_VALATO","SP_COMPLEX","SP_FINANC",
             "SP_PF_CBO","IN_TP_VAL","SP_QT_PROC")

            #SP_NAIH,SP_ATOPROF,SP_QT_PROC


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
      "ESPEC")


    return(dados)
}
