
#' Obtem os dados do SIH e retorna em uma unica tabela
#'
#' @description Realiza o download dos arquivos DBC ('SIH-RD', 'SIH-RJ' e 'SIH-SP'), combina as três bases de dados em uma única estrutura, seleciona apenas as variáveis relevantes, cria novas variáveis e retorna uma base de dados contendo exclusivamente as variáveis que serão utilizadas nos cálculos dos indicadores
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os indicadores
#'
#'
#' @examples
#' \dontrun{
#'   data_SIH = get_data_SIH(
#'     year_start = 2021,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 12,
#'     save_path = tempdir()
#'   )
#' }
#'
#' @export
get_data_SIH <- function(
    year_start,
    month_start,
    year_end,
    month_end,
    save_path = tempdir()){

  `%>%` <- dplyr::`%>%`
  estados = c(
    "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
    "RS","PA","GO","PR","PB","RN","CE","MT","MA",
    "SC","PI","AP","TO","ES","SP"
  )

  #Download dos DBC (SIH-RD,SIH-RJ e SIH-SP)
  download_data(year_start,
                month_start,
                year_end,
                month_end,
                estados,
                tempdir())

  # #Ler os arquivos DBC; Filtra os estabelecimentos da EBSERH e seleciona as colunas necessárias
  data_rd = load_data("SIH-RD")
  data_rj = load_data("SIH-RJ")
  data_sp = load_data("SIH-SP")

  # data_rd = readRDS("./R/data_rd.rds")
  # data_rj = readRDS("./R/data_rj.rds")
  # data_sp = readRDS("./R/data_sp.rds")

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
  data_rd$TIPO_AIH <- 1 #"Aprovada"
  data_rj$TIPO_AIH <- 2 #"Rejeitada"
  dados <- rbind(data_rd,data_rj)

  #Cria a coluna TOTAL_CIRURGIAS_APRESENTADAS a partir da base data_cirurgias
  dados = dados %>%
    dplyr::left_join(data_cirurgias, by = c('N_AIH'="SP_NAIH")) %>%
    dplyr::mutate(TOTAL_CIRURGIAS_APRESENTADAS =
                    ifelse(is.na(sum_SP_QT_PROC), 1,
                           sum_SP_QT_PROC)) %>%
    dplyr::select(-sum_SP_QT_PROC)

  #Cria a coluna CO_GRUPO_PROC, que contem o número do grupo do procedimento
  dados = dados %>%
    dplyr::mutate(CO_GRUPO_PROC = substr(PROC_REA, 2, 2))

  #cria coluna SELECAO_MOTIVO_SAIDA, que contem sim ou não
  dados$SELECAO_MOTIVO_SAIDA <- ifelse(
    dados$COBRANCA >= 21 & dados$COBRANCA <= 28,
    2, 1) #sim=1, não=2

  #Converte as colunas para inteiro
  dados <- dados %>%
    dplyr::mutate(dplyr::across(
      c(CO_GRUPO_PROC, TOTAL_CIRURGIAS_APRESENTADAS, TIPO_AIH, SELECAO_MOTIVO_SAIDA),
      as.integer
    ))


  dados <- dados %>%
    dplyr::mutate(
      #Add um 0 entre ano e mes se mes tiver só um caracter
      mes_formatado = stringr::str_pad(MES_CMPT, width = 2, side = "left", pad = "0"),
      data = as.integer(paste0(ANO_CMPT, mes_formatado))
    )

  dados = dados %>% dplyr::select(
    "TIPO_AIH",
    "N_AIH",
    "data",
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

  #Renomear as colunas
  dados <- dados %>%
    dplyr::rename(
      ano = ANO_CMPT,
      mes = MES_CMPT,
      procedimento_realizado = PROC_REA,
      dias_permanencia = DIAS_PERM,
      cod_grupo_procedimento = CO_GRUPO_PROC,
      motivo_saida_ou_permanencia = COBRANCA,
      especialidade_leito = ESPEC,
      tipo_uti_usada = MARCA_UTI,
      dias_uti_no_mes = UTI_MES_TO
    )

  #Nome das colunas para minusculo
  names(dados) <- tolower(names(dados))

  return(dados)
}
