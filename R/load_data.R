
#' Leitura e filtragem dos dados DBC
#'
#' @description Le arquivos DBC baixados pela função download_data, suportando um unico tipo de sistema por execucao: 'SIH-RD', 'SIH-RJ' ou 'SIH-SP'. Filtra os registros para incluir apenas estabelecimentos da EBSERH e seleciona as colunas relevantes.
#'
#' @param information_system string. Sigla do sistema de informacao. Valores permitidos: SIH-RD, SIH-RJ e SIH-SP.
#' @param save_path String. Diretorio onde os arquivos DBC serao salvos. O padrao e tempdir().
#'
#' @return Um DataFrame com os dados filtrados
#'
#' @examples
#' \dontrun{
#'   load_data(
#'     information_system = "SIH-RD",
#'     save_path = tempdir()
#'  )
#' }
#'
#' @export
load_data <- function(information_system,
                      save_path = tempdir()){
  `%>%` <- dplyr::`%>%`
  CNES_EBSERH = c(
    "0000396","0002534","0003816","0004731","0009709",
    "0010510","0012505","0027049","2006197","2017644",
    "2146339","2146355","2146371","2206595","2218798",
    "2244306","2252694","2295415","2332981","2338424",
    "2384299","2400243","2409208","2481286","2504502",
    "2561492","2640244","2653982","2655411","2676060",
    "2694751","2707675","2710935","2726653","3157245",
    "3285391","3432076","3654826","4014111","4044916",
    "5586348","6042414","6568343"
  )

  colunas_selecionadas = list(
    `SIH-RD` = c("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                 "CNES","COBRANCA","DIAS_PERM","ESPEC",
                 "MARCA_UTI","UTI_MES_TO"),
    `SIH-RJ` = c("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                 "CNES","COBRANCA","DIAS_PERM","ESPEC",
                 "MARCA_UTI","UTI_MES_TO"),
    `SIH-SP` = c("SP_AA","SP_MM","SP_CNES","SP_NAIH","SP_PROCREA",
                 "SP_ATOPROF","SP_VALATO","SP_COMPLEX","SP_FINANC",
                 "SP_PF_CBO","IN_TP_VAL","SP_QT_PROC")
  )

  tempo_inicio <- system.time({
    # Lista os nomes dos arquivos DBC que estao no diretorio save_path
    dbc_dir_path = fs::path(save_path, "file_DBC", information_system)
    dbf_files = fs::dir_ls(dbc_dir_path, glob = "*.dbc")

    files_chunks = chunk_fast(dbf_files)

    # Função para processar cada chunk
    processa_chunk = function(chunk, n) {
      # Ler os arquivos do chunk
      raw_SIH = purrr::map_dfr(chunk,
                               read.dbc::read.dbc,
                               as.is = TRUE, .id = "file_id")

      # Filtragem por estabelecimentos EBSERH
      if(information_system=="SIH-SP"){
        raw_SIH = dplyr::filter(raw_SIH, SP_CNES %in% CNES_EBSERH)
      } else {
        raw_SIH = dplyr::filter(raw_SIH, CNES %in% CNES_EBSERH)
      }

      # Seleção de colunas
      colunas = colunas_selecionadas[[information_system]]
      if (!is.null(colunas)) {
        raw_SIH = dplyr::select(raw_SIH, dplyr::all_of(colunas))
      }

      # Caminho para salvar o chunk processado
      output_path = fs::path(dbc_dir_path, sprintf("output_%s_chunk_%d.rds", information_system, n))
      saveRDS(raw_SIH, file = output_path)
      rm(raw_SIH)
      gc()
    }

    # Processamento sequencial
    purrr::walk2(files_chunks, seq_along(files_chunks), processa_chunk)

    # União dos arquivos .rds gerados
    outputSIH = fs::dir_ls(dbc_dir_path, glob = "*.rds") %>%
      purrr::map_dfr(readRDS)
  })
  cat("Tempo para ler os dados",information_system,":",round(tempo_inicio[3]/60,4), "minutos\n")
  return(outputSIH)
}
