
#' Download, Filtragem e Armazenamento de Dados dos Estabelecimentos EBSERH nos Sistemas SIH
#'
#' @description Realiza o download dos arquivos DBC dos sistemas SIH-RD, SIH-RJ e SIH-SP para todos os estados que possuem pelo menos um estabelecimento da EBSERH, considerando os meses especificados pelo usuário. Após o download, os arquivos são carregados e filtrados para selecionar apenas os estabelecimentos da EBSERH e as variáveis relevantes. Em seguida, os dados filtrados são salvos em uma base no diretório indicado por save_path.
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é ".\\".
#'
#' @examples
#' \dontrun{
#'   download_data(
#'     year_start = 2023,
#'     month_start = 1,
#'     year_end = 2023,
#'     month_end = 3,
#'     save_path = tempdir()
#'   )
#' }
#'
#' @export
download_data <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           save_path = tempdir()) {

  `%>%` <- dplyr::`%>%`

  #Verificar se a pasta 'tempdir()/dados' já existe, se sim,
  #apaga os arquivos que estão dentro dela
  information_system_dir <- stringr::str_glue("{save_path}\\dados")
  if (!dir.exists(information_system_dir)) {
    dir.create(information_system_dir)
  } else{
    arquivos <- list.files(information_system_dir, full.names = TRUE)
    unlink(arquivos, recursive = TRUE)
  }
  # Vetores --------------------------------------
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

  estados = c(
    "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
    "RS","PA","GO","PR","PB","RN","CE","MT","MA",
    "SC","PI","AP","TO","ES","SP"
  )

  # RD ------------------------------------------
  #Verificar se a pasta 'tempdir()/SIH-RD' já existe, se sim, apaga os arquivos que estão dentro dela
  information_system_dir <- stringr::str_glue("{save_path}\\SIH-RD")
  if (!dir.exists(information_system_dir)) {
    dir.create(information_system_dir)
  } else{
    arquivos <- list.files(information_system_dir, full.names = TRUE)
    unlink(arquivos, recursive = TRUE)
  }

  #Obtendo as bases dbc do RD
  PaPAHR::download_dbc(
    information_system = "SIH-RD",
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    state_abbr = estados,
    save_path = save_path
  )

  #Lista os nomes dos arquivos RD que serão carregados de cada mê
  dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-RD")
  dbf_files <- list.files(dbc_dir_path,
                          pattern = "\\.dbc$",
                          full.names = FALSE)

  output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")
  files_chunks = PaPAHR::chunk(output_files_path)
  n_chunks = length(files_chunks)

  tempo_inicio <- system.time({
    for (n in 1:n_chunks) {
      #Carrega os dados RD
      raw_SIH_RD <- purrr::map_dfr(files_chunks[[n]],
                                   read.dbc::read.dbc,
                                   as.is=TRUE, .id="file_id")

      #Filtra so os estabelecimentos da EBSERH
      raw_SIH_RD = raw_SIH_RD %>% dplyr::filter(CNES %in% CNES_EBSERH)

      data_rd = raw_SIH_RD %>%
        dplyr::select("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                      "CNES","COBRANCA","DIAS_PERM","ESPEC",
                      "MARCA_UTI","UTI_MES_TO")

      rm(raw_SIH_RD)
      #O output de cada chunk é salvo em um arquivo .rds em uma pasta temporária do sistema.
      if (!is.null(data_rd)) {
        output_path <-
          stringr::str_glue(
            "{save_path}\\SIH-RD\\output_SIH-RD_chunk_{n}.rds")

        saveRDS(data_rd, file = output_path)
      }
      rm(data_rd)
    }
  })
  cat("Tempo para baixar os dados do RD:", tempo_inicio[3] / 60, "minutos\n")

  #Une os arquivos output.rds de cada chunk em um único arquivo.
  outputSIH_RD <-
    save_path %>%
    list.files("SIH-RD",
               full.names = TRUE,
               recursive = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "\\.rds$")) %>%
    purrr::map_dfr(readRDS)

  output_path <- stringr::str_glue("{save_path}\\dados\\df_RD.rds")
  saveRDS(outputSIH_RD, file = output_path)
  rm(outputSIH_RD)

  # RJ ------------------------------------------
  #Verificar se a pasta 'tempdir()/SIH-RJ' já existe, se sim, apaga os arquivos que estão dentro dela
  information_system_dir <- stringr::str_glue("{save_path}\\SIH-RJ")
  if (!dir.exists(information_system_dir)) {
    dir.create(information_system_dir)
  } else{
    arquivos <- list.files(information_system_dir, full.names = TRUE)
    unlink(arquivos, recursive = TRUE)
  }

  #Obtendo as bases dbc do RJ
  PaPAHR::download_dbc(
    information_system = "SIH-RJ",
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    state_abbr = estados,
    save_path = save_path
  )

  #Lista os nomes dos arquivos RJ que serão carregados de cada mê
  dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-RJ")
  dbf_files <- list.files(dbc_dir_path,
                          pattern = "\\.dbc$",
                          full.names = FALSE)

  output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")
  files_chunks = PaPAHR::chunk(output_files_path)
  n_chunks = length(files_chunks)

  tempo_inicio <- system.time({
    for (n in 1:n_chunks) {
      #Carrega os dados RJ
      raw_SIH_RJ <- purrr::map_dfr(files_chunks[[n]],
                                   read.dbc::read.dbc,
                                   as.is=TRUE, .id="file_id")

      #Filtra so os estabelecimentos da EBSERH
      raw_SIH_RJ = raw_SIH_RJ %>% dplyr::filter(CNES %in% CNES_EBSERH)

      data_rj = raw_SIH_RJ %>%
        dplyr::select("N_AIH","ANO_CMPT","MES_CMPT","PROC_REA",
                      "CNES","COBRANCA","DIAS_PERM","ESPEC",
                      "MARCA_UTI","UTI_MES_TO")

      rm(raw_SIH_RJ)
      #O output de cada chunk é salvo em um arquivo .rds em uma pasta temporária do sistema.
      if (!is.null(data_rj)) {
        output_path <-
          stringr::str_glue(
            "{save_path}\\SIH-RJ\\output_SIH-RJ_chunk_{n}.rds")

        saveRDS(data_rj, file = output_path)
      }
      rm(data_rj)
    }
  })
  cat("Tempo para baixar os dados do RJ:", tempo_inicio[3] / 60, "minutos\n")

  #Une os arquivos output.rds de cada chunk em um único arquivo.
  outputSIH_RJ <-
    save_path %>%
    list.files("SIH-RJ",
               full.names = TRUE,
               recursive = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "\\.rds$")) %>%
    purrr::map_dfr(readRDS)

  output_path <- stringr::str_glue("{save_path}\\dados\\df_RJ.rds")
  saveRDS(outputSIH_RJ, file = output_path)
  rm(outputSIH_RJ)


  # SP ------------------------------------------
  #Verificar se a pasta 'tempdir()/SIH-SP' já existe, se sim, apaga os arquivos que estão dentro dela
  information_system_dir <- stringr::str_glue("{save_path}\\SIH-SP")
  if (!dir.exists(information_system_dir)) {
    dir.create(information_system_dir)
  } else{
    arquivos <- list.files(information_system_dir, full.names = TRUE)
    unlink(arquivos, recursive = TRUE)
  }

  #Obtendo as bases dbc do SP
  PaPAHR::download_dbc(
    information_system = "SIH-SP",
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    state_abbr = estados,
    save_path = save_path
  )

  #Lista os nomes dos arquivos SP que serão carregados de cada mê
  dbc_dir_path = stringr::str_glue("{save_path}\\file_DBC\\SIH-SP")
  dbf_files <- list.files(dbc_dir_path,
                          pattern = "\\.dbc$",
                          full.names = FALSE)

  output_files_path <- stringr::str_glue("{dbc_dir_path}\\{dbf_files}")
  files_chunks = PaPAHR::chunk(output_files_path,data_type = 'SP')
  n_chunks = length(files_chunks)

  tempo_inicio <- system.time({
    for (n in 1:n_chunks) {
      #Carrega os dados SP
      raw_SIH_SP <- purrr::map_dfr(files_chunks[[n]],
                                   read.dbc::read.dbc,
                                   as.is=TRUE, .id="file_id")

      #Filtra so os estabelecimentos da EBSERH
      raw_SIH_SP = raw_SIH_SP %>% dplyr::filter(SP_CNES %in% CNES_EBSERH)

      data_sp = raw_SIH_SP %>%
        dplyr::select("SP_AA","SP_MM","SP_CNES","SP_NAIH","SP_PROCREA",
                      "SP_ATOPROF","SP_VALATO","SP_COMPLEX","SP_FINANC",
                      "SP_PF_CBO","IN_TP_VAL","SP_QT_PROC")

      rm(raw_SIH_SP)
      #O output de cada chunk é salvo em um arquivo .rds em uma pasta temporária do sistema.
      if (!is.null(data_sp)) {
        output_path <-
          stringr::str_glue(
            "{save_path}\\SIH-SP\\output_SIH-SP_chunk_{n}.rds")

        saveRDS(data_sp, file = output_path)
      }
      rm(data_sp)
    }
  })
  cat("Tempo para baixar os dados do SP:", tempo_inicio[3] / 60, "minutos\n")

  #Une os arquivos output.rds de cada chunk em um único arquivo.
  outputSIH_SP <-
    save_path %>%
    list.files("SIH-SP",
               full.names = TRUE,
               recursive = TRUE) %>%
    purrr::keep(~ stringr::str_detect(.x, "\\.rds$")) %>%
    purrr::map_dfr(readRDS)

  output_path <- stringr::str_glue("{save_path}\\dados\\df_SP.rds")
  saveRDS(outputSIH_SP, file = output_path)
  rm(outputSIH_SP)
}
