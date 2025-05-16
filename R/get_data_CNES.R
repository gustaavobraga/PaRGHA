
#' Download dos dados do CNES que será utilizada pelos indicadores
#'
#' @description Realiza o download dos dados do CNES para um dos sistemas especificados (LT = Leitos, HB = Habilitações, EQ = Equipamentos) e seleciona apenas as colunas necessárias.
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa
#' @param type_data string. O padrao e "LT". Valores aceitos ("LT", "HB", "EQ"). O tipo da base de dados do CNES que sera baixado.
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @return Um DataFrame com os dados dos estabelecimentos de saude
#'
#'
#' @examples
#' \dontrun{
#'   dados = get_data_CNES(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 2,
#'     state_abbr = "all",
#'     type_data = "LT",
#'     save_path = tempdir()
#'  )
#' }
#'
#' @export
get_data_CNES <-
  function(year_start,
           month_start,
           year_end,
           month_end,
           state_abbr = "all",
           type_data = "EQ",
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

    # tempo_inicio_CNES <- system.time({
    #   #Obtendo base CNES-LT .dbc
    #   PaPAHR::download_cnes_files(
    #     year_start = year_start,
    #     month_start = month_start,
    #     year_end = year_end,
    #     month_end = month_end,
    #     newer=F,
    #     state_abbr = state_abbr,
    #     type_data=type_data)
    # })
    # cat("Tempo para baixar os dados do CNES:", round(tempo_inicio_CNES[3]/60,4), "minutos\n")

    tempo_inicio <- system.time({
      dbc_dir_path = fs::path(save_path, "CNES", type_data)
      dbf_files = fs::dir_ls(dbc_dir_path, glob = "*.dbc")

      files_chunks = chunk_fast(dbf_files)
      length(files_chunks)

      # Função para processar cada chunk
      processa_chunk = function(chunk, n) {
        # Ler os arquivos do chunk
        raw_CNES = purrr::map_dfr(chunk,
                                  read.dbc::read.dbc,
                                  as.is = TRUE,
                                  .id = "file_id")
        # Filtragem por estabelecimentos EBSERH
        raw_CNES = dplyr::filter(raw_CNES, CNES %in% CNES_EBSERH)

        #Selecionar as colunas com base no valor de type_datab
        raw_CNES <- switch(
          type_data,
          "LT" = raw_CNES %>% dplyr::select(
            "CODUFMUN","CNES","COMPETEN","TP_LEITO","CODLEITO","QT_EXIST"),
          "HB" = raw_CNES %>% dplyr::select("SGRUPHAB", "CNES", "COMPETEN"),
          "EQ" = raw_CNES %>% dplyr::select(
            "CNES","COMPETEN","TIPEQUIP","CODEQUIP","QT_EXIST","QT_USO"),
        )

        output_path = fs::path(dbc_dir_path,
                               sprintf("output_CNES-%s_chunk_%d.rds", type_data, n))
        saveRDS(raw_CNES, file = output_path)
        rm(raw_CNES)
        gc()
      }
      # Processamento sequencial
      purrr::walk2(files_chunks, seq_along(files_chunks), processa_chunk)

      # União dos arquivos .rds gerados
      outputCNES = fs::dir_ls(dbc_dir_path, glob = "*.rds") %>%
        purrr::map_dfr(readRDS)

      #Formatando a coluna Data
      outputCNES = outputCNES %>%
        dplyr::mutate(
          ANO_CMPT = stringr::str_sub(COMPETEN, 1, 4),
          MES_CMPT = stringr::str_sub(COMPETEN, 5, 6)
        ) %>%
        dplyr::select(ANO_CMPT, MES_CMPT, dplyr::everything(), -COMPETEN)
    })
    cat("Tempo para ler os dados CNES: ",round(tempo_inicio[3]/60,4), "minutos\n")
    return(outputCNES)
  }
