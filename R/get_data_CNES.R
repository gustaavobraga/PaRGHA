
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
#'   data_CNES = get_data_CNES(
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
           type_data = "LT",
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
    publication_date_start <-
      lubridate::ym(stringr::str_glue("{year_start}-{month_start}"))
    publication_date_end <-
      lubridate::ym(stringr::str_glue("{year_end}-{month_end}"))

    nome_colunas <- c(
      data = "COMPETEN",
      ano = "ANO_CMPT",
      mes = "MES_CMPT",
      cod_tipo_leito = "TP_LEITO",
      cod_especialidade_leito = "CODLEITO",
      cod_habilitacao = "SGRUPHAB",
      cod_tipo_equipamento = "TIPEQUIP",
      cod_equipamento = "CODEQUIP"
    )

    #Download dos microdados ----------------------
    tempo_inicio_CNES <- system.time({
      type_data = toupper(trimws(type_data))

      if ( !(type_data %in% c("ST","LT","HB","EQ")) ){
        stop("O valor passado no par\u00e2metro type_data n\u00e3o \u00e9 aceito.\n")
      }

      #Define a URL de acordo com o type_data
      base_url <-
        switch(
          type_data,
          "ST" = "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/",
          "LT" = "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/",
          "HB" = "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/",
          "EQ" = "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/",
        )

      #Cria os diretorios onde vao ser salvos os microdados
      output_dir <- fs::path(save_path, "CNES")
      if (!dir.exists(output_dir)){
        dir.create(output_dir)
      }
      output_dir <- fs::path(output_dir, type_data)
      dir.create(output_dir)

      #Tentar conectar e ler os dados com tratamento de erros
      dir_files <- tryCatch({
        #Estabelece conexao
        connection <- curl::curl(base_url)

        #Garante que a conexao seja fechada mesmo em caso de erro
        on.exit(close(connection), add = TRUE)

        # Leitura e tratamento dos arquivos encontrados
        readLines(connection) %>%
          stringr::str_sub(start = -12) %>%
          tibble::as_tibble_col(column_name = "file_name") %>%
          dplyr::mutate(
            state = stringr::str_sub(file_name, 3, 4),
            publication_date = suppressWarnings(
              lubridate::ym(stringr::str_sub(file_name, 5, 8))
            ),
            format_file = stringr::str_sub(file_name, -3)
          ) %>%
          dplyr::filter(format_file == "dbc" & !is.na(publication_date))

      }, warning = function(w) {
        message("Aviso ao acessar o FTP do DATASUS: ", conditionMessage(w))
        return(NULL)
      }, error = function(e) {
        message("Erro ao acessar o FTP do DATASUS: ", conditionMessage(e))
        return(NULL)
      })

      #Filtra so os microdados que estao no intervalo desejado
      dir_files <- dir_files %>%
        dplyr::filter(
          publication_date >= publication_date_start &
            publication_date <= publication_date_end
        )

      #Filtra os UF
      if (!("all" %in% state_abbr)) {
        dir_files <- dir_files %>%
          dplyr::filter(state %in% state_abbr)
      }

      #Defini as URLs de cada arquivo a ser baixado
      files_name <- dplyr::pull(dir_files, file_name)
      download_files_url <- fs::path(base_url, files_name)
      output_files_path <- fs::path(output_dir, files_name)

      #Download dos microdados
      purrr::walk2(download_files_url,
                   output_files_path,
                   curl::curl_download)
    })
    cat("Tempo para baixar os dados do CNES:",
        round(tempo_inicio_CNES[3]/60,4), "minutos\n")



    #Leitura dos microdados -------------------------------
    tempo_inicio <- system.time({

      #Path dos microdados baixados
      dbc_dir_path <- fs::path(save_path, "CNES", type_data)
      dbf_files <- fs::dir_ls(dbc_dir_path, glob = "*.dbc")

      #Divide os microdados em chunk/pedacos, otimizando a leitura
      files_chunks <- chunk_fast(dbf_files)

      #Funcao para processar cada chunk
      processa_chunk <- function(chunk, n) {

        #Ler os arquivos do chunk
        raw_CNES <- purrr::map_dfr(chunk,
                                   read.dbc::read.dbc,
                                   as.is = TRUE,
                                   .id = "file_id")

        #Filtra os estabelecimentos EBSERH
        raw_CNES <- dplyr::filter(raw_CNES, CNES %in% CNES_EBSERH)

        #Seleciona as colunas com base no valor de type_data
        raw_CNES <- switch(
          type_data,
          "LT" = raw_CNES %>% dplyr::select(
            "CNES","COMPETEN","TP_LEITO","CODLEITO","QT_EXIST","QT_SUS"),
          "HB" = raw_CNES %>% dplyr::select("CNES","SGRUPHAB", "COMPETEN"),
          "EQ" = raw_CNES %>% dplyr::select(
            "CNES","COMPETEN","TIPEQUIP","CODEQUIP","QT_EXIST","QT_USO"),
        )

        #O resultado é salvo em formato RDS para otimizar o uso de memória e evitar sobrecarga no ambiente do R.
        output_path <-
          fs::path(dbc_dir_path,
                   sprintf("output_CNES-%s_chunk_%d.rds", type_data, n)
                   )
        saveRDS(raw_CNES, file = output_path)

        rm(raw_CNES)
        gc()
      }
      # Processamento sequencial
      purrr::walk2(files_chunks, seq_along(files_chunks), processa_chunk)

      #União dos arquivos .rds gerados
      outputCNES = fs::dir_ls(dbc_dir_path, glob = "*.rds") %>%
        purrr::map_dfr(readRDS)

      #Formatando a coluna Data
      outputCNES = outputCNES %>%
        dplyr::mutate(
          COMPETEN = as.integer(COMPETEN),
          ANO_CMPT = as.integer(stringr::str_sub(COMPETEN, 1, 4)),
          MES_CMPT = as.integer(stringr::str_sub(COMPETEN, 5, 6))
        ) %>%
        dplyr::select(COMPETEN, ANO_CMPT, MES_CMPT, dplyr::everything())

      #Filtra o nome_colunas apenas os nomes que existem na outputCNES
      nome_valido <- purrr::keep(nome_colunas, ~ .x %in% names(outputCNES))

      #Renomear as colunas com base no nome_colunas
      outputCNES = outputCNES %>%
        dplyr::rename(!!!setNames(nome_valido,names(nome_valido)))
    })
    cat("Tempo para ler os dados CNES: ",
        round(tempo_inicio[3]/60,4), "minutos\n")
    return(outputCNES)
  }
