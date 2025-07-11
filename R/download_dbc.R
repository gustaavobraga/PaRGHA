
#' Downloads DBC data from the specified healthcare systems: 'SIH-RD', 'SIH-RJ', or 'SIH-SP'
#'
#' @description Realiza o download de arquivos DBC dos sistemas de saúde especificados ('SIH-RD', 'SIH-RJ' ou 'SIH-SP') e os salva em um diretório definido pelo usuário, criando automaticamente uma pasta chamada 'file_DBC'.
#'
#' @param information_system String. Valor aceito "SIH-RD", "SIH-RJ" ou "SIH-SP".
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param state_abbr string ou vetor de strings. Sigla da Unidade Federativa
#' @param save_path String. Diretório onde os arquivos DBC serão salvos. O padrão é tempdir().
#'
#' @examples
#'   download_dbc(
#'     information_system = "SIH-RJ",
#'     year_start = 2023,
#'     month_start = 1,
#'     year_end = 2023,
#'     month_end = 3,
#'     state_abbr = "CE",
#'     save_path = tempdir()
#'   )
#'
#' @export
download_dbc <-
  function(information_system,
           year_start,
           month_start,
           year_end,
           month_end,
           state_abbr,
           save_path = tempdir()) {

    `%>%` <- dplyr::`%>%`

    information_system = toupper(trimws(information_system))
    state_abbr = toupper(trimws(state_abbr))

    data_source = stringr::str_sub(information_system, 1, 3)
    data_type = stringr::str_sub(information_system, 5, 6)
    base_url <- stringr::str_glue(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/{data_source}SUS/200801_/Dados/")

    publication_date_start <-
      lubridate::ym(stringr::str_glue("{year_start}-{month_start}"))
    publication_date_end <-
      lubridate::ym(stringr::str_glue("{year_end}-{month_end}"))

    #Lista os nomes dos arquivos que serão baixados de cada mês
      connection <- curl::curl(base_url)

      #$ Listar os arquivos (RD, RJ ou SP) do DATASUS
      dir_files <-
        connection %>%
        readLines() %>%
        stringr::str_sub(start = -13) %>%
        tibble::as_tibble_col(column_name = "file_name") %>%
        dplyr::mutate(
          file_name = stringr::str_trim(file_name, side = "left"),
          file_name = paste0(toupper(substr(file_name, 1, 4)), substr(file_name, 5, nchar(file_name))),
          state = stringr::str_sub(file_name, 3, 4),
          publication_date = lubridate::ym(stringr::str_sub(file_name, 5, 8), quiet = TRUE),
          file_type = stringr::str_sub(file_name, 1, 2)
        ) %>%
        dplyr::filter(
          file_type %in% data_type,
          state %in% state_abbr,
          publication_date >= publication_date_start,
          publication_date <= publication_date_end
        )

      close(connection)

    #Verifica se dir_files contém o nome de pelo menos um arquivo data_type para cada mês.
      # Cria um vetor com todos os meses dentro do intervalo especificado.
      meses <-
        seq.Date(
          from = lubridate::floor_date(publication_date_start, "month"),
          to = lubridate::ceiling_date(publication_date_end, "month") - lubridate::days(1),
          by = "month"
        )

      meses_baixados = unique(dir_files$publication_date)
      faltando <- setdiff(as.character(meses), as.character(meses_baixados))

      if (length(faltando) > 0) {
        cat("Não foi possível baixar os dados de", data_type,"do(s) estado(s) ", state_abbr,
            "para os seguinte(s) mes(es):\n")
        print(faltando)
      }


    #Separa os arquivos em grupos, caso haja vários arquivos para serem baixados.
    files_chunks <- chunk(dir_files$file_name)

    n_chunks = length(files_chunks)

    base_url <- stringr::str_glue("ftp://ftp.datasus.gov.br/dissemin/publicos/{data_source}SUS/200801_/Dados/")

    #Criando as pastas
    output_dir <- stringr::str_glue("{save_path}\\file_DBC")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    output_dir <- stringr::str_glue("{save_path}\\file_DBC\\{information_system}")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }

    for (n in 1:n_chunks) {
      download_files_url <-
        stringr::str_glue("{base_url}{files_chunks[[n]]}")
      output_files_path <-
        stringr::str_glue("{save_path}\\file_DBC\\{information_system}\\{files_chunks[[n]]}")

      #Download dos dados
      purrr::walk2(download_files_url, output_files_path, curl::curl_download)
    }
  }
