
#' Cria tabelas e salva no banco de dados
#'
#' @description Cria as tabelas de Leitos, Habilitacoes, Equipamentos e do Numerador/Denominador dos indicadores, e salva cada tabela no banco de dados remoto
#'
#' @param db_name String. Nome do banco de dados.
#' @param host String. Host.
#' @param port String. Porta de conexão.
#' @param user String. Nome do usuario que tem acesso ao banco
#' @param password String. Senha do usuario para acessa o banco
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#' @param cloud Logical. O padrao é FALSE. Se TRUE, salva as tabelas no DW; caso contrario, cria um RData com todas as tabelas
#' @param save_Rdata_to_path String.Caminho para o diretorio onde deve ser salvo o arquivo RData que contém todas as tabelas do BI. O padrao e o diretorio atual "./".
#' @param save_path String. Diretorio onde os arquivos DBC serao salvos. O padrao é tempdir().
#'
#' @return Se cloud = FALSE, a função salva um arquivo .RData com o nome tabelas_do_Painel no diretório especificado pelo argumento save_Rdata_to_path.
#'
#' @examples
#' \dontrun{
#'   auto_PaRGHA(
#'     db_name = "postgres",
#'     host = "teste.postgres.database.azure.com",
#'     port = "1234",
#'     user = "gustavo",
#'     password = "-",
#'     year_start = 2021,
#'     month_start = 1,
#'     year_end = 2021,
#'     month_end = 2,
#'     cloud = FALSE,
#'     save_Rdata_to_path = "./"
#'   )
#' }
#'
#' @export
auto_PaRGHA = function(
    db_name,
    host,
    port,
    user,
    password,
    year_start,
    month_start,
    year_end,
    month_end,
    cloud = FALSE,
    save_Rdata_to_path = "./",
    save_path = tempdir()
){
  `%>%` <- dplyr::`%>%`

  tempo_inicio <- system.time({
    #Cria o diretorio onde vai ser salvo as tabelas
    output_dir <- fs::path(save_path, "table_BI")
    if (!dir.exists(output_dir)){
      dir.create(output_dir)
    } else {
      unlink(output_dir, recursive = TRUE)
      dir.create(output_dir)
    }


    #>Leitos -------------------
    cat("Leito:\n")
    leitos = leitos(year_start, month_start, year_end, month_end,FALSE)

    if(cloud){
      #Armazena os dados no banco de dados
      DB_Azure(db_name, host, port, user, password, "leitos", leitos)
    } else {
      output_path <- fs::path(output_dir, "leitos.rds")
      leitos = leitos %>% dplyr::select(-c(ano,mes))
      saveRDS(leitos, file = output_path)
    }

    #Apaga a pasta temporaria com os dados dos leitos
    unlink(fs::path(tempdir(), "CNES", "LT"), recursive = TRUE)

    rm(leitos)
    gc()

    #>Habilitacoes -------------------
    cat("\nHabilitacao:\n")
    habilitacoes = habilitacoes(year_start, month_start, year_end, month_end,FALSE)

    if(cloud){
      #Armazena os dados no banco de dados
      DB_Azure(db_name, host, port, user, password, "habilitacao", habilitacoes)
    } else {
      output_path <- fs::path(output_dir, "habilitacao.rds")
      habilitacoes = habilitacoes %>% dplyr::select(-c(ano,mes))
      saveRDS(habilitacoes, file = output_path)
    }

    #Apaga a pasta temporaria com os dados das habilitacoes
    unlink(fs::path(tempdir(), "CNES", "HB"),
           recursive = TRUE)
    rm(habilitacoes)
    gc()

    #>Equipamentos -------------------
    cat("\nEquipamentos:\n")
    equipamentos = equipamentos(year_start, month_start, year_end, month_end,FALSE)

    if(cloud){
      #Armazena os dados no banco de dados
      DB_Azure(db_name, host, port, user, password, "equipamentos", equipamentos)
    } else {
      output_path <- fs::path(output_dir, "equipamentos.rds")
      equipamentos = equipamentos %>% dplyr::select(-c(ano,mes))
      saveRDS(equipamentos, file = output_path)
    }

    #Apaga a pasta temporaria com os dados dos equipamentos
    unlink(fs::path(tempdir(), "CNES", "EQ"),
           recursive = TRUE)
    rm(equipamentos)
    gc()

    #>Data_aih -------------------
    cat("\nData_aih:\n")
    data_aih = get_data_SIH(year_start = year_start,
                            month_start = month_start,
                            year_end = year_end,
                            month_end = month_end,
                            save_path = save_path)


    if(cloud){
      #Armazena os dados no banco de dados
      DB_Azure(db_name, host, port, user, password, "data_aih", data_aih)
    } else {
      output_path <- fs::path(output_dir, "data_aih.rds")
      saveRDS(
        data_aih %>% dplyr::select(-c(ano,mes)),
        file = output_path
      )
    }

    #Apaga a pasta temporaria com os dados do SIH
    unlink(fs::path(tempdir(), "file_DBC"),
           recursive = TRUE)

    #>Numerador Denominador -------------------
    #Obtem os dados do SIH
    cat("\nNumerador Denominador:\n")

    state_abbr = c(
      "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
      "RS","PA","GO","PR","PB","RN","CE","MT","MA",
      "SC","PI","AP","TO","ES","SP"
    )
    #Obtem os dados do CNES
    data_CNES = get_data_CNES(year_start = year_start,
                              month_start = month_start,
                              year_end = year_end,
                              month_end = month_end,
                              state_abbr = state_abbr,
                              type_data = "LT",
                              save_path = save_path)

    #Apaga a pasta temporaria com os dados do CNES
    unlink(fs::path(tempdir(), "CNES", "LT"),
           recursive = TRUE)

    #Cria um DF com as colunas dos numeradores,
    #denominadores e dos indicadores
    indicadores = indicadores(data_SIH = data_aih,
                              data_CNES = data_CNES,
                              labels_CNES = FALSE)

    indicadores = dplyr::ungroup(indicadores)

    #Remove as colunas que sao indicadores relativos
    numerador_denominador = indicadores %>% dplyr::select(
      -c(
        "i_taxa_de_cesarea",
        "i_taxa_ocupacao_h",
        "i_taxa_ocupacao_uti",
        "i_tempo_medio_permanencia_cirurgica",
        "i_tempo_medio_permanencia_clinica",
        "i_tempo_medio_permanencia_hospitalar",
        "i_giro_de_leito"
      )
    )

    if(cloud){
      #Armazena os dados no banco de dados
      DB_Azure(db_name, host, port, user, password, "numerador_denominador", numerador_denominador)
    } else {
      output_path <- fs::path(output_dir, "numerador_denominador.rds")
      numerador_denominador = numerador_denominador %>% dplyr::select(-c(ano,mes))
      saveRDS(numerador_denominador, file = output_path)
    }

    rm(data_CNES, data_aih, indicadores, numerador_denominador, state_abbr)
    gc()

    #>Salvar todas as tabelas em um Rdata----
    if(!cloud){
      #Datas para criar a tabela d_calendario
      publication_date_start <-
        lubridate::ym(stringr::str_glue("{year_start}-{month_start}"))
      publication_date_end <-
        lubridate::ym(stringr::str_glue("{year_end}-{month_end}"))
      datas = c(publication_date_start,
                publication_date_end)


      #Obtendo as tabelas dos labels
      tab_grupo_procedimentos <- labels(type_data="tab_grupo_procedimentos")
      tab_cnes <- labels(type_data="tab_cnes")
      tab_equipamentos <- labels(type_data="tab_equipamentos")
      tab_tipo_equipamentos <- labels(type_data="tab_tipo_equipamentos")
      tab_habilitacao <- labels(type_data="tab_habilitacao")
      tab_tipo_leito <- labels(type_data="tab_tipo_leito")
      tab_especialidade_leito <- labels(type_data="tab_especialidade_leito")
      tab_motivo_saida_permanencia <- labels(type_data="tab_motivo_saida_permanencia")
      d_calendario <- labels(datas, type_data="d_calendario")
      name_indicadores <- labels(type_data="name_indicadores")

      # Local onde vai ser salvo o Rdata
      save_Rdata_to_path <- fs::path(save_Rdata_to_path, "tabelas_do_Painel.RData")

      # Listar todos os arquivos .rds no diretório
      arquivos_rds <- list.files(output_dir, pattern = "\\.rds$",
                                 full.names = TRUE)

      # Loop para ler os arquivos
      for (arquivo in arquivos_rds) {
        nome_arquivo <- tools::file_path_sans_ext(basename(arquivo))
        assign(nome_arquivo, readRDS(arquivo))
      }

      # Lista com os nomes das tabelas principais
      objetos_rds <- tools::file_path_sans_ext(basename(arquivos_rds))

      # Lista com os nomes das tabelas principais e tabelas de labels
      table_bi <- c(
        objetos_rds,
        "tab_grupo_procedimentos",
        "tab_cnes",
        "tab_equipamentos",
        "tab_tipo_equipamentos",
        "tab_habilitacao",
        "tab_tipo_leito",
        "tab_especialidade_leito",
        "tab_motivo_saida_permanencia",
        "d_calendario",
        "name_indicadores"
      )

      # Salva as tabelas em um Rdata
      save(list = table_bi, file = save_Rdata_to_path)
    }
  })
  cat("Tempo de execução da função auto_PaRGHA:",round(tempo_inicio[3]/60,4), "minutos\n")
}
