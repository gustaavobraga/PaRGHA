
#' Escreve em um Banco de Dados Remoto
#'
#' @description Escreve em uma das tabelas a seguir: (leitos, habilitacao, equipamentos, numerador_denominador)
#'
#' @param db_name String. Nome do banco de dados.
#' @param host String. Host.
#' @param port String. Porta de conexão.
#' @param user String. Nome do usuario que tem acesso ao banco
#' @param password String. Senha do usuario para acessa o banco
#' @param tabela String. Nome da tabela do banco (data_aih, leitos, habilitacao, equipamentos,numerador_denominador).
#' @param tab_labels String. Nome da tabela de labels. O padao e NULL
#' @param data DataFrame. Dados que serao inseridos na tabela.
#'
#' @examples
#' \dontrun{
#'   DB_Azure(
#'     db_name = "postgres",
#'     host = "teste-tcc.postgres.database.azure.com",
#'     port = "5432",
#'     user = "gustavo",
#'     password = "-",
#'     tabela = "leitos",
#'     data = data,
#'     tab_labels = NULL
#'   )
#' }
#'
#' @export
DB_Azure <- function(db_name,
                     host,
                     port,
                     user,
                     password,
                     tabela,
                     data,
                     tab_labels = NULL)
{

  `%>%` <- dplyr::`%>%`

  #Nome das tabelas de labels
  labels = c(
    "tab_cnes",
    "d_calendario",
    "tab_tipo_leito",
    "tab_habilitacao",
    "name_indicadores",
    "tab_equipamentos",
    "tab_tipo_equipamentos",
    "tab_especialidade_leito",
    "tab_grupo_procedimentos",
    "tab_motivo_saida_permanencia"
  )

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = db_name,
    host = host,
    port = port,
    user = user,
    password = password,
    sslmode = "require"
  )

  #Coloca o nome das colunas em minusculo
  names(data) <- tolower(names(data))

  if (tabela %in% c("leitos",
                    "data_aih",
                    "habilitacao",
                    "equipamentos",
                    "numerador_denominador"
  )) {
    data = data %>% dplyr::select(-c(ano,mes))

    #Insercao de novos registros na tabela especificada
    DBI::dbWriteTable(
      con,
      tabela,
      data,
      append = TRUE,
      row.names = FALSE
    )
  } else if(tabela == "labels"){
    if (tab_labels %in% labels){

      query <- paste0("DELETE FROM ", tab_labels, ";")

      # Apagar os dados da tabela existente
      DBI::dbExecute(con, query)

      #Insercao de novos registros, especifica para as tabelas de labels
      DBI::dbWriteTable(
        con,
        tab_labels,
        data,
        append = TRUE,
        row.names = FALSE
      )
    }
  }

  #Fecha a conexao
  DBI::dbDisconnect(con)

#Codigo para criar as tabelas no bacno --------------------------
#
#    #leitos-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE leitos (
#       key_data                INTEGER,
#       CNES                    TEXT,
#       cod_tipo_leito          SMALLINT,
#       cod_especialidade_leito SMALLINT,
#       qt_exist                SMALLINT,
#       qt_sus                  SMALLINT,
#       qt_exist_uti            SMALLINT
#     );"
#    )
#
#
#    #habilitacao-----
#    DBI::dbExecute(con,
#     "CREATE TABLE habilitacao (
#       key_data          INTEGER,
#       cnes              TEXT,
#       cod_habilitacao   SMALLINT
#      );")
#
#
#    #equipamentos-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE equipamentos (
#       key_data              INTEGER,
#       cnes                  TEXT,
#       cod_tipo_equipamento  SMALLINT,
#       cod_equipamento       SMALLINT,
#       qt_exist              SMALLINT,
#       qt_uso                SMALLINT,
#       qt_n_uso              SMALLINT
#      );"
#    )
#
#    #numerador_denominador-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE numerador_denominador (
#         key_data                           INTEGER,
#         cnes                               TEXT,
#         num_dias_perman_hosp               SMALLINT,
#         num_motivos_saida_hosp             SMALLINT,
#         num_dias_perman_clinica            SMALLINT,
#         num_motivos_saida_clinica          SMALLINT,
#         num_dias_perman_cirurgica          SMALLINT,
#         num_motivos_saida_cirurgica        SMALLINT,
#         num_partos_cesareos                SMALLINT,
#         num_partos_total                   SMALLINT,
#         n_dias_em_uti                      SMALLINT,
#         n_cirurgias_apre                   SMALLINT,
#         n_internacoes_h                    SMALLINT,
#         n_leitos_uti                       SMALLINT,
#         n_leitos_hosp_sem_leitos_hosp_dia  SMALLINT
#     );"
#    )
#
#    #data_aih-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE data_aih (
#      key_data                        INTEGER,
#      tipo_aih                        SMALLINT,
#      n_aih                           TEXT,
#      procedimento_realizado          TEXT,
#      cnes                            TEXT,
#      dias_permanencia                SMALLINT,
#      cod_grupo_procedimento          SMALLINT,
#      cod_motivo_saida_ou_permanencia SMALLINT,
#      total_cirurgias_apresentadas    SMALLINT,
#      selecao_motivo_saida            SMALLINT,
#      cod_especialidade_leito         SMALLINT,
#      tipo_uti_usada                  SMALLINT,
#      dias_uti_no_mes                 SMALLINT
#    );"
#    )
#    #tab_cnes-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_cnes (
#          cnes                    TEXT,
#          nome_cnes               TEXT,
#          sigla_cnes              TEXT
#        );"
#    )
#    #tab_equipamentos-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_equipamentos (
#          cod_equipamento               SMALLINT,
#          nome_equipamento              TEXT
#        );"
#    )
#    #tab_especialidade_leito-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_especialidade_leito (
#          cod_especialidade_leito       SMALLINT,
#          nome_especialidade_leito      TEXT
#        );"
#    )
#    #tab_grupo_procedimentos-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_grupo_procedimentos (
#          cod_grupo_procedimento        SMALLINT,
#          nome_grupo_procedimento       TEXT
#        );"
#    )
#    #tab_tipo_equipamentos-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_tipo_equipamentos (
#          cod_tipo_equipamento        SMALLINT,
#          nome_tipo_equipamento       TEXT
#        );"
#    )
#    #tab_habilitacao-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_habilitacao (
#          cod_habilitacao         SMALLINT,
#          nome_habilitacao        TEXT
#        );"
#    )
#    #tab_tipo_leito-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_tipo_leito (
#          cod_tipo_leito         SMALLINT,
#          nome_tipo_leito        TEXT
#        );"
#    )
#    #tab_motivo_saida_permanencia-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE tab_motivo_saida_permanencia (
#          cod_motivo_saida_ou_permanencia       SMALLINT,
#          nome_motivo_saida_permanencia         TEXT
#        );"
#    )
#    #d_calendario-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE d_calendario (
#          key_data          INTEGER,
#          data              DATE,
#          ano               SMALLINT,
#          mes               SMALLINT,
#          mes_nome          TEXT
#         );"
#    )
#    #name_indicadores-----
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE name_indicadores (
#          indicador_name              TEXT,
#          indicador_absoluto          SMALLINT
#        );"
#    )
}
