
#' Escreve em um Banco de Dados Remoto
#'
#' @description Escreve em uma das tabelas a seguir: (leitos, habilitacao, equipamentos, numerador_denominador)
#'
#' @param db_name String. Nome do banco de dados.
#' @param host String. Host.
#' @param port String. Porta de conexão.
#' @param user String. Nome do usuario que tem acesso ao banco
#' @param password String. Senha do usuario para acessa o banco
#' @param tabela String. Nome da tabela do banco (data_aih, leitos, habilitacao, equipamento
#' s,numerador_denominador).
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
#'     data = data
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
                     data)
{

  `%>%` <- dplyr::`%>%`
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

  data = data %>% dplyr::select(-c(ano,mes))

  if (tabela %in% c("data_aih",
                    "leitos",
                    "habilitacao",
                    "equipamentos",
                    "numerador_denominador"
  )) {
    #Insercao de novos registros na tabela especificada
    DBI::dbWriteTable(
      con,
      tabela,
      data,
      append = TRUE,
      row.names = FALSE
    )
  }
  #Fecha a conexao
  DBI::dbDisconnect(con)

#Criar as tabelas no bacno --------------------------
#
#    #Create table leitos
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE leitos (
#       data                    INTEGER,
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
#    #Create table habilitacao
#    DBI::dbExecute(con,
#                   "CREATE TABLE habilitacao (
#       data              INTEGER,
#       cnes              TEXT,
#       cod_habilitacao   SMALLINT
#      );")
#
#
#    #Create table equipamentos
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE equipamentos (
#       data                  INTEGER,
#       cnes                  TEXT,
#       cod_tipo_equipamento  SMALLINT,
#       cod_equipamento       SMALLINT,
#       qt_exist              SMALLINT,
#       qt_uso                SMALLINT,
#       qt_n_uso              SMALLINT
#      );"
#    )
#
#    #Create table numerador_denominador
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE numerador_denominador (
#         data                               INTEGER,
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
#    #Create table data_aih
#    DBI::dbExecute(
#      con,
#      "CREATE TABLE data_aih (
#      data                         INTEGER,
#      tipo_aih                     SMALLINT,
#      n_aih                        TEXT,
#      procedimento_realizado       TEXT,
#      cnes                         TEXT,
#      dias_permanencia             SMALLINT,
#      cod_grupo_procedimento       SMALLINT,
#      motivo_saida_ou_permanencia  SMALLINT,
#      total_cirurgias_apresentadas SMALLINT,
#      selecao_motivo_saida         SMALLINT,
#      especialidade_leito          SMALLINT,
#      tipo_uti_usada               SMALLINT,
#      dias_uti_no_mes              SMALLINT
#    );"
#    )
}
