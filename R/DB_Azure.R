
#' Escreve em um Banco de Dados Remoto
#'
#' @description Escreve em uma das tabelas a seguir: (leitos, habilitacao, equipamentos, numerador_denominador)
#'
#' @param db_name String. Nome do banco de dados.
#' @param host String. Host.
#' @param port String. Porta de conexão.
#' @param user String. Nome do usuario que tem acesso ao banco
#' @param password String. Senha do usuario para acessa o banco
#' @param tabela String. Nome da tabela do banco (leitos, habilitacao, equipamento
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

  if (tabela %in% c("leitos",
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

#   #>Criar tabelas ------
#   names(indicadores) <- tolower(names(indicadores))
#   colnames(indicadores)
#
#   #Create table leitos
#   DBI::dbExecute(
#     con,
#     "CREATE TABLE leitos (
#       data                    INTEGER,
#       ano                     INTEGER,
#       mes                     INTEGER,
#       CNES                    TEXT,
#       cod_tipo_leito          INTEGER,
#       cod_especialidade_leito INTEGER,
#       qt_exist                INTEGER,
#       qt_sus                  INTEGER,
#       qt_exist_uti            INTEGER
#    );"
#   )
#   #Create table habilitacao
#   DBI::dbExecute(
#     con,
#     "CREATE TABLE habilitacao (
#       data              INTEGER,
#       ano               INTEGER,
#       mes               INTEGER,
#       cnes              TEXT,
#       cod_habilitacao   INTEGER
#     );"
#   )
#   #Create table equipamentos
#   DBI::dbExecute(
#     con,
#     "CREATE TABLE equipamentos (
#       data                  INTEGER,
#       ano                   INTEGER,
#       mes                   INTEGER,
#       cnes                  TEXT,
#       cod_tipo_equipamento  INTEGER,
#       cod_equipamento       INTEGER,
#       qt_exist              INTEGER,
#       qt_uso                INTEGER,
#       qt_n_uso              INTEGER
#     );"
#   )
#
#   #Create table numerador_denominador
#   DBI::dbExecute(con, "
#   CREATE TABLE numerador_denominador (
#     data                               INTEGER,
#     ano                                INTEGER,
#     mes                                INTEGER,
#     cnes                               TEXT,
#     num_dias_perman_hosp               INTEGER,
#     num_motivos_saida_hosp             INTEGER,
#     num_dias_perman_clinica            INTEGER,
#     num_motivos_saida_clinica          INTEGER,
#     num_dias_perman_cirurgica          INTEGER,
#     num_motivos_saida_cirurgica        INTEGER,
#     num_partos_cesareos                INTEGER,
#     num_partos_total                   INTEGER,
#     n_dias_em_uti                      INTEGER,
#     n_cirurgias_apre                   INTEGER,
#     n_internacoes_h                    INTEGER,
#     n_leitos_uti                       INTEGER,
#     n_leitos_hosp_sem_leitos_hosp_dia  INTEGER
#   );
# ")
#
#   #Remove a tabela
#   #DBI::dbExecute(con, "DROP TABLE numerador_denominador;")
#
#   #DBI::dbExistsTable(con, "equipamentos")
#
#   DBI::dbGetQuery(con, "SELECT * FROM leitos LIMIT 10")
#
#   #Dimensao da tabela
#   DBI::dbGetQuery(con, "SELECT COUNT(*) FROM habilitacao")
#   # count
#   # 1 1626250
#
#   #Tamanho em disco que ela está ocupando
#   DBI::dbGetQuery(
#     con,
#     "SELECT pg_size_pretty(pg_total_relation_size('data_sih')) AS tamanho_total"
#   )
#
#   # Fecha a conexão
#   DBI::dbDisconnect(con)
}
