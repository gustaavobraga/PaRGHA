
#' Obtem ...
#'
#' @description ...
#'
#' @param data DataFrame. Dados que serao inseridos na tabela.
#' @param tabela String. Nome da tabela do banco (leitos, habilitacao, equipamentos,numerador_denominador).
#'
#' @return ...
#'
#'
#' @examples
#' \dontrun{
#'   azure()
#' }
#'
#' @export
azure <- function(data, tabela) {

  con = DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "teste-tcc.postgres.database.azure.com",
    port = 5432,
    user = "gustavo",
    password = "",
    sslmode = "require"
  )

  #Coloca o nome das colunas em minusculo
  names(data) <- tolower(names(data))

  if (tabela %in% c("leitos", "habilitacao", "equipamentos", "numerador_denominador")) {
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
#       ano_cmpt   TEXT,
#       mes_cmpt   TEXT,
#       codufmun   TEXT,
#       cnes       TEXT,
#       tp_leito   TEXT,
#       codleito   TEXT,
#       qt_exist   INTEGER
#     );"
#   )
#   #Create table habilitacao
#   DBI::dbExecute(
#     con,
#     "CREATE TABLE habilitacao (
#       ano_cmpt   TEXT,
#       mes_cmpt   TEXT,
#       cnes       TEXT,
#       cod_habilitacao   TEXT
#     );"
#   )
#   #Create table equipamentos
#   DBI::dbExecute(
#     con,
#     "CREATE TABLE equipamentos (
#       ano_cmpt          TEXT,
#       mes_cmpt          TEXT,
#       cnes              TEXT,
#       tipo_equipamento  TEXT,
#       cod_equipamento   TEXT,
#       qt_exist          INTEGER,
#       qt_uso            INTEGER,
#       qt_n_uso          INTEGER
#     );"
#   )
#
#   #Create table numerador_denominador
#   DBI::dbExecute(con, "
#   CREATE TABLE numerador_denominador (
#     ano_cmpt                           TEXT,
#     mes_cmpt                           TEXT,
#     cnes                               TEXT,
#     name_cnes                          TEXT,
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
#   #DBI::dbExecute(con, "DROP TABLE data_sih;")
#
#   #DBI::dbExistsTable(con, "equipamentos")
#
#   DBI::dbGetQuery(con, "SELECT * FROM numerador_denominador LIMIT 10")
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
