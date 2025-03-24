
#' Retorna a base de dados com todos os indicadores empilhados
#'
#' @description Reestrutura a base de dados geradada pela função indicadores(), criando uma base de dados onde os indicadores estão disposto todos em uma unica coluna.
#'
#' @param dados a base de dados criada pela função indicadores()
#'
#' @return Um DataFrame com os indicadores empilhados
#'
#'
#' @examples
#' \dontrun{
#'   dados_empilhados = indicadores_empilhados(dados)
#' }
#'
#' @export
indicadores_empilhados <-
  function(dados){

    `%>%` <- dplyr::`%>%`

    #Separa os indicadores em variaveis
    i_1 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, N_CIRURGIAS_APRE, Nome_CNES)
    i_2 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, N_Internacoes_H, Nome_CNES)
    i_3 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_taxa_de_cesarea, Nome_CNES)
    i_4 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_taxa_ocupacao_H, Nome_CNES)
    i_5 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_taxa_ocupacao_UTI, Nome_CNES)
    i_6 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_tempo_medio_permanencia_cirurgica, Nome_CNES)
    i_7 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_tempo_medio_permanencia_clinica, Nome_CNES)
    i_8 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_tempo_medio_permanencia_hospitalar, Nome_CNES)
    i_9 = dados %>% dplyr::select(ANO_CMPT, MES_CMPT, CNES, i_giro_de_leito, Nome_CNES)

    #Cria a coluna com o nome do indicador
    i_1$Nome_Indicador = "Numero de Cirurgias Apresentadas"
    i_2$Nome_Indicador = "Numero de Internacoes Hospitalares"
    i_3$Nome_Indicador = "Taxa de Cesarea"
    i_4$Nome_Indicador = "Taxa de Ocupacao Hospitalar"
    i_5$Nome_Indicador = "Taxa de Ocupacao de UTI"
    i_6$Nome_Indicador = "Tempo Medio de Permanencia Cirurgica"
    i_7$Nome_Indicador = "Tempo Medio de Permanencia Clinica"
    i_8$Nome_Indicador = "Tempo Medio de Permanencia Hospitalar"
    i_9$Nome_Indicador = "Giro de Leito da Producao Hospitalar"

    #Renomear a coluna do indicador
    colnames(i_1)[ncol(i_1)-2] = "Indicador"
    colnames(i_2)[ncol(i_2)-2] = "Indicador"
    colnames(i_3)[ncol(i_3)-2] = "Indicador"
    colnames(i_4)[ncol(i_4)-2] = "Indicador"
    colnames(i_5)[ncol(i_5)-2] = "Indicador"
    colnames(i_6)[ncol(i_6)-2] = "Indicador"
    colnames(i_7)[ncol(i_7)-2] = "Indicador"
    colnames(i_8)[ncol(i_8)-2] = "Indicador"
    colnames(i_9)[ncol(i_9)-2] = "Indicador"

    #Converte a coluna indicador para string e deixar so 4 numeros apois a virgula
    i_1$Indicador = sprintf("%.4f", i_1$Indicador)
    i_2$Indicador = sprintf("%.4f", i_2$Indicador)
    i_3$Indicador = sprintf("%.4f", i_3$Indicador)
    i_4$Indicador = sprintf("%.4f", i_4$Indicador)
    i_5$Indicador = sprintf("%.4f", i_5$Indicador)
    i_6$Indicador = sprintf("%.4f", i_6$Indicador)
    i_7$Indicador = sprintf("%.4f", i_7$Indicador)
    i_8$Indicador = sprintf("%.4f", i_8$Indicador)
    i_9$Indicador = sprintf("%.4f", i_9$Indicador)

    #Empilha as bases em uma unica base
    tabela_indicadores_empilhada = dplyr::bind_rows(
      i_1,i_3,i_2,i_4,i_5,i_6,i_7,i_8,i_9)

    return(tabela_indicadores_empilhada)
}
