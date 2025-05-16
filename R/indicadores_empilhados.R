
#' Retorna a base de dados com todos os indicadores empilhados
#'
#' @description Reestrutura a base de dados geradada pela função indicadores(), criando uma base de dados onde os indicadores estão disposto todos em uma unica coluna.
#'
#' @param dados a base de dados criada pela função indicadores()
#'
#' @return Um DataFrame com os indicadores empilhados
#'
#' @examples
#' \dontrun{
#'   dados_empilhados = indicadores_empilhados(dados)
#' }
#'
#' @export
indicadores_empilhados <-
  function(dados) {
    `%>%` = dplyr::`%>%`

    # Lista com os nomes dos indicadores e suas colunas correspondentes
    indicadores_info = list(
      "N\u00FAmero de Cirurgias Apresentadas"               = "N_CIRURGIAS_APRE",
      "N\u00FAmero de Interna\u00E7\u00F5es Hospitalares"   = "N_Internacoes_H",
      "Taxa de Ces\u00E1rea"                                = "i_taxa_de_cesarea",
      "Taxa de Ocupa\u00E7\u00E3o Hospitalar"               = "i_taxa_ocupacao_H",
      "Taxa de Ocupa\u00E7\u00E3o de UTI"                   = "i_taxa_ocupacao_UTI",
      "Tempo M\u00E9dio de Perman\u00EAncia Cir\u00FArgica" = "i_tempo_medio_permanencia_cirurgica",
      "Tempo M\u00E9dio de Perman\u00EAncia Cl\u00EDnica"   = "i_tempo_medio_permanencia_clinica",
      "Tempo M\u00E9dio de Perman\u00EAncia Hospitalar"     = "i_tempo_medio_permanencia_hospitalar",
      "Giro de Leito da Produ\u00E7\u00E3o Hospitalar"      = "i_giro_de_leito"
    )

    # Funcao para criar e formatar cada sub-dataframe
    criar_df_indicador = function(coluna, nome_indicador) {
      dados %>%
        dplyr::select(ANO_CMPT, MES_CMPT, CNES, Name_CNES, Indicador = all_of(coluna)) %>%
        dplyr::mutate(
          Nome_Indicador = nome_indicador,
          Indicador = round(as.numeric(Indicador), 4),
          Indicador = gsub("\\.", ",", as.character(Indicador))
        )
    }

    # Aplicar a funcao a todos os indicadores
    lista_indicadores =
      purrr::imap(indicadores_info, criar_df_indicador)

    # Empilhar todos os data frames em um so
    indicadores_empilhada = bind_rows(lista_indicadores)

    return(indicadores_empilhada)
  }
