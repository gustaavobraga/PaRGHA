

#' Cria a base de dados dos Equipamentos
#'
#' @description Cria a base de dados dOs equipamentos dos estabelecimentos EBSERH
#'
#' @param year_start numeric. Ano inicial para o download dos dados, no formato yyyy.
#' @param month_start numeric. Mês inicial para o download dos dados, no formato mm.
#' @param year_end numeric. Ano final para o download dos dados, no formato yyyy.
#' @param month_end numeric. Mês final para o download dos dados, no formato mm.
#'
#' @return Um DataFrame com os dados dos equipamentos
#'
#'
#' @examples
#' \dontrun{
#'   dados = equipamentos(
#'     year_start = 2024,
#'     month_start = 1,
#'     year_end = 2024,
#'     month_end = 1
#'  )
#' }
#'
#' @export
equipamentos <-
  function(year_start,
           month_start,
           year_end,
           month_end) {
    `%>%` <- dplyr::`%>%`

    estados = c(
      "PE","SE","BA","MS","DF","RJ","MG","AL","AM",
      "RS","PA","GO","PR","PB","RN","CE","MT","MA",
      "SC","PI","AP","TO","ES","SP"
    )

    data_cnes = get_data_CNES(
      year_start = year_start,
      month_start = month_start,
      year_end = year_end,
      month_end = month_end,
      type_data = "EQ",
      save_path = tempdir(),
      state_abbr = estados
    )

    #Renomeando o nome da coluna
    names(data_cnes)[4] = 'Tipo_Equipamento'
    names(data_cnes)[5] = 'Cod_Equipamento'
    data_cnes = data_cnes %>%
      dplyr::mutate(QT_N_USO = QT_EXIST - QT_USO)

    data_cnes = data_cnes %>% dplyr::select(
      "ANO_CMPT",
      "MES_CMPT",
      "CNES",
      "Cod_Equipamento",
      "QT_EXIST",
      "QT_USO",
      "Tipo_Equipamento"
    )

    data_cnes = data_cnes %>%
      dplyr::mutate(
        Tipo_Equipamento = dplyr::case_match(
          Tipo_Equipamento,
          "1" ~ "Equipamentos De Diagnostico Por Imagem",
          "2" ~ "Equipamentos De Infra-Estrutura",
          "3" ~ "Equipamentos Por M\u00e9todos \u00d3pticos",
          "4" ~ "Equipamentos Por M\u00e9todos Gr\u00e1ficos",
          "5" ~ "Equipamentos Para Manuten\u00e7\u00e3o Da Vida",
          "6" ~ "Outros Equipamentos",
          "7" ~ "Equipamentos De Odontologia",
          "8" ~ "Equipamentos De Audiologia",
          "9" ~ "Equipamentos De Telessa\u00fade",
          .default = Tipo_Equipamento
        )
      ) %>% dplyr::relocate(Tipo_Equipamento, .before = 4)

    data_cnes = data_cnes %>%
      dplyr::mutate(
        Cod_Equipamento = dplyr::case_match(
          Cod_Equipamento,
          "01" ~ "Gama C\u00e2mara",
          "02" ~ "Mam\u00f3grafo com Comando Simples",
          "03" ~ "Mam\u00f3grafo com Estereotaxia",
          "04" ~ "Raio X At\u00e9 100 Ma",
          "05" ~ "Raio X de 100 A 500 Ma",
          "06" ~ "Raio X Mais de 500ma",
          "07" ~ "Raio X Dent\u00e1rio",
          "08" ~ "Raio X com Fluoroscopia",
          "09" ~ "Raio X para Densitometria \u00d3ssea",
          "10" ~ "Raio X para Hemodin\u00e2mica",
          "11" ~ "Tom\u00f3grafo Computadorizado",
          "12" ~ "Resson\u00e2ncia Magn\u00e9tica",
          "13" ~ "Ultrassom Doppler Colorido",
          "14" ~ "Ultrassom Ec\u00f3grafo",
          "15" ~ "Ultrassom Convencional",
          "16" ~ "Processadora de Filme Exclusiva Para Mamografia",
          "17" ~ "Mam\u00f3grafo Computadorizado",
          "18" ~ "PET/CT",
          "19" ~ "Ar Condicionado",
          "20" ~ "Camera Frigorifica",
          "21" ~ "Controle Ambiental/Ar-condicionado Central",
          "22" ~ "Grupo Gerador",
          "23" ~ "Usina de Oxig\u00eanio",
          "24" ~ "Camara para Conserva\u00e7\u00e3o de Hemoderivados/Imuno/Termolabeis",
          "25" ~ "Camara para Conserva\u00e7\u00e3o de Imunobiologicos",
          "26" ~ "Condensador",
          "27" ~ "Freezer Cient\u00edfico",
          "28" ~ "Grupo Gerador (101 A 300 KVA)",
          "29" ~ "Grupo Gerador (8 A 100 KVA)",
          "30" ~ "Grupo Gerador (Acima de 300 KVA)",
          "31" ~ "Endosc\u00f3pio das Vias Respirat\u00f3rias",
          "32" ~ "Endosc\u00f3pio das Vias Urinarias",
          "33" ~ "Endosc\u00f3pio Digestivo",
          "34" ~ "Equipamentos para Optometria",
          "35" ~ "Laparosc\u00f3pico/V\u00eddeo",
          "36" ~ "Microsc\u00f3pio Cir\u00fargico",
          "37" ~ "Cadeira Oftalmol\u00f3gica",
          "38" ~ "Coluna Oftalmol\u00f3gica",
          "39" ~ "Refrator",
          "40" ~ "Lensometro",
          "41" ~ "Eletrocardi\u00f3grafo",
          "42" ~ "Eletroencefal\u00f3grafo",
          "43" ~ "Grupo Gerador de 1.500 (m\u00ednimo)",
          "44" ~ "Projetor ou Tabela de Optotipos",
          "45" ~ "Retinosc\u00f3pio",
          "46" ~ "Oftalmosc\u00f3pio",
          "47" ~ "Ceratometro",
          "48" ~ "Ton\u00f4metro de Aplana\u00e7\u00e3o",
          "49" ~ "Biomicroscopio (L\u00e2mpada de Fenda)",
          "50" ~ "Campimetro",
          "51" ~ "Bomba/Bal\u00e3o Intra-a\u00f3rtico",
          "52" ~ "Bomba de Infus\u00e3o",
          "53" ~ "Ber\u00e7o Aquecido",
          "54" ~ "Bilirrubinometro",
          "55" ~ "Debit\u00f4metro",
          "56" ~ "Desfibrilador",
          "57" ~ "Equipamento de Fototerapia",
          "58" ~ "Incubadora",
          "59" ~ "Marca-passo Tempor\u00e1rio",
          "60" ~ "Monitor de ECG",
          "61" ~ "Monitor de Press\u00e3o Invasivo",
          "62" ~ "Monitor de Press\u00e3o N\u00e3o Invasivo",
          "63" ~ "Reanimador Pulmonar/Ambu",
          "64" ~ "Respirador/Ventilador",
          "65" ~ "Grupo Gerador Port\u00e1til (at\u00e9 7 KVA)",
          "66" ~ "Refrigerador",
          "67" ~ "Caminh\u00e3o Ba\u00fa Refrigerado",
          "68" ~ "Embarca\u00e7\u00e3o para Transporte com Motor Popa (at\u00e9 12 pessoas)",
          "69" ~ "Empilhadeira",
          "70" ~ "Ve\u00edculo Utilit\u00e1rio (Tipo Furg\u00e3o)",
          "71" ~ "Aparelho de Diatermia por Ultrassom/Ondas Curtas",
          "72" ~ "Aparelho de Eletroestimula\u00e7\u00e3o",
          "73" ~ "Bomba de Infus\u00e3o de Hemoderivados",
          "74" ~ "Equipamentos de Af\u00e9rese",
          "75" ~ "Equipamento de Circula\u00e7\u00e3o Extracorp\u00f3rea",
          "76" ~ "Equipamento Para Hemodi\u00e1lise",
          "77" ~ "Forno de Bier",
          "78" ~ "Ve\u00edculo Pick-up Cabine Dupla 4x4 (Diesel)",
          "79" ~ "Equipo Odontol\u00f3gico Completo",
          "80" ~ "Compressor Odontol\u00f3gico",
          "81" ~ "Fotopolimerizador",
          "82" ~ "Caneta de Alta Rota\u00e7\u00e3o",
          "83" ~ "Caneta de Baixa Rota\u00e7\u00e3o",
          "84" ~ "Amalgamador",
          "85" ~ "Aparelho de Profilaxia C/Jato de Bicarbonato",
          "86" ~ "Emiss\u00f5es Otoac\u00fasticas Evocadas Transientes",
          "87" ~ "Emiss\u00f5es Otoac\u00fasticas Evocadas por Produto de Distor\u00e7\u00e3o",
          "88" ~ "Potencial Evocado Auditivo de Tronco Encef\u00e1lico Autom\u00e1tico",
          "89" ~ "Pot Evocado Aud Tronco Encef de Curta, Media e Longa Lat\u00eancia",
          "90" ~ "Audi\u00f4metro de Um Canal",
          "91" ~ "Audi\u00f4metro de Dois Canais",
          "92" ~ "Imitanciometro",
          "93" ~ "Imitanciometro Multifrequencial",
          "94" ~ "Cabine Ac\u00fastica",
          "95" ~ "Sistema de Campo Livre",
          "96" ~ "Sistema Completo de Refor\u00e7o Visual (VRA)",
          "97" ~ "Ganho de Inser\u00e7\u00e3o",
          "98" ~ "Hi-Pro",
          .default = Cod_Equipamento
        )
      )

    return(data_cnes)
  }
