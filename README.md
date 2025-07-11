# PaRGHA - Pacote R para Gestão Hospitalar Automatizado

<!-- badges: start -->
<!--[![R-CMD-check](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml)-->
[![CRAN status](https://www.r-pkg.org/badges/version/microdatasus)](https://CRAN.R-project.org/package=microdatasus)
<!-- badges: end -->

Este pacote foi desenvolvido para automatizar a coleta, o tratamento e a geração de indicadores hospitalares com base nos dados dos sistemas de informação em saúde do DATASUS. Criado como parte de um sistema de Business Intelligence (BI) voltado à Rede EBSERH, ele visa apoiar gestores hospitalares na tomada de decisão, promovendo maior agilidade, padronização e reprodutibilidade na produção de informações estratégicas em saúde.

Além disso, os indicadores gerados pelo pacote podem ser visualizados por meio de um painel interativo, disponível no diretório inst/extdata/painel_DB.pbix. Para utilizá-lo, basta alterar a fonte dos dados para os arquivos gerados localmente pelo usuário.

Atualmente, o pacote utiliza os seguintes Sistemas de Informação em Saúde (SIS) disponibilizados pelo DATASUS: SIH-RD, SIH-RJ, SIH-SP, CNES-LT, CNES-HB, CNES-ST e CNES-EQ.

Para uma descrição detalhada de cada um desses sistemas, recomenda-se a leitura do e-book elaborado por Raphael Saldanha, disponível neste [link](https://rfsaldanha.github.io/sis/).


# Instalação
Para instalar o pacote **PaRGHA**, é necessário que o pacote `remotes` já esteja instalado no seu ambiente R.

Além disso, o pacote `read.dbc` também deve ser instalado, pois ele é utilizado para a leitura dos arquivos no formato `.dbc`, disponibilizados pelo DataSUS.

Abaixo está o código para instalar o pacote `read.dbc`:

```r
remotes::install_github("danicat/read.dbc")
```
Por fim, utilize o código abaixo para instalar o pacote PaRGHA:
```r
remotes::install_github("gustaavobraga/PaRGHA")
```
# Funções
- `auto_PaRGHA()` função principal do pacote, responsável por executar todas as etapas do processo, incluindo o download dos dados e a criação das tabelas de indicadores e de labels exigidas pelo painel. Todas as tabelas geradas são salvas em um arquivo .RData no diretório local do usuário ("./
  ").

As funções a seguir podem ser utilizadas caso o usuário deseje obter cada uma das tabelas individualmente.
- `leitos()`: realiza o download dos microdados e gera a tabela de indicadores relacionada aos leitos.
- `habilitacoes()`: realiza o download dos microdados e gera a tabela de indicadores relacionada às habilitações.
- `equipamentos()`: realiza o download dos microdados e gera a tabela de indicadores relacionada aos equipamentos.
- `indicadores()`: realiza o download dos microdados e cria a tabela de valores absolutos, além das colunas utilizadas no cálculo dos indicadores.

# Exemplos
```r
library(PaRGHA)

auto_PaRGHA(
  year_start = 2023,
  month_start = 10,
  year_end = 2024,
  month_end = 2,
  cloud = FALSE,
  save_Rdata_to_path = "./"
)
```
Para obter as tabelas individualmente:
```r
df_leitos <- leitos(
  year_start = 2024,
  month_start = 1,
  year_end = 2024,
  month_end = 1,
  labels = TRUE
)

df_habilitacoes <- habilitacoes(
  year_start = 2024,
  month_start = 1,
  year_end = 2024,
  month_end = 1,
  labels = TRUE
)

df_equipamentos <- equipamentos(
  year_start = 2024,
  month_start = 1,
  year_end = 2024,
  month_end = 2,
  labels = TRUE
)
```
Para gerar a tabela de indicadores, é necessário obter previamente duas outras tabelas,
```r
data_CNES = get_data_CNES(
  year_start = 2024,
  month_start = 1,
  year_end = 2024,
  month_end = 2,
  state_abbr = "all",
  type_data = "LT",
  save_path = tempdir()
)
data_SIH = get_data_SIH(
  year_start = 2024,
  month_start = 1,
  year_end = 2024,
  month_end = 2,
  save_path = tempdir()
)
indicadores = indicadores(data_SIH = data_SIH,
                          data_CNES = data_CNES,
                          labels_CNES = TRUE)
```

# Informações de contato
Caso tenha alguma dúvida, entre em contato comigo pelo e [gustaa1320\@gmail.com](mailto:gusta1320@gmail.com).
