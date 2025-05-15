
#' Allocate files into groups with a size limit.
#'
#' @description Uma heurística de empacotamento para alocar arquivos em grupos com limite de tamanho.
#'
#' @param files_name Um vetor contendo os nomes dos arquivos.
#'
#' @return files_chunks, uma lista de vetores de strings.
#'
#' @examples \dontrun{chunk( files_name = files_name)}
#'
#' @export
chunk_fast <- function(files_name) {
  # Obter tamanhos dos arquivos
  tamanhos <- file.info(files_name)$size

  # Ordenar do maior para o menor
  ordenado <- order(-tamanhos)
  arquivos <- files_name[ordenado]
  tamanhos <- tamanhos[ordenado]

  # Parâmetro de limite (10000 KB)
  limite_grupo <- 10000 * 1024

  # Inicialização
  grupos <- vector("list", length(arquivos))
  somas <- numeric()
  n_grupos <- 0

  for (i in seq_along(arquivos)) {
    arq <- arquivos[i]
    tam <- tamanhos[i]

    alocado <- FALSE

    for (j in seq_len(n_grupos)) {
      if (somas[j] + tam <= limite_grupo) {
        grupos[[j]] <- c(grupos[[j]], arq)
        somas[j] <- somas[j] + tam
        alocado <- TRUE
        break
      }
    }

    if (!alocado) {
      n_grupos <- n_grupos + 1
      grupos[[n_grupos]] <- arq
      somas[n_grupos] <- tam
    }
  }

  return(grupos[1:n_grupos])
}
