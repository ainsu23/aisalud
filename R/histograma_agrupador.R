#
# Analítica Integrada Salud
#
# Derechos de autor 2021 por MD&CO Consulting Group (NIT 901.119.781-5)
# Copyright (C) 2021 by MD&CO Consulting Group
#
# Este programa es software libre: puede redistribuirlo o modificarlo bajo
# los términos de la licencia Affero General Public License tal cual
# publicada por la Free Software Foundation, sea la versión 3 de la licencia
# o cualquier versión posterior. Este programa se distribuye SIN GARANTÍA
# EXPERSA O IMPLÍCITA, INCLUIDAS LAS DE NO INFRACCIÓN, COMERCIABILIDAD O
# APTITUD PARA UN PROPÓSITO PARTICULAR. Referir a la
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) para más detalles.
#

#' @title Histograma de valores por segmentación
#' @description Gráfico con histograma de valores distribuido por segmentación
#'
#' @param data tabla con información que contenga edades
#' @param titulo caracter nombre de la columna con los valores
#' @param numero_bins Número entero de edades contenidas en la población
#' @param columna_sep variable de segmentación 
#' @return gráfica con histograma
#' @examples
#' histograma_agrupador(
#'  data = edades_pacientes,
#'  columna_numero = "edad",
#'  columna_sep = segmentacion,
#'  numero_bins = numero_bins)


histograma_agrupador <- function(data, titulo = "Valor", numero_bins = NULL,
                                 columnas_sep) {

  histograma <- plot_ly(type = "bar")

  lapply(
    X = names(data),
    FUN = function(i) {
      if (!is.null(data[[i]])) {
        if (!is.null(numero_bins)) {
          x <- data[[i]] %>%
            ungroup() %>%
            right_join(columnas_sep, copy = TRUE) %>%
            db_compute_bins(valor_calculos, bins = numero_bins)
        } else {
          x <- data[[i]] %>%
            ungroup() %>%
            right_join(columnas_sep, copy = TRUE) %>%
            db_compute_bins(valor_calculos)
        }
        histograma <<- histograma %>%
          add_bars(x = x[["valor_calculos"]],
                    y = x[["count"]],
                    name = toupper(i))
      }
    }
  )

  histograma %>%
    config(locale = "es") %>%
    layout(barmode = "overlay",
           xaxis = list(title = "Valor",
                        tickformat = ",.2f"),
           yaxis = list(title = "Conteo",
                        tickformat = ",.2f"),
           showlegend = TRUE)
}
