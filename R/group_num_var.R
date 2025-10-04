#' Grupiranje numeričke varijable
#'
#' Funkcija \code{group_num_var} omogućuje odabir jedne numeričke varijable iz
#' \code{data.frame}-a, kreira frekvencijsku tablicu prema rezredima (bins) i
#' prikazuje histogram te frekvencijski poligon koristeći \code{plotly}.
#'
#' @param x varijabla se odabire putem interaktivnog izbornika.
#' @param br_razreda Broj razreda (bins) za histogram. Ako je \code{NULL}, koristi se Sturgesovo pravilo.
#'
#' @return Lista sa sljedećim komponentama:
#' \itemize{
#'   \item \strong{tf} - tablica frekvencija, postotaka i kumulativnih vrijednosti
#'   \item \strong{hist} - histogram frekvencija
#'   \item \strong{poli} - poligon frekvencija
#' }
#'
#' @details
#' Funkcija koristi paket \code{plotly}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' Varijabla se odabire putem interaktivnog izbornika, a broj razreda se može zadati
#' ručno ili koristiti Sturgesovo pravilo za automatski izračun.
#'
#' @examples
#' \dontrun{
#' # Primjer korištenja s data.frame-om df
#' group_num_var(df)
#' group_num_var(df, br_razreda = 8)
#' }
#'
#' @importFrom plotly plot_ly layout
#' @export
group_num_var <- function(x, br_razreda = NULL) {
  if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
  library(plotly)

  num_vars <- names(df)[sapply(df, is.numeric)]
  nvar <- select.list(num_vars, multiple = FALSE, title = "Odaberi varijablu:")
  x <-  df[[nvar]]

  n <- length(x)
  if (is.null(br_razreda)) {
    br_razreda <- ceiling(1 + log2(n))  # Sturges default
  }
  
  ir <- (max(x) - min(x)) / (br_razreda-1)
  ir2 <- ir / 2
  breaks <- round(seq(min(x) - ir2, max(x) + ir2, by = ir), digits = 2)
  
  f.cut <- cut(x, breaks, right = TRUE)
  d.freq <- table(f.cut)
  tf <- cbind.data.frame(d.freq)
  colnames(tf) <- c("Bins", "Frequency")
  tf$Percent <- round(tf$Frequency / sum(tf$Frequency) * 100, 2)
  tf$Cumulative <- cumsum(tf$Frequency)
  tf$`Cumulative Percent` <- round(cumsum(tf$Percent), 2)
  
  hist <- plot_ly(
    data = tf,
    x = ~Bins,
    y = ~Frequency,
    type = 'bar',
    text = ~Frequency,
    textposition = 'outside'
  ) %>%
    layout(title = "Histogram", bargap = 0.01)
  
  ggr <- breaks[-1]
  cf <- data.frame(GGR = ggr, RCF = tf$`Cumulative Percent`)
  
  pol <- plot_ly(
    data = cf,
    x = ~GGR,
    y = ~RCF,
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
    layout(
      title = "Frequency Polygons",
      xaxis = list(title = "Upper bins limits"),
      yaxis = list(title = "%", dtick = 10)
    )
  
  return(list(tf = tf, hist = hist, poli = pol))
}
