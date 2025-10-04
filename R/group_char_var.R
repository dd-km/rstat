#' Grupiranje kategorijalne varijable
#'
#' Funkcija \code{group_char_var} omogućuje odabir jedne kategorijalne varijable iz
#' \code{data.frame}-a, izračunava frekvencije i postotke po kategorijama, 
#' očekivane frekvencije za uniformnu distribuciju, te provodi chi-square test.
#' Kreira interaktivni grafikon s jednostavnim stupcima (bar chart) i strukturni krug (pie chart) pomoću \code{plotly}.
#'
#' @param x Varijabla se biraju putem interaktivnog izbornika.
#'
#' @return Lista sa sljedećim komponentama:
#' \itemize{
#'   \item \strong{ft} - tablica frekvencija
#'   \item \strong{hi} - tekstualni rezultat chi-square testa
#'   \item \strong{bar} - grafikon s jednostavnim stupcima (bar chart)
#'   \item \strong{pie} - strukturni krug (pie chart)
#' }
#'
#' @details
#' Funkcija koristi paket \code{plotly}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' Varijabla se biraju putem interaktivnog izbornika, a funkcija radi samo s kategorijalnim varijablama.
#'
#' @examples
#' \dontrun{
#' # Primjer korištenja s data.frame-om df
#' group_char_var(df)
#' }
#'
#' @importFrom plotly plot_ly
#' @export
group_char_var <- function(x) {
  if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
  library(plotly)
  
# Odabir kvalitativne varijable
  cat_vars <- names(df)[sapply(df, is.character)]
  cvar <- select.list(cat_vars, multiple = FALSE, title = "Odaberi kvalitativnu varijablu:")
  x <- df[[cvar]]
  freq <- table(x)
  relfreq <- prop.table(freq) * 100
  ft <- data.frame(
    Groups = names(freq),
    Observed_Frequency = as.numeric(freq),
    Observed_Percent = round(as.numeric(relfreq), 2),
    Expected_Frequency = round(rep(sum(freq) / length(freq), length(freq)), 2),
    Expected_Percent = round(rep(1 / length(freq) * 100, length(freq)), 2)
  )
  hi <- chisq.test(x = ft$Observed_Frequency, 
                   p = rep(1 / nrow(ft), nrow(ft)))
  bar <- plot_ly(
    data = ft, 
    x = ~Groups, 
    y = ~Observed_Frequency, 
    type = 'bar',
    text = ~Observed_Frequency,
    textposition = 'outside'
  )
  pie <- plot_ly(
    data = ft, 
    labels = ~Groups, 
    values = ~Observed_Percent, 
    type = 'pie',
    textinfo = 'label+percent', 
    textposition = 'outside'
  )
  hi <- paste0(
    "Chi-square = ", round(hi$statistic, 2),
    ", df = ", hi$parameter,
    ", p = ", round(hi$p.value, 4)
  )
  return(list(
    ft = ft,
    hi = hi,
    bar = bar,
    pie = pie
  ))
}