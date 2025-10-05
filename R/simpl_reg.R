#' Jednostavna linearna regresijska analiza
#'
#' Funkcija provodi jednostavnu linearnu regresiju između dvije numeričke varijable
#' iz zadanog data frame-a i vraća parametre regresijskog modela i grafikone koristeći paket \code{plotly}.
#'
#' @param df Data frame koji sadrži najmanje dvije numeričke varijable. 
#' Funkcija omogućuje korisniku da odabere nezavisnu i zavisnu varijablu.
#'
#' @return Lista koja sadrži:
#' \item{b0}{Intercept (b0) regresije.}
#' \item{b1}{Koeficijent nagiba (b1).}
#' \item{r}{Korelacijski koeficijent .}
#' \item{see}{Standardna greška prognoze.}
#' \item{F}{F-vrijednost.}
#' \item{df1}{Stupnjevi slobode (df1).}
#' \item{df2}{Stupnjevi slobode (df2).}
#' \item{p}{P-vrijednost F-testa.}
#' \item{residuals}{Vektor residuala vrijednosti}
#' \item{fitted}{Vektor prognoziranih vrijednosti.}
#' \item{graph1}{Interaktivni scatter plot sa regresijskom linijom (plotly).}
#' \item{graph2}{Interaktivni grafikon residuala (plotly).}
#'
#' #' @details
#' Ako paket \code{plotly} nije instaliran, funkcija ga automatski instalira
#' i učitava. Odabir varijabli je interaktivan putem \code{select.list}.
#'
#' @examples
#' \dontrun{
#' result <- simpl_reg(mtcars)
#' result$graph1  # prikaz scatter plot s regresijskom linijom
#' result$graph2  # prikaz residualnog grafika
#' }
#'
#' @import plotly
#' @export
simpl_reg <- function(df) {
  if (!require(plotly)) install.packages("plotly")
  library(plotly)

  if (missing(df)) stop("Data frame 'df' nije definiran!")
  num_vars <- names(df)[sapply(df, is.numeric)]
  if (length(num_vars) < 2) stop("Data frame mora imati barem dvije numeričke varijable.")
  
  in_var <- select.list(num_vars, multiple = FALSE, title = "Odaberi nezavisnu varijablu:")
  de_var <- select.list(num_vars, multiple = FALSE, title = "Odaberi zavisnu varijablu:")
  
  x <- df[[in_var]]
  y <- df[[de_var]]
  
  model <- lm(y ~ x)
  res <- summary(model)

  b0 <- coef(model)[1]
  b1 <- coef(model)[2]
  r2 <- res$r.squared
  r <- sqrt(r2)
  see <- res$sigma
  F <- res$fstatistic[1]
  df1 <- res$fstatistic[2]
  df2 <- res$fstatistic[3]
  p <- pf(F, df1, df2, lower.tail = FALSE)
  residuals <- resid(model)
  fitted_vals <- fitted(model)

  fig1 <- plot_ly(x = x, y = y, type = 'scatter', mode = 'markers') %>%
    add_lines(x = x, y = fitted_vals, name='Reg. pravac') %>%
    layout(
      showlegend = FALSE,
      xaxis = list(title = in_var),
      yaxis = list(title = de_var)
    )

  fig2 <- plot_ly(x = fitted_vals, y = residuals, type='scatter', mode='markers') %>%
    add_lines(x = fitted_vals, y = rep(0, length(residuals)), name='y = 0') %>%
    layout(
      showlegend = FALSE,
      xaxis = list(title = "Prognozirane vrijednosti"),
      yaxis = list(title = "Rezidualne vrijednosti")
    )

  return(list(
    b0 = b0, 
    b1 = b1,
    r = r,
    see = see,
    F = F,
    df1 = df1, 
    df2 = df2,
    p = p,
    residuals = residuals,
    fitted = fitted_vals,
    graph1 = fig1,
    graph2 = fig2
  ))
}
