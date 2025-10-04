
#' Grupiranje dviju kategorijalnih varijabli
#'
#' Funkcija \code{group_2char_var} omogućuje odabir dviju kategorijalnih varijabli
#' iz \code{data.frame}-a, izračunava kontigencijsku tablicu, postotke po redcima, te provodi 
#' Pearsonov chi-square test i chi-square test s Yatesovom korekcijom te stacked bar grafikon pomoću \code{plotly}.
#'
#' @param x, y Varijable se odabiru putem interaktivnog izbornika.
#'
#' @return Lista sa sljedećim komponentama:
#' \itemize{
#'   \item \strong{ctm} - kontigencijska tablica s dodanim marginama (Sum row/col)
#'   \item \strong{ctprm} - postotci po redcima s dodanim sumama
#'   \item \strong{hit} - tekstualni rezultat chi-square testa s Yatesovom korekcijom
#'   \item \strong{hif} - tekstualni rezultat Pearsonovog chi-square testa
#'   \item \strong{bar_stack} - grafikon s razdjeljenim stupcima (stacked bar)
#' }
#'
#' @details
#' Funkcija koristi paket \code{plotly}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' Varijable se odabiru putem interaktivnog izbornika, a funkcija radi samo s faktorskim
#' ili karakter varijablama.
#'
#' @examples
#' \dontrun{
#' # Primjer korištenja s data.frame-om df
#' group_2char_var(df)
#' }
#'
#' @importFrom plotly plot_ly layout
#' @export
group_2char_var <- function(x, y) {
  if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
  library(plotly)

  c_var <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  s_var1 <- select.list(c_var, title = "Odaberi prvu varijablu")
  s_var2 <- select.list(c_var, title = "Odaberi drugu varijablu")
  x <- df[[s_var1]]
  y <- df[[s_var2]]

  ct <- table(x, y)
  ctm <- as.data.frame.matrix(addmargins(ct))
  ctpr <- round(prop.table(ct, margin = 1) * 100, 1)
  ctprm <- cbind(ctpr, Sum = round(rowSums(ctpr), 0))
  
  hif <- chisq.test(ct, correct = FALSE)
  hit <- chisq.test(ct, correct = TRUE)

  ctprdf <- as.data.frame(ctpr)
  colnames(ctprdf) <- c("x_var", "y_var", "Perc")

  bar_stack <- plot_ly(
    data = ctprdf,
    x = ~x_var,
    y = ~Perc,
    color = ~y_var,
    type = "bar"
  ) %>% layout(
    barmode = "stack",
    xaxis = list(title = deparse(substitute(x))),
    yaxis = list(title = "%")
  )

  hif_text <- paste0(
    "Pearson's chi-squared test:\n",
    "Chi-square = ", round(hif$statistic, 3),
    ", df = ", hif$parameter,
    ", p-value = ", round(hif$p.value, 4), "\n"
  )

  hit_text <- paste0(
    "Chi-square test with Yates' correction:\n",
    "Chi-square = ", round(hit$statistic, 3),
    ", df = ", hit$parameter,
    ", p-value = ", round(hit$p.value, 4), "\n"
  )

  return(list(
    ctm = ctm,
    ctprm = ctprm,
    hit = hit_text,
    hif = hif_text,
    bar_stack = bar_stack
  ))
}
