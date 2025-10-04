#' Deskriptivna statistika
#'
#' Funkcija \code{des_par} izračunava osnovne deskriptivne pokazatelje (n, mean, median, sd, 
#' coefficient of variation, min, max, range, skewness, kurtosis) za numeričke varijable
#' u zadanom \code{data.frame}-u. Korisnik bira varijable putem interaktivnog izbornika.
#'
#' @param data A \code{data.frame} koji sadrži numeričke varijable za analizu.
#'
#' @return \code{data.frame} sa sljedećim stupcima:
#' \itemize{
#'   \item \strong{VARS} - naziv varijable
#'   \item \strong{n} - broj entiteta
#'   \item \strong{MEAN} - aritmetička sredina
#'   \item \strong{MEDIAN} - medijan
#'   \item \strong{SD} - standardna devijacija
#'   \item \strong{CV} - koeficijent varijabilnosti (u %)
#'   \item \strong{MIN} - minimalna vrijednost
#'   \item \strong{MAX} - maksimalna vrijednost
#'   \item \strong{RANGE} - totalni raspon (max - min)
#'   \item \strong{SKEW} - skewnes
#'   \item \strong{KURT} - kurtosis
#' }
#'
#' @details
#' Funkcija koristi paket \code{psych}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' 
#' @examples
#' \dontrun{
#' # Primjer korištenja
#' des_par(mtcars)
#' }
#'
#' @importFrom psych describe
#' @export
des_par <- function(data) {
  if(!requireNamespace("psych", quietly = TRUE)) install.packages("psych")
  library(psych)

  num_vars <- names(df)[sapply(df, is.numeric)]
  nvars <- select.list(num_vars, multiple = TRUE, title = "Odaberi varijable:")

  des <- psych::describe(data[, nvars, drop = FALSE])
  
  des_df <- data.frame(
    VARS   = rownames(des),
    n      = round(des$n, 0),
    MEAN   = round(des$mean, 2),
    MEDIAN = round(des$median, 2),
    SD     = round(des$sd, 2),
    CV     = ifelse(des$mean == 0, NA, round(des$sd / des$mean * 100, 1)),
    MIN    = round(des$min, 2),
    MAX    = round(des$max, 2),
    RANGE  = round(des$max - des$min, 2),
    SKEW   = round(des$skew, 2),
    KURT   = round(des$kurtosis, 2)
  )
  return(des_df)
}

