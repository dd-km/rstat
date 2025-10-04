#' Deskriptivna statistika po grupama
#'
#' Funkcija računa osnovne deskriptivne pokazatelje (N, MEAN, SD, SE) za numeričke
#' varijable grupirane po kategorijalnoj varijabli. Korisnik bira varijable putem interaktivnog izbornika.
#'
#' @param df Data frame koji sadrži podatke.
#'
#' @return Data frame sa stupcima:
#'   \item{VAR}{Naziv varijable}
#'   \item{GROUP}{Naziv grupe}
#'   \item{N}{Broj entiteta za svaku grupu}
#'   \item{MEAN}{Aritmetička sredina za svaku grupu}
#'   \item{SD}{Standardna devijacija za svaku grupu}
#'   \item{SE}{Standardna greška aritmetičke sredine za svaku grupu}
#'
#' @examples
#' rezultat <- des_group(df)
#'
#' @export
des_group <- function(df) {
  qual_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  quant_vars <- names(df)[sapply(df, is.numeric)]
  group_var <- select.list(qual_vars, title = "Odaberi grupnu varijablu")
  dep_vars <- select.list(quant_vars, title = "Odaberi zavisne varijable", multiple = TRUE)

  deskriptiva_fun <- function(x) {
    n <- length(x)
    m <- mean(x)
    sdv <- sd(x)
    se <- sdv / sqrt(n)
    return(c(N = n, MEAN = round(m,3), SD = round(sdv,3), SE = round(se,3)))
  }

  res <- do.call(rbind, lapply(dep_vars, function(var) {
    m <- tapply(df[[var]], df[[group_var]], deskriptiva_fun)
    mat <- do.call(rbind, m)
    data.frame(
      VAR = var,
      GROUP = rownames(mat),
      mat,
      row.names = NULL
    )
  }))
  
  return(res)
}
