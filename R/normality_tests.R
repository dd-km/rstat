#' Testovi normalnosti za numeričke varijable
#'
#' Funkcija \code{normality_tests} omogućuje odabir jedne ili više numeričkih varijabli iz 
#' \code{data.frame}-a i provođenje odabranog testa normalnosti: Kolmogorov-Smirnov, 
#' Shapiro-Wilk, Lilliefors ili Anderson-Darling. Također može dodati interpretaciju rezultata.
#'
#' @param df \code{data.frame} koji sadrži numeričke varijable za testiranje.
#' @param interpret Logička vrijednost. Ako je TRUE (default), funkcija dodaje kolonu s interpretacijom rezultata (normalno / nije normalno).
#'
#' @return Lista sa sljedećim komponentama:
#' \itemize{
#'   \item \strong{table} - tablica rezultata testa za svaku odabranu varijablu
#'   \item \strong{test} - naziv odabranog testa
#' }
#'
#' @details
#' Funkcija koristi paket \code{nortest}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' Varijable se odabiru putem interaktivnog izbornika. Shapiro-Wilk test nije primjenjiv za više od 5000 entiteta i tada vraća NA.
#'
#' @examples
#' \dontrun{
#' # Primjer korištenja s data.frame-om df
#' normality_tests(df)
#' }
#'
#' @importFrom nortest lillie.test ad.test
#' @export
normality_tests <- function(df, interpret = TRUE) {
  if(!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")
  library(nortest)

  num_vars <- names(df)[sapply(df, is.numeric)]
  nvars <- select.list(num_vars, multiple = TRUE, title = "Odaberi varijable:")

  options <- c("Kolmogorov-Smirnov", "Shapiro-Wilk", "Lilliefors", "Anderson-Darling")
  choice <- utils::select.list(options, title = "Odaberi test normalnosti")
  
  if (length(nvars) == 0 || choice == "") {
    stop("Niste odabrali test ili dataframe nema numeričkih varijabli.")
  }
  
 results <- lapply(nvars, function(v) {
    x <- df[[v]]
    
    rez <- switch(choice,
      "Kolmogorov-Smirnov" = ks.test(scale(x), "pnorm", mean = 0, sd = 1),
      "Shapiro-Wilk"       = if(length(x) <= 5000) shapiro.test(x) else list(statistic = NA, p.value = NA),
      "Lilliefors"         = nortest::lillie.test(x),
      "Anderson-Darling"   = nortest::ad.test(x)
    )
    
    data.frame(
      Variable      = v,
      Statistic     = round(rez$statistic, 3),
      p_value       = round(rez$p.value, 4),
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results)
  
  colnames(results_df) <- switch(choice,
    "Kolmogorov-Smirnov" = c("Variable", "maxD", "p-value"),
    "Shapiro-Wilk"       = c("Variable", "W", "p-value"),
    "Lilliefors"         = c("Variable", "D", "p-value"),
    "Anderson-Darling"   = c("Variable", "A", "p-value")
  )
  
  if (interpret) {
    results_df$Interpretacija <- ifelse(results_df[["p-value"]] > 0.05,
                                        "Normalno (p > 0.05)",
                                        "Nije normalno (p ≤ 0.05)")
  }
  
  return(list(
    table = results_df,
    test = choice
  ))
}
