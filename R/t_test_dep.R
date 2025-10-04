#' T-test za zavisne uzorke za više varijabli
#'
#' Ova funkcija omogućuje izvođenje t-testova zavisne uzorke za odabrane parove varijabli u data frame-u.
#' Također izračunava aritmetičku sredinu, standardnu pogrešku razlike, t-vrijednost, stupnjeve slobode,
#' p-vrijednost i Cohenov d.
#'
#' @param df Data frame koji sadrži varijable za analizu.
#' 
#' @return Data frame sa sljedećim kolonama:
#' \itemize{
#'   \item VARS1 - naziv varijable prvog mjerenja
#'   \item VARS2 - naziv varijable drugog mjerenja
#'   \item MEAN1 - aritmetička sredina prvog mjerenja
#'   \item MEAN2 - aritmetička sredina drugog mjerenja
#'   \item `MEAN-DIF` - razlika aritmetičkh sredina
#'   \item SED - standardna pogreška razlike
#'   \item T - t-statistika
#'   \item DF - stupnjevi slobode
#'   \item P - p-vrijednost
#'   \item `Cohen's D` - Cohenov d
#' }
#'
#' @details Funkcija koristi interaktivni izbor varijabli prvog i drugog mjerenja.
#' Broj varijabli u prvom i drugom mjerenju mora biti jednak.
#' Funkcija koristi paket \code{lsr}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#'
#' @examples
#' \dontrun{
#' rezultati <- t_test_dep(df)
#' }
#'
#' @export
t_test_dep <- function(df) {
  if(!requireNamespace("lsr", quietly = TRUE)) install.packages("lsr")
  library(lsr)

  var_prvo <- select.list(names(df), title = "Odaberi varijable 1. mjerenja", multiple = TRUE)
  var_drugo <- select.list(names(df), title = "Odaberi varijable 2. mjerenja", multiple = TRUE)
  
  if(length(var_prvo) != length(var_drugo)){
    stop("Broj varijabli prvog mjerenja mora biti jednak broju varijabli drugog mjerenja!")
  }
  
  t_tab <- data.frame(
    Var_1 = character(),
    Var_2 = character(),
    Mean_1 = numeric(),
    Mean_2 = numeric(),
    Mean_diff = numeric(),
    sed = numeric(),
    t = numeric(),
    df = numeric(),
    p = numeric(),
    d = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(var_prvo)){
    v_prvo <- var_prvo[i]
    v_drugo <- var_drugo[i]
    t_res <- t.test(df[[v_prvo]], df[[v_drugo]], paired = TRUE)
    t_tab <- rbind(t_tab, data.frame(
      Var_1 = v_prvo,
      Var_2 = v_drugo,
      Mean_1 = round(mean(df[[v_prvo]]), 3),
      Mean_2 = round(mean(df[[v_drugo]]), 3),
      Mean_diff = round(unname(t_res$estimate), 3) * -1,
      sed = round(t_res$stderr, 3),
      t = abs(round(t_res$statistic, 3)),
      df = round(t_res$parameter, 3),
      p = round(t_res$p.value, 4),
      d = round(lsr::cohensD(df[[v_prvo]], df[[v_drugo]], method = "paired"), 3)
    ))
  }
  
  colnames(t_tab) <- c("VARS1", "VARS2", "MEAN1", "MEAN2", "MEAN-DIF", "SED", "T", "DF", "P", "Cohen's D")
  
  return(t_tab)
}
