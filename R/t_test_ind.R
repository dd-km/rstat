#' T-test za nezavisne uzorke za više varijabli
#'
#' Ova funkcija izvodi t-test za nezavisne uzorke (Student, Welch ili automatski prema Levene testu)
#' za više kvantitativnih varijabli i jednu kvalitativnu varijablu s dvije kategorije.
#'
#' @param df Data frame koji sadrži podatke.
#' @return Data frame s rezultatima t-testa:
#' \itemize{
#'   \item aritmetičke sredine po grupama,
#'   \item standardne pogreške razlika (SED),
#'   \item t-vrijednosti i stupnjeve slobode (DF),
#'   \item p-vrijednost,
#'   \item 95% interval pouzdanosti,
#'   \item Levene F i p-vrijednosti,
#'   \item Cohenov d,
#'   \item vrstu t-testa (Student/Welch).
#' }
#'
#' @details Funkcija omogućava interaktivni odabir:
#' \itemize{
#'   \item kvalitativne (grupne) varijable,
#'   \item jedne ili više kvantitativnih varijabli,
#'   \item tip t-testa: "Student t-test", "Welch t-test" ili "Auto" (odabire se prema Levene testu za jednakost varijanci).
#' }
#' Funkcija koristi pakete \code{lsr} i \code{car}. Ako paketi nusu instalirani, funkcija će ih automatski instalirati i učitati.
#'
#' @examples
#' \dontrun{
#' library(readxl)
#' df <- read_excel(file.choose())
#' rezultati <- t_test_ind(df)
#' print(rezultati)
#' }
#'
#' @importFrom car leveneTest
#' @importFrom effsize cohen.d
#' @export
t_test_ind <- function(df) {
  if(!requireNamespace("effsize", quietly = TRUE)) install.packages("effsize")
  if(!requireNamespace("car", quietly = TRUE)) install.packages("car")
  library(effsize)
  library(car)

  # Odabir kvalitativne varijable
  cat_vars <- names(df)[sapply(df, is.character)]
  cvar <- select.list(cat_vars, multiple = FALSE, title = "Odaberi kvalitativnu varijablu:")
  grupe <- unique(df[[cvar]])
  
  if (length(grupe) != 2) stop("Varijabla grupe mora imati točno 2 kategorije.")
  
  # Odabir kvantitativnih varijabli
  num_vars <- names(df)[sapply(df, is.numeric)]
  nvars <- select.list(num_vars, multiple = TRUE, title = "Odaberi kvantitativne varijable:")
  
  # Odabir tipa t-testa
  options <- c("Student t-test", "Welch t-test", "Auto (po Levene testu)")
  choice <- select.list(options, title = "Odaberite tip t-testa:")
  
  results <- lapply(nvars, function(var) {
    lt <- car::leveneTest(df[[var]] ~ df[[cvar]], center = median)
    levene_p <- lt$`Pr(>F)`[1]
    
    if (choice == "Student t-test") {
      t <- t.test(df[[var]] ~ df[[cvar]], var.equal = TRUE)
      test_type <- "Student"
    } else if (choice == "Welch t-test") {
      t <- t.test(df[[var]] ~ df[[cvar]], var.equal = FALSE)
      test_type <- "Welch"
    } else { # Auto
      var_equal <- ifelse(levene_p > 0.05, TRUE, FALSE)
      t <- t.test(df[[var]] ~ df[[cvar]], var.equal = var_equal)
      test_type <- ifelse(var_equal, "Student", "Welch")
    }
    
    d <- effsize::cohen.d(df[[var]], df[[cvar]])$estimate
    means <- round(unname(t$estimate), 3)
    
    data.frame(
      VARS = var,
      MEAN_1 = means[1],
      MEAN_2 = means[2],
      SED = round(t$stderr, 3),
      T = abs(round(t$statistic, 3)),
      DF = round(t$parameter, 2),
      P = round(t$p.value, 4),
      CI_95_lower = round(t$conf.int[1], 3),
      CI_95_upper = round(t$conf.int[2], 3),
      Levene_F = round(lt$`F value`[1], 3),
      Levene_P = round(levene_p, 4),
      Cohen_D = round(d, 3),
      Test_Type = test_type,
      stringsAsFactors = FALSE
    )
  })
  
  t_df <- do.call(rbind, results)
  colnames(t_df)[2:3] <- paste0("MEAN-", grupe)
  return(t_df)
}
