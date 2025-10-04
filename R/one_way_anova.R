#' Jednofaktorska ANOVA ili Welch ANOVA s deskriptivnom statistikom
#'
#' Funkcija omogućava izvođenje jednofaktorske ANOVE ili Welch ANOVE na više
#' zavisnih varijabli uz odabir grupne varijable. Osim testiranja hipoteza,
#' vraća i deskriptivne statistike te testove pretpostavki (Levene i Shapiro).
#'
#' @param df Data frame koji sadrži kvalitativne i kvantitativne varijable.
#'
#' @details
#' - Korisnik interaktivno bira:
#'   * grupnu varijablu (faktor ili karakter),
#'   * zavisne varijable (numeričke),
#'   * tip testa (ANOVA ili Welch ANOVA).
#'
#' - Za svaku zavisnu varijablu računa se:
#'   * Deskriptivna statistika (N, MEAN, SD, SE),
#'   * Rezultati ANOVA/Welch testa (F, df1, df2, p),
#'   * Levene test homogenosti varijanci,
#'   * Shapiro-Wilk test normalnosti reziduala.
#' 
#' Funkcija koristi paket \code{car}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' 
#' 
#' @return Lista s tri elementa:
#' \describe{
#'   \item{\code{dtable}}{Deskriptivna statistika po grupama.}
#'   \item{\code{atable}}{Rezultati ANOVA/Welch testa i testova pretpostavki.}
#'   \item{\code{atest}}{Vrsta odabranog testa ("ANOVA" ili "Welch ANOVA").}
#' }
#'
#' @importFrom car leveneTest
#' @export
#'
#' @examples
#' \dontrun{
#'   # Primjer s ugrađenim datasetom iris
#'   result <- one_way_anova(iris)
#'   result$dtable   # Deskriptive
#'   result$atable   # ANOVA rezultati
#' }
one_way_anova <- function(df) {
  if(!requireNamespace("car", quietly = TRUE)) install.packages("car")
  library(car)
  
  # Odabir varijabli
  qual_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  quant_vars <- names(df)[sapply(df, is.numeric)]
  group_var <- select.list(qual_vars, title = "Odaberi grupnu varijablu")
  dep_vars  <- select.list(quant_vars, title = "Odaberi zavisne varijable", multiple = TRUE)
  test_type <- select.list(c("ANOVA", "Welch ANOVA"), title = "Odaberi vrstu testa:")
  
  # Funkcija za deskriptivu
  deskriptiva_fun <- function(x) {
    n <- length(x)
    m <- mean(x)
    sdv <- sd(x)
    se <- sdv / sqrt(n)
    return(c(N = n, MEAN = round(m, 2), SD = round(sdv, 2), SE = round(se, 2)))
  }
  
  # Deskriptivne statistike
  des <- do.call(rbind, lapply(dep_vars, function(var) {
    m <- tapply(df[[var]], df[[group_var]], deskriptiva_fun)
    mat <- do.call(rbind, m)
    data.frame(
      Varijabla = var,
      Grupa = rownames(mat),
      mat,
      row.names = NULL
    )
  }))
  
  welch <- ifelse(test_type == "Welch ANOVA", TRUE, FALSE)
  
  # Funkcija za ANOVA/Welch rezultate
  anova_results <- function(data, group, dep, welch = FALSE) {
    fmla <- as.formula(paste(dep, "~", group))
    
    if (welch) {
      aov_res <- oneway.test(fmla, data = data, var.equal = FALSE)
      F_val <- unname(aov_res$statistic)
      df1   <- unname(aov_res$parameter[1])
      df2   <- unname(aov_res$parameter[2])
      p_val <- aov_res$p.value
    } else {
      model <- aov(fmla, data = data)
      smry  <- summary(model)[[1]]
      F_val <- smry[["F value"]][1]
      df1   <- smry[["Df"]][1]
      df2   <- smry[["Df"]][2]
      p_val <- smry[["Pr(>F)"]][1]
    }
    
    # Levene test
    lev <- car::leveneTest(fmla, data = data, center = mean)
    Levene_F <- lev$`F value`[1]
    Levene_p <- lev$`Pr(>F)`[1]
    
    # Shapiro test na rezidualima (samo ako nije Welch)
    model <- aov(fmla, data = data)
    sh <- shapiro.test(residuals(model))
    
  data.frame(
      Variable   = dep,
      F          = round(F_val, 3),
      df1        = df1,
      df2        = round(df2, 3),
      p          = round(p_val, 4),
      Levene_F   = round(Levene_F, 3),
      Levene_p   = round(Levene_p, 4),
      Shapiro_W  = round(sh$statistic, 3),
      Shapiro_p  = round(sh$p.value, 4),
      stringsAsFactors = FALSE
    )
  }
  
  # Rezultati za sve odabrane varijable
  res <- do.call(rbind, lapply(dep_vars, function(v) {
    anova_results(df, group_var, v, welch = welch)
  }))
  
  return(list(
    dtable = des,
    atable = res,
    atest  = test_type
  ))
}
