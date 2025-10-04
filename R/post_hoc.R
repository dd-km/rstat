#' Post-hoc analiza nakon ANOVA
#'
#' Interaktivno bira faktor, numeričku varijablu i metodu korekcije.
#' Vraća tablicu parnih usporedaba s razlikama srednjih vrijednosti, SE, t i p-vrijednostima.
#'
#' @details
#' Funkcija koristi paket \code{emmeans}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' 
#' @param df Data frame koji sadrži barem jednu faktorsku i jednu numeričku varijablu.
#' @return Lista s dva elementa:
#' \itemize{
#'   \item{phtable}{Data frame s post-hoc rezultatima}
#'   \item{phtest}{Korištena metoda za višestruke usporedbe (multiple comparisons)}
#' }
#' @examples
#' \dontrun{
#' result <- post_hoc(df)
#' print(result$phtable)
#' }
#' @importFrom emmeans emmeans pairs
#' @export
post_hoc <- function(df) {
if(!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
  library(emmeans)
  
  # Odabir varijabli
  qual_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  quant_vars <- names(df)[sapply(df, is.numeric)]
  group_var <- select.list(qual_vars, title = "Odaberi grupnu varijablu")
  dep_var  <- select.list(quant_vars, title = "Odaberi zavisne varijable")
  
  # Odabir metode za multiple comparisons
  adjust_method <- select.list(
    c("bonferroni","tukey","holm","scheffe","none"), 
    title = "Odaberi metodu"
  )
  
  # Formula i ANOVA
  form <- as.formula(paste(dep_var, "~", group_var))
  res_aov <- aov(form, data = df)
  
  # Post-hoc analiza
  phh <- emmeans(res_aov, specs = group_var)
  ph_pairs <- pairs(phh, adjust = adjust_method)
  ph_df <- as.data.frame(ph_pairs)
 
  # Preimenovanje stupaca (base R)
  names(ph_df)[names(ph_df) == "contrast"] <- "Groups"
  names(ph_df)[names(ph_df) == "estimate"] <- "Mean difference"
  names(ph_df)[names(ph_df) == "t.ratio"] <- "t"
  names(ph_df)[names(ph_df) == "p.value"] <- "p"
  
  # Zaokruživanje decimala (base R)
  ph_df$`Mean difference` <- round(ph_df$`Mean difference`, 3)
  ph_df$SE <- round(ph_df$SE, 3)
  ph_df$t <- round(ph_df$t, 3)
  ph_df$p <- round(ph_df$p, 4)
  
  return(list(
    phtable = ph_df,
    phtest  = adjust_method
  ))
}