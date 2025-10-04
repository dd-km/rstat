#' Normalna distribucija (interaktivni unos)
#'
#' Funkcija prikazuje graf normalne distribucije uz zadane parametre
#' aritmetičke sredine (μ), standardne devijacije (σ) i vrijednosti x.
#' Parametri se unose interaktivno putem konzole.
#'
#' Površina do vrijednosti x je osenčena, a na grafu se prikazuju
#' vertikalne linije za μ i x. U konzoli se ispisuju vjerojatnosti
#' P(X ≤ x) i P(X ≥ x).
#'
#' @details
#' Funkcija koristi paket \code{plotly}. Ako paket nije instaliran, funkcija će ga automatski instalirati i učitati.
#' 
#' @return Vraća \code{plotly} graf normalne distribucije sa osenčenim područjem
#' do unesene vrijednosti x.
#'
#' @examples
#' \dontrun{
#' normal_distribution()
#' }
#'
#' @export
normal_distribution <- function() {
  if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
  library(plotly)

  # Interaktivni unos
  mu <- as.numeric(readline("Unesi aritmetičku sredinu: "))
  st_dev <- as.numeric(readline("Unesi standardnu devijaciju: "))
  x_val <- as.numeric(readline("Unesi vrijednost x: "))

  # Podaci za raspodjelu
  x <- seq(mu - 4 * st_dev, mu + 4 * st_dev, 0.01)
  y <- dnorm(x, mean = mu, sd = st_dev)

  # Površina do x_val
  x_fill <- seq(mu - 4 * st_dev, x_val, 0.01)
  y_fill <- dnorm(x_fill, mean = mu, sd = st_dev)

  # Vjerojatnosti
  prob <- pnorm(x_val, mean = mu, sd = st_dev)

  # Oznake μ ± σ
  sigma_lines <- mu + st_dev * c(-3, -2, -1, 0, 1, 2, 3)

  # Kreiranje grafa
  graf <- plotly::plot_ly(x = ~x, y = ~y, type = "scatter", mode = "lines",
                          name = paste0("N(", mu, ", ", st_dev, ")")) %>%
    plotly::add_polygons(x = c(x_fill, rev(x_fill)),
                         y = c(y_fill, rep(0, length(y_fill))),
                         fillcolor = "lightblue", opacity = 0.4,
                         line = list(width = 0),
                         name = paste("P(X ≤", x_val, ") =", round(prob, 4))) %>%
    plotly::add_lines(x = c(mu, mu), y = c(0, max(y)),
                      line = list(color = "red", dash = "dash"),
                      name = "μ") %>%
    plotly::add_lines(x = c(x_val, x_val), y = c(0, dnorm(x_val, mean = mu, sd = st_dev)),
                      line = list(color = "green", dash = "dot"),
                      name = paste("x =", x_val)) %>%
    plotly::layout(
      title = "Normalna distribucija",
      xaxis = list(title = "", tickvals = sigma_lines),
      yaxis = list(title = "f(x)"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)
    )

  # Ispis u konzoli
  cat("____________________________________\n")
  cat("P(x ≤", x_val,") =", round(prob, 4), "\n")
  cat("P(x ≥", x_val, ") =", round(1 - prob, 4), "\n")

  return(graf)
}
