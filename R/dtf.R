#' Prikaz data frame-a u DT tablici sa rownames = FALSE
#'
#' @param x Data frame koji se prikazuje.
#' @return Objekt klase `datatable`.
#' @examples
#' dtf(mtcars)
#' @export
dtf <- function(x){
  DT::datatable(x,
                rownames = FALSE,
                class = "row-border",
                filter = "none",
                extensions = c("Buttons"),
                options = list(pageLength = nrow(x), 
                               dom = 'Bt', 
                               buttons = "copy",
                               scrollX = TRUE)
  )
}
