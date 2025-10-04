#' Prikaz data frame-a u DT tablici sa rownames = TRUE
#'
#' @param x Data frame koji se prikazuje.
#' @return Objekt klase `datatable`.
#' @examples
#' dtt(mtcars)
#' @export
dtt <- function(x){
  DT::datatable(x,
                rownames = TRUE,
                class = "row-border",
                filter = "none",
                extensions = c("Buttons"),
                options = list(pageLength = nrow(x), 
                               dom = 'Bt', 
                               buttons = "copy",
                               scrollX = TRUE)
  )
}