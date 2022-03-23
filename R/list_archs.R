#' @title Lista de archivos de la ruta especificada
#'
#'
#' @export
list_archs <- function(pat="",...){
  if (pat=="") {
    x <- list.files(.c,...)
  }else{
    x <- list.files(.c[grep(pat,.c)],...)
  }
  return(x)
}
