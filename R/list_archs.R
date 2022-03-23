#' @title Lista de archivos de la ruta especificada
#' @description Devuelve una lista de todos los archivos especificados, ya sea en toda la lista de archivos de la carpeta especificada
#' o puede buscar por un patron.
#' @param pat Patron en donde se especifica la carpeta a buscar.
#' @examples
#' -Se especifican todos los archivos que contiene la carpeta origen.
#'
#' list_archs()
#'
#' -Se especifican los archivos que se encuentran con el patron descrito.
#'
#' list_archs("VICI$")
#' @export
list_archs <- function(pat="",...){
  if (pat=="") {
    x <- list.files(.c,...)
  }else{
    x <- list.files(.c[grep(pat,.c)],...)
  }
  return(x)
}
