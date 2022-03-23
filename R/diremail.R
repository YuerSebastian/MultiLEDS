#' @title Especificacion de la carpeta general y el correo de google
#' @description Especifica la carpeta origen a utilizar para todos los archivos que se van a leer y tambien el correo a utilizar
#' para la carpeta del drive personal.
#' @param c Carpeta origen.
#' @param email Correo de google para la carpeta del drive.
#' @examples
#' diremail("D:/Reportes","ejemplo.1@gmail.com")
#' @export
diremail <- function(c="",email=""){
  .c <<- list.dirs(c)
  .email <<- email
  if (.email != ""){
    googledrive::drive_auth(.email)
    googlesheets4::gs4_auth(.email)
  }
}
