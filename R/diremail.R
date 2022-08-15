#' @title Especificacion de la carpeta general, correo de google y conexion a MySQL
#' @description Especifica la carpeta origen a utilizar para todos los archivos que se van a leer, el correo a utilizar
#' para la carpeta del drive personal y especificaciones para MySQL
#' @param c Carpeta origen.
#' @param email Correo de google para la carpeta del drive.
#' @param msql Conexion a MySQL, vector con orden c("host","usuario","contrasenia","base de datos")
#' @examples
#' diremail("D:/Reportes","ejemplo.1@gmail.com",c("10.10.10.100","usuario","contra","base_de_datos"))
#' @export
diremail <- function(c="",email="",msql=""){
  if(c!="") .c <<- list.dirs(c)
  if(length(msql)!=1) .msql <<- msql
  if(email!=""){
    .email <<- email
    googledrive::drive_auth(.email)
    googlesheets4::gs4_auth(.email)
  }
}
