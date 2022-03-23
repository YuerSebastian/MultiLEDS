#' @title Especificacion de la carpeta general y el correo de google
#'
#'
#' @export
diremail <- function(c="",email=""){
  .c <<- list.dirs(c)
  .email <<- email
  if (.email != ""){
    googledrive::drive_auth(.email)
    googlesheets4::gs4_auth(.email)
  }
}
