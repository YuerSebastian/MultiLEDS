#' @title Lista de conexiones
#' @description Regresa la lista de conexiones actuales a leer.
#' @examples
#' list_cons()
#' @export
list_cons <- function(){
  if(exists(".c")) c <- .c else c <- ""
  if(exists(".email")) email <- .email else email <- ""
  if(exists(".msql")) msql <- c("Host"=.msql[1],"Usuario"=.msql[2],"Password"=.msql[3],"Base de datos"=.msql[4]) else msql <- ""
  x <- list("Carpeta general"=c,"Drive email"=email,"Conexion MySQL"=msql)
  return(x)
}
