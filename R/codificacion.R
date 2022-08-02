#' @title Codificacion
#' @description Codifica un dataframe de una codificacion origen a otra.
#' Por defecto convierte de UTF-8 a ISO8859-1, o mas conocido como LATIN1, pero se puede especificar otra.
#' @param x Dataframe a convertir.
#' @param desde Codificacion origen.
#' @param hacie Codificacion destino.
#' @examples
#' diremail("D:/Ejemplos")
#' x <- leer(c("archivo","carpeta$"))
#' x <- codificacion(x)
#' -En este punto, si el archivo de origen tiene la codifiacion UTF-8, se convertira a LATIN1, de a cuerdo al alfabeto latino.
codificacion <- function(x,desde="UTF-8",hacia="ISO8859-1"){
  x <- mutate_all(x,~iconv(.,desde,hacia)) %>% `colnames<-`(.,iconv(names(.),desde,hacia))
  return(x)
}
