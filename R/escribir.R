#' @title Escritura simplificada (Incompleto)
#' @description Escribe un archvo .csv o .txt por el momento, tal como la funcion leer() se indica el nombre y el patron a usar en la carpeta indicada en diremail().
#' @param base DataFrame a escribir.
#' @param arch Vector donde se indica Nombre del archivo y extension, para arch[1] y patron de la carpeta en donde se va a escribir en arch[2].
#' @param sep Por defecto, si se imprime un archivo .txt este se separara con tabulaciones, se puede editar para usar cualquier separador.
#' @param ... Parametros propios de las funciones  write.csv() y  write.table().
#' @examples
#' escribir(Base,c("Ejemplo.csv","Carpeta2$"))
#' En este ejemplo se observa como se utiliza
#' un parametro propio de write.table()
#' escribir(Nombre,c("Ejemplo2.txt","Carpeta2$"),"-",eol = "#")
#' @import utils
#' @export
escribir <- function(base,arch=c("",""),sep = "\t",...){
  dir <- paste(.c[grep(arch[2],.c)],arch[1],sep="/")
  x <- stringr::str_extract(arch[1],"\\..*")
  if (x==".csv") {
    write.csv(base,dir,row.names = F,...)
  }
  else if (x==".txt"){
    write.table(base,dir,row.names = F,sep = sep,...)
  }
}
