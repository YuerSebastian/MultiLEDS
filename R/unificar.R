#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export

unificar <- function(dir=.c,pat=NULL,cols=NULL,renom=NULL,...){
  #####-------------Identificando archivos-------------#####
  archs <- list.files(.c[grep(dir,.c)],full.names = T)
  if(!is.null(pat)) archs <- archs[grep(pat,archs)]
  #####-------------Proceso completo de unificacion-------------#####
  for (i in 1:length(archs)) {
    y <- leer(archs[i],...) #Lee archivo.
    #####-------------Re nombrando-------------#####
    if (!is.null(renom)) {
      y <- reshape::rename(y,renom)
    }
    #####-------------Columnas inexistentes y seleccion-------------#####
    if (!is.null(cols)) {
      for (j in 1:length(cols)) {
        if (!hasName(y,cols[j])) {
          y[cols[j]] <- "_"
        }
      }
      y <- select(y,identity(cols))#Selecciona las columnas.
    }
    if(i==1) x <- y else x <- rbind(x,y) #Unifica
  }
  print(paste(i," Archivos unificados con exito.",sep=""))
  return(x)
}






