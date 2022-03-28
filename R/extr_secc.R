#' @title Extraccion de secciones
#' @description Extrae secciones completas (si se requiere) de archivos gsheet o excel.
#' @export
extr_secc <- function(x,secc){
  #Extraccion de la seccion
  var <- which(names(x)==secc)
  for (i in var:length(names(x))) {
    if (grepl("\\...",names(x)[i+1])) var <- c(var,i+1) else break
  }
  x <- x[var] %>% `colnames<-`(.[1,]) %>% .[-1,]
  #Columna con menos NA
  var <- colSums(!is.na(x))
  for (i in 1:length(var)) {
    if(var[i]==max(var)){
      col <- names(var[i])
      break
    }
  }
  x <- filter(x,!is.na(x[col]))
  return(x)
}
