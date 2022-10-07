#' @title Reparte equitativamente
#' @description La funcion reparte de una manera lo mas equitativa posible un vector, se puede elegir la columna y que datos se van a repartir y sustituir.
#' No incluye una opcion para repartir una columna completa ya que se puede utilizar directamente rep_len() para esas situaciones.
#' Los datos de reemlazo y los datos a reemplazar deben ser del mismo tipo.
#' @param Base Dataframe, se retorna el mismo con la columna repartida
#' @param x Vector de datos los cuales se van a repartir.
#' @param col Columna del dataframe que se tomara para repartir el vector.
#' @param reem Datos que se van a reemplazar de la columna dada, por defecto es "NA".
#' @examples
#' Base <- repartir(Base,c("A","B","C"),"columna")
#' Base <- repartir(Base,c(1,2,3)"columna2",c(3,4,5))
#' Base <- repartir(Base,c("A","B","C")"columna2",c("A","C","D"))
#' @import utils dplyr
#' @export
repartir <- function(Base,x,col,reem=NA){
  #Conteo de faltantes
  y <- as.data.frame(x) %>% `colnames<-`(col)%>%
    left_join(filter(Base,Base[[col]] %in% x) %>% count(.[[col]]) %>% `colnames<-`(c(col,"n")),by=col)%>%
    mutate(n=replace_na(n,0)) %>% mutate(n=max(n)-n) %>% arrange(desc(n))
  #Llenado completo
  noms <- c()
  for (i in 1:(length(y[[1]])-1)) {
    aux <- rep(y[[col]][i],y[["n"]][i])
    noms <- c(noms,aux)
  }
  #Resta de sobrantes o faltantes
  n <- length(filter(Base,Base[[col]] %in% reem)[[1]]) - length(noms)
  #Completando o restando
  if (n > 0) {
    aux <- rep_len(y[[col]],n)
    noms <- c(noms,aux)
  }else{
    noms <- noms[1:(length(noms)+n)]
  }
  #Asignando y unificando
  y <- filter(Base,Base[[col]] %in% reem)
  Base <- filter(Base,!Base[[col]] %in% reem)

  y[[col]] <- noms
  Base <- rbind(Base,y)
  return(Base)
}








