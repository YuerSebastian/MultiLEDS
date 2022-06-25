#' @title Carrete
#' @description Modifica un vector de manera en que parezca un "carrete", es decir, de a cuerdo a las repeticiones
#' que se indiquen, identifica en que posicion del vector se quedo exactamente una replicacion de la funcion "rep_len()",
#' el vector se modifica iniciando desde el ultimo valor de replicacion y termina con el mismo numero de valores original, dando como
#' resultado un nuevo vector del mismo tamaño en diferente orden.
#' @param x vector el cual se hará el calculo de replicaciones.
#' @param rep numreo de replicaciones
#' @param sig por defecto es "True", ésto quiere decir que el nuevo vector emppezará desde el siguiente valor de a cuerdo al último de replicacion,
#' si se modifica a "False", el vector empieza desde el valor ultimo exacto.
#' @examples
#' x <- c("a","b","c","d")
#' carrete(x,6)
#' - La salida en este caso seria "("c","d","a","b")", ya que las repeticiones son 6, seguiria una sucesion como la siguiente:
#' "("a","b","c","d","a","b")", debido a que el parametro "sig" es "True" por defecto, inicia desde la siguiente posicion, o sea el valor "c" de a cuerdo
#' a la sucesion quedando como resultado el vector indicado.
#'
#' x <- c("1","2","3","4")
#' carrete(x,3,F)
#' -En este caso, el ejemplo es el mismo que el anterior, pero la salida seria "("3","4","1","2")", la digerencia esta en el parametro "sig", debido a que es
#' "False", el vector empieza desde el valor exacto del ultimo valor de replicacion.
#'
#' @export
#'
carrete <- function(x,rep,sig=T){
  y <- rep_len(x,rep) %>% .[length(.)]
  if(sig) x <- x[c(grep(y,x):length(x),1:grep(y,x))] %>% .[2:length(.)] else x <- x[c(grep(y,x):length(x),1:grep(y,x))] %>% .[1:length(.)-1]
}
