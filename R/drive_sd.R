#' @title Descarga y subida de archivos en Google drive
#' @description Sube o descarga archivos de google drive
#'
#'
#' @export
#' @import googledrive googlesheets4
#'
drive_sd <- function(mod=NULL,arch=c("nom","nom2","dir"),...){
  if (length(arch==2)) {
    nom <- stringr::str_split(arch[1],"/",simplify = T)
    nom <- nom[[length(nom)]]#Para obtener solo el nombre del archivo y no la ruta completa
    dir <- paste(.c[grep(arch[2],.c)],nom,sep="/")
    if (mod=="des") {
      drive_download(arch[1],dir,overwrite = T)
    }
  }
}

