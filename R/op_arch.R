#' @title Operaciones con archivos
#' @export
op_arch <- function(op=NULL,arch=c("","","",""),...){
  if (!is.null(op)) {
    #####-----------Identificando archivo-----------#####
    if (length(arch)==4) {
      if(op=="mov"|op=="cop"){
        ant <- list.files(.c[grep(arch[3],.c)],full.names = T) %>% .[grep(paste("/",arch[1],"\\.",sep = ""),.)]; ext <- stringr::str_extract(ant,"\\..*")
        nue <- paste(.c[grep(arch[4],.c)],"/",arch[2],ext,sep="")
      }
    }else{
      if(op=="ren") dir <- arch[3] else if(op=="mov"|op=="cop"|op=="eli") dir <- arch[2]
      ant <- list.files(.c[grep(dir,.c)],full.names = T) %>% .[grep(paste("/",arch[1],"\\.",sep = ""),.)]; ext <- stringr::str_extract(ant,"\\..*")
    }
    #####-----------Operacion-----------#####
    if (op=="ren") {
      nue <- paste(.c[grep(arch[3],.c)],"/",arch[2],ext,sep="")
      file.rename(ant,nue)
      print(paste("Archivo: ",ant,sep = ""))
      print(paste("Renombrado a: ",nue,sep=""))
      print("")
    }
    else if (op=="mov") {
      if(length(arch)<4) nue <- paste(.c[grep(arch[3],.c)],"/",arch[1],ext,sep="")
      file.rename(ant,nue)
      print(paste("Archivo: ",ant,sep = ""))
      print(paste("Movido a: ",nue,sep=""))
      print("")
    }
    else if (op=="cop") {
      if(length(arch)<4) nue <- paste(.c[grep(arch[3],.c)],"/",arch[1],ext,sep="")
      file.copy(ant,nue,overwrite = T,copy.date = T,...)
      print(paste("Archivo: ",ant,sep = ""))
      print(paste("Copiado o sobreescrito a: ",nue,sep=""))
      print("")
    }
    else if (op=="eli") {
      file.remove(ant)
      print(paste("Archivo: ",ant,sep = ""))
      print("Eliminado")
      print("")
    }
  }else if(op!="ren" & op!="mov" & op!="cop" & op!="eli"){print("Operacion incorrecta.")
  }else{print("No se especifica operacion a realizar con el archivo.")}
}



