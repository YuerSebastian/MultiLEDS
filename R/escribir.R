#' @title Escritura simplificada (Incompleto)
#' @description Escribe un archvo .csv o .txt por el momento, tal como la funcion leer() se indica el nombre y el patron a usar en la carpeta indicada en diremail().
#' @param base DataFrame a escribir.
#' @param arch Vector donde se indica Nombre del archivo y extension, para arch[1] y patron de la carpeta en donde se va a escribir en arch[2].
#' @param sep Por defecto, si se imprime un archivo .txt este se separara con tabulaciones, se puede editar para usar cualquier separador.
#' @param cod Elige la codificacion para escribir, por defecto "LATIN1", no aplica al escribir en un gsheet
#' @param ... Parametros propios de las funciones  write.csv() y  write.table().
#' @examples
#' escribir(Base,c("Ejemplo.csv","Carpeta2$"))
#' En este ejemplo se observa como se utiliza
#' un parametro propio de write.table()
#' escribir(Nombre,c("Ejemplo2.txt","Carpeta2$"),"-",eol = "#")
#' @import utils googlesheets4
#' @export
escribir <- function(base,arch=c("",""),sep = "\t",cod = "LATIN1",...){
  if(length(arch)==1){#Parte de una direccion completa
    x <- stringr::str_extract(arch,"\\..*")
    if (x==".csv") {
      write.csv(base,arch,row.names = F,fileEncoding = cod,...)
    }else if (x==".txt"){
      write.table(base,dir,row.names = F,sep = sep,fileEncoding = cod,...)
    }
  }else{#Parte de un patron
    if(!stringr::str_detect(arch[2],"gsheet")){#Si no se escribe en hoja de google.
      dir <- paste(.c[grep(arch[2],.c)],arch[1],sep="/")
      x <- stringr::str_extract(arch[1],"\\..*")
      if (x==".csv") {
        write.csv(base,dir,row.names = F,fileEncoding = cod,...)
      }
      else if (x==".txt"){
        write.table(base,dir,row.names = F,sep = sep,fileEncoding = cod,...)
      }
    }else{#Si es una hoja de google.
      if (stringr::str_detect(arch[2],".ID")) {#Si se va a escribir con un ID...
        x <- stringr::str_remove(stringr::str_remove(stringr::str_remove(arch[2],"_"),".ID"),"gsheet") #Selecciona nombre de hoja, despues de "_" y antes de ".ID"
        range_clear(arch[1],x,reformat = F,...); sheet_resize(arch[1],x,2,2,T)
        range_write(arch[1],base,x,reformat = F,...); range_autofit(arch[1],x,...)
      }else{#Si no, busca por nombre
        x <- stringr::str_remove(stringr::str_remove(arch[2],"_"),"gsheet") #Selecciona nombre de hoja, despues de "_"
        y <- gs4_find(arch[1]); y <- as.character(y$id) #Encuentra ID de hoja
        range_clear(y,x,reformat = F,...); sheet_resize(y,x,2,2,T)
        range_write(y,base,x,reformat = F,...); range_autofit(y,x,...)
      }
    }
  }
}







