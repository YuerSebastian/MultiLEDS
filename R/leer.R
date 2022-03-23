#' @title Lectura de multiples fuentes
#' @description Lee archivos de varios tipos (.xlsx, .csv, .txt y gsheets) , no es necesario especificar la extension ya que este la detecta de forma automatica.
#' Puede leer una direccion completa o un patron de una lista de direcciones indicada en diremail().
#' Por defecto, todos los archivos a leer convierte las columnas a tipo "character", puede haber problemas con fechas en archivos .xlsx para columnas de tipo "Date"
#' debido al mismo motivo, se toma en cuenta para solucionarlo.
#' @param arch Direccion completa o vector donde indica el nombre y la ruta en forma de patron, si se lee un gsheet se indica "gsheet" en arch[2],
#' si se requiere leer por su ID se debe especificar en el mismo "gsheet.ID", para esto es necesario especificar el correo electronico a usar para leer los archivos de tipo gsheet,
#' indicado en diremail()
#' @param delim Opcional. si se lee un archivo .txt en ella se indica el delimitador a usar.
#' @param ... Parametros propios de las distintas funciones read_csv(), read_delim(), read_excel() y range_speedread().
#' @examples
#' -Lectura simple de una ruta completa.
#'
#' leer("D:/archivo.csv")
#'
#' -En este caso se toma como ejemplo un archivo cuya direccion tiene como patron terminar
#' -en la palabra "Carpeta", la direccion completa puede ser "D:/Documentos/Archivos/Carpetas/Carpeta2/archivo_2.xlsx",
#' -no es necesario escribir la extension, se puede observar como se puede usar un parametro propio de otra funcion,
#' -en este caso de read_csv() el parametro col_select
#'
#' leer(c("archivo_2","Carpeta2$"),col_select = c("Matricula","Programa"))
#'
#' -Para leer un gsheet puede ser por su nombre o ID, se pueden usar los parametros propios de
#' -la funcion range_speedread().
#'
#' leer(c("Info General","gsheet"))
#' leer(c("sDfGtHjGfusj56G8hkohejemplo","gsheet.ID"), sheet= "Info")
#' @return Un data frame del archivo leido
#' @export
#' @import dplyr tidyr readr googlesheets4 readxl
#' @encoding LATIN1


leer <- function(arch=c("",""),delim=NULL,...){
  if (length(arch)==1){ #Si es una direccion completa o en este caso, el vector tiene longitud 1...
    x <- stringr::str_extract(arch,"\\..*")
    if (x==".csv") {
      x <- read_csv(arch,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
    }else if (x==".xlsx"){
      x <- read_excel(arch,col_types = "text",...)
    }
  }else{ #Si no, busca la direccion completa por un patron.
    if (!stringr::str_detect(arch[2],"gsheet")) { #Si no se lee una hoja de google...
      dir <- list.files(.c[grep(arch[2],.c)],full.names = T) %>% .[grep(arch[1],.)]
      y <- stringr::str_extract(dir,"\\..*")
      if (y==".csv") {#Si es un csv...
        x <- read_csv(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
      }else if (y==".xlsx"){#Si es un xlsx...
        x <- read_excel(dir,col_types = "text",...)
      }else if (y==".txt"){#Si es un txt...
        x <- read_delim(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),delim = delim...)
      }
    }else{#Si es una hoja de google, lee.
      if (stringr::str_detect(arch[2],".ID")) {#Si se esta leyendo con un ID...
        x <- range_speedread(arch[1],col_types = cols(.default = ".c"),...)
      }else{#Si no, busca por nombre el ID y lo lee, solo funciona para archivos propios no compartidos
        y <- gs4_find(arch[1]); y <- as.character(y$id); x <- range_speedread(y,col_types = cols(.default = "c"),...)
      }
    }
  }
  return(x) #Retorna la base leida.
}








