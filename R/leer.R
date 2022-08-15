#' @title Lectura de multiples fuentes
#' @description Lee archivos de varios tipos (.xlsx, .csv, .txt y gsheets) , no es necesario especificar la extension ya que este la detecta de forma automatica.
#' Puede leer una direccion completa o un patron de una lista de direcciones indicada en diremail().
#' Por defecto, todos los archivos a leer convierte las columnas a tipo "character", puede haber problemas con fechas en archivos .xlsx para columnas de tipo "Date"
#' debido al mismo motivo, se toma en cuenta para solucionarlo.
#' @param arch Direccion completa o vector donde indica el nombre y la ruta en forma de patron, si se lee un gsheet se indica "gsheet" en arch[2],
#' si se requiere leer por su ID se debe especificar en el mismo "gsheet.ID", para esto es necesario especificar el correo electronico a usar para leer los archivos de tipo gsheet,
#' indicado en diremail().
#' Puede realizar una consulta de MySQL especificando "msql" en arch[2] y una consulta en arch[1] retornando un dataframe con la consulta especificada.
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
#' @import dplyr tidyr readr googlesheets4 readxl RMySQL
#' @encoding LATIN1
leer <- function(arch=c("",""),secc=NULL,...){
  if (length(arch)==1){ #Si es una direccion completa o en este caso, el vector tiene longitud 1...
    x <- stringr::str_extract(arch,"\\..*")
    if (x==".csv") {
      x <- read_csv(arch,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
    }else if (x==".xlsx" | x==".xlsm" | x==".xls"){
      x <- read_excel(arch,col_types="text",...)
    }else if (x==".txt"){
      x <- read_delim(arch,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
    }
  }else{ #Si no, busca la direccion completa por un patron.
    if (!stringr::str_detect(arch[2],"gsheet") & !stringr::str_detect(arch[2],"msql")) {# Si no se lee gsheet ni msql...
      dir <- list.files(.c[grep(arch[2],.c)],full.names = T) %>% .[grep(paste("/",arch[1],"\\.",sep = ""),.)]
      y <- stringr::str_extract(dir,"\\..*")
      if (y==".csv") {#Si es un csv...
        x <- read_csv(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
      }else if (y==".xlsx" | y==".xlsm" | y==".xls"){#Si es un xlsx...
        x <- read_excel(dir,col_types="text",...)
        if (!is.null(secc)) {x <- extr_secc(x,secc)}
      }else if (y==".txt"){#Si es un txt...
        x <- read_delim(dir,locale = locale(encoding = "LATIN1"),col_types = cols(.default = "c"),...)
      }
    }else{#Si se quiere leer gsheet o msql
      if (stringr::str_detect(arch[2],"gsheet")) {#Si se lee gsheet...
        if (stringr::str_detect(arch[2],".ID")) {#Si se esta leyendo con un ID...
          x <- range_speedread(arch[1],col_types = cols(.default = "c"),...)
          if (!is.null(secc)) {x <- extr_secc(x,secc)}
        }else{#Si no, busca por nombre el ID y lo lee, solo funciona para archivos propios no compartidos
          y <- gs4_find(arch[1]); y <- as.character(y$id); x <- range_speedread(y,col_types = cols(.default = "c"),...)
          if (!is.null(secc)) {x <- extr_secc(x,secc)}
        }
      }else if (stringr::str_detect(arch[2],"msql")){#Si no, hace una consulta de msql
        con <- dbConnect(MySQL(),user=.msql[2],host=.msql[1],password=.msql[3],dbname=.msql[4])
        x <- dbGetQuery(con,arch[1])
        dbDisconnect(con)
      }
    }
  }
  return(x) #Retorna la base leida.
}








