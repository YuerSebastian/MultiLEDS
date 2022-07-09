#' @title Unificacion de varios archivos a la vez
#' @description Unifica archivos completos en un solo DataFrame, utiliza la misma funcion leer() para transformar los archivos.
#' @param dir Ruta de la carpeta en donde contiene todos los archivos a unificar, puede utilizar un patron especificando un vector c("dir","pat")
#' para especificar archivos exactos.
#' @param cols Vector de columnas a seleccionar en el DataFrame final, es muy util en archivos de tipo .xlsx ya que no contiene un parametro especifico
#' para seleccionar columnas, tambien es util cuando se requiere cambiar primero los nombres que no coinciden en todos los archivos a unificar, si una
#' columna dentro del archivo no existe, crea una columna con un solo caracter especificado en col_vac
#' @param renom Renombra las columnas al nombre que se necesiten
#' @param col_vac Caracter utilizado para especificar como se debe llenar la columna vacia, en caso de que no exista en el archivo cuando se selecciona
#' @param ... Parametros propios de las funciones que utiliza leer()
#' @examples
#' -Aqui no se especifica ningun parametro mas que dir y pat, en donde unifica todos los archivos
#' -que contengan el patron "Actas Docente"
#'
#' unificar("SIR$","Actas Docente")
#'
#' -En este caso complejo se observa que no se selecciona ningun patron, toma todos los archivos
#' -que contenga la carpeta "FLOKZU", internamente primero lee el archivo, despues renombra
#' -las columnas que se requieran especificadas en "renom" si es que existen, despues selecciona
#' -las columnas especificadas en "cols", si no existe alguna columna, la crea por cada archivo
#' -y ya que no se especifica "col_vac", se llenara la columna con un "_", y por ultimo se
#' -especifica en un parametro propio de "read_excel()" las filas a omitir ya que todos
#' -los archivos son ".xlsx", se omite 1 fila.
#'
#' Base <- unificar("FLOKZU$",
#' cols = c("Identificador","Matriicula","Correo","Fecha Inicio","Finalizado","Fecha Finalizacioon","Tarea Actual","Fecha Tarea","Analista SER"),
#' renom = c("SIU"="Matriicula",
#'           "Matricula"="Matriicula",
#'           "Correo del estudiante"="Correo",
#'           "Correo del prospecto"="Correo",
#'           "Fecha asignacioon Tarea actual"="Fecha Tarea",
#'           "A.analista SER"="Analista SER"),
#'           skip = 1)
#' @return Un DataFrame con todos los archivos unificados.
#' @import dplyr tidyr readr readxl
#' @export

unificar <- function(dir=c(.c,""),renom=NULL,cols=NULL,col_vac="_",iden="no",iden_esp=NULL,...){
  #####-------------Identificando archivos-------------#####
  niden <- iden
  if (length(dir) == 1) {
    archs <- list.files(.c[grep(dir, .c)], full.names = T)
  } else{
    archs <- list.files(.c[grep(dir[1], .c)], full.names = T); archs <- archs[grep(dir[2], archs)]
  }
  #####-------------Identificador de archivo-------------#####
  if(niden!="no"){
    if(length(dir)==1){
      if(niden=="nom"){
        id <- list.files(.c[grep(dir, .c)])
      }
      else if(niden=="dir"){
        id <- list.files(.c[grep(dir, .c)], full.names = T)
      }
      else if(niden=="num"){
        id <- list.files(.c[grep(dir, .c)]) %>% row_number()
      }
      else if(niden=="esp"){
        id <- niden
      }
    }else{
      if(niden=="nom"){
        id <- list.files(.c[grep(dir[1], .c)]); id <- id[grep(dir[2], id)]
      }
      else if(niden=="dir"){
        id <- list.files(.c[grep(dir[1], .c)], full.names = T); id <- id[grep(dir[2], id)]
      }
      else if(niden=="num"){
        id <- list.files(.c[grep(dir[1], .c)]); id <- id[grep(dir[2], id)] %>% row_number()
      }
      else if(niden=="esp"){
        if (!is.null(iden_esp)){
          id <- iden_esp
        }else{
          print("No se escribe identificador especifico, no se genera columna...")
          niden <- "no"
        }
      }
    }
  }else{
    iden <- "no"
  }
  #####-------------Proceso completo de unificacion-------------#####
  for (i in 1:length(archs)) {
    y <- leer(archs[i],...) #Lee archivo.
    if (niden!="no") y <- mutate(y,iden=id[i])
    #####-------------Re nombrando-------------#####
    if (!is.null(renom)) {
      y <- reshape::rename(y,renom)
    }
    #####-------------Columnas inexistentes y seleccion-------------#####
    if (!is.null(cols)) {
      for (j in 1:length(cols)) {
        if (!hasName(y,cols[j])) {
          y[cols[j]] <- col_vac
        }
      }
      y <- select(y,identity(cols))#Selecciona las columnas.
    }
    if(i==1) x <- y else x <- rbind(x,y) #Unifica
  }
  print(paste(i," Archivos unificados con exito.",sep=""))
  return(x)
}






