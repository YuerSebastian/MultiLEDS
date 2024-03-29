% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_archs.R
\name{list_archs}
\alias{list_archs}
\title{Lista de archivos de la ruta especificada}
\usage{
list_archs(pat = "", ...)
}
\arguments{
\item{pat}{Patron en donde se especifica la carpeta a buscar.}
}
\description{
Devuelve una lista de todos los archivos especificados, ya sea en toda la lista de archivos de la carpeta especificada
o puede buscar por un patron.
}
\examples{
-Se especifican todos los archivos que contiene la carpeta origen.

list_archs()

-Se especifican los archivos que se encuentran con el patron descrito.

list_archs("VICI$")
}
