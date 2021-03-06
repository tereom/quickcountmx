% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nal_2012.R
\docType{data}
\name{nal_2012}
\alias{nal_2012}
\title{Presidential electoral results 2012.}
\format{A data frame with 6746 rows and 21 variables:
\describe{
  \item{casilla_id}{Numeric identifier of the polling station.}
  \item{distrito_fed_17, distrito_fed_12}{Federal district, the districts
  where redefined in 2017, we include both variables.}
  \item{distrito_loc_17, distrito_loc_12}{Local district, the districts
  where redefined in 2017, we include both variables.}
  \item{are}{Electoral Responsability Area identifier for the 2012
  election.}
  \item{seccion}{The section is the miminal electoral geographical unit that
  contains the polling station.}
  \item{casilla}{Type of polling station, Basic (B) is the
  basic and most common type, Contigua (C) these polling stations arise when
  the nominal list surpases 750 people, Extraoridinaria (E) these are
  installed when people can not reach their planned station and Especial (S)
  polling stations are where people can vote outside their asigned polling
  station}
  \item{tipo_seccion}{Section type (in 2017), whether the section of the
  station is rural (R), urban (U) or mixed (M).}
  \item{rural}{Binary variable indicating if the polling station is on a
  rural section.}
  \item{region}{Numeric variable indicating the state corresponding to the
  polling station.}
  \item{pri_pvem}{Number of votes favoring the parties PRI and/or PVEM.}
  \item{pan_na}{Number of votes favoring the parties PAN and/or Partido
  Nueva Alianza.}
  \item{prd}{Number of votes favoring the party PRD.}
  \item{mc}{Number of votes favoring the party Movimiento Ciudadano.}
  \item{otros}{Number of votes that do not favor any of the parties (null,
  non-registered candidates).}
  \item{total}{Total number of votes registered.}
  \item{ln}{Nominal list, number of people registered to vote.}
  \item{tamano_md}{Binary variable indicating if the popultaion of potential
  voters (size of the nominal list) in the section corresponding to the
  polling station is medium sized (greater than 1000 and less than 5000).}
  \item{tamano_gd}{Binary variable indicating if the popultaion of potential
  voters (size of the nominal list) in the section corresponding to the
  polling station is greater than 5000).}
  \item{casilla_ex}{Binary variable indicating if the polling station is of
  type Extraordinaria}
}}
\source{
\url{https://cartografia.ife.org.mx}
}
\usage{
nal_2012
}
\description{
A dataset containing the election results for president 2012, each row
corresponds to a polling station.
}
\keyword{datasets}
