#' Presidential electoral results 2012.
#'
#' A dataset containing the election results for president 2012, each row
#' corresponds to a polling station.
#'
#' @format A data frame with 143132 rows and 23 variables:
#' \describe{
#'   \item{casilla_id}{Numeric identifier of the polling station.}
#'   \item{distrito_fed_17, distrito_fed_12}{Federal district, the districts
#'   where redefined in 2017, we include both variables.}
#'   \item{distrito_loc_17, distrito_loc_12}{Local district, the districts
#'   where redefined in 2017, we include both variables.}
#'   \item{are}{Electoral Responsability Area identifier for the 2012
#'   election.}
#'   \item{seccion}{The section is the miminal electoral geographical unit that
#'   contains the polling station.}
#'   \item{casilla}{Type of polling station, Basic (B) is the
#'   basic and most common type, Contigua (C) these polling stations arise when
#'   the nominal list surpases 750 people, Extraoridinaria (E) these are
#'   installed when people can not reach their planned station and Especial (S)
#'   polling stations are where people can vote outside their asigned polling
#'   station}
#'   \item{tipo_seccion}{Section type (in 2017), whether the section of the
#'   station is rural (R), urban (U) or mixed (M).}
#'   \item{rural}{Binary variable indicating if the polling station is on a
#'   rural section.}
#'   \item{region}{Numeric variable indicating the state corresponding to the
#'   polling station.}
#'   \item{pri_pvem}{Number of votes favoring the parties PRI and/or PVEM.}
#'   \item{pan_na}{Number of votes favoring the parties PAN and/or Partido
#'   Nueva Alianza.}
#'   \item{prd}{Number of votes favoring the party PRD.}
#'   \item{mc}{Number of votes favoring the party Movimiento Ciudadano.}
#'   \item{otros}{Number of votes that do not favor any of the parties (null,
#'   non-registered candidates).}
#'   \item{total}{Total number of votes registered.}
#'   \item{ln}{Nominal list, number of people registered to vote.}
#'   \item{tamano_md}{Binary variable indicating if the popultaion of potential
#'   voters (size of the nominal list) in the section corresponding to the
#'   polling station is medium sized (greater than 1000 and less than 5000).}
#'   \item{tamano_gd}{Binary variable indicating if the popultaion of potential
#'   voters (size of the nominal list) in the section corresponding to the
#'   polling station is greater than 5000).}
#'   \item{casilla_ex}{Binary variable indicating if the polling station is of
#'   type Extraordinaria}
#' }
#' @source \url{https://cartografia.ife.org.mx}
"nal_2012"
#
# library(tidyverse)
# nal_estratos <- readr::read_delim(fs::path_join(c("~/Documents/GitHub/",
#     "ine_cotecora/datos_procesados/Presidente2012_20180430/",
#     "Presidente2012_completo.csv")), ",", escape_double = FALSE, trim_ws = TRUE,
#     locale = readr::locale(encoding = "ISO-8859-1"))
# nal_2012 <- nal_estratos %>%
#     dplyr::mutate(
#         casilla_id = 1:n(),
#         edo_id = iD_ESTADO,
#         distrito_fed_17 = DISTRITO_FEDERAL_2017,
#         distrito_fed_12 = DISTRITO_FEDERAL_2012,
#         distrito_loc_17 = Distrito_Local_2017,
#         are = stringr::str_c(iD_ESTADO, distrito_fed_12,
#             ID_AREA_RESPONSABILIDAD_2E__2012, sep = "-"),
#         seccion = SECCION,
#         casilla = dplyr::case_when(
#             stringr::str_detect(TIPO_CASILLA, "[B-C]") ~ "B-C",
#             stringr::str_detect(TIPO_CASILLA, "E") ~ "E",
#             stringr::str_detect(TIPO_CASILLA, "S") ~ "S",
#             TRUE ~ "M"
#         ),
#         tipo_seccion = TIPO_SECCION_21ago_2017,
#         rural = (CASILLA == 2) * 1,
#         region = iD_ESTADO,
#         pri_pvem = CPRI,
#         pan = CPAN,
#         panal = CPANAL,
#         prd_pt_mc = CPRD,
#         otros = NUM_VOTOS_NULOS + NUM_VOTOS_CAN_NREG,
#         total = pri_pvem + pan + panal + prd_pt_mc + otros,
#         ln = LISTA_NOMINAL
#     ) %>%
#     dplyr::group_by(region, seccion) %>%
#     dplyr::mutate(ln_seccion = sum(ln)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#         tamano = dplyr::case_when(
#             ln_seccion < 1000 ~ 1,
#             ln_seccion < 5000 ~ 2,
#             TRUE ~ 3
#         ),
#         tamano_md = (tamano == 2) * 1,
#         tamano_gd = (tamano == 3) * 1,
#         casilla_ex = (casilla == "E") * 1,
#         ln_total = ifelse(ln == 0, total, ln)
#     )  %>%
#     dplyr::select(casilla_id:ln, tamano_md:ln_total, estrato)
