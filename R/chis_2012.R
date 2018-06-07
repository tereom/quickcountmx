#' Governor electoral results for the State of Chiapas 2012.
#'
#' A dataset containing the election results for governor of Chiapas in 2012,
#' each row corresponds to a polling station and the variables include the
#' total counts per party.
#'
#' @format A data frame with 5474 rows and 22 variables:
#' #' \describe{
#'   \item{casilla_id}{Numeric identifier of the polling station.}
#'   \item{distrito_fed_17, distrito_fed_12}{Federal district, the districts
#'   where redefined in 2017, we include both variables.}
#'   \item{distrito_loc_17, distrito_loc_12}{Local district, the districts
#'   where redefined in 2017, we include both variables.}
#'   \item{are}{Electoral Responsability Area identifier for the 2012 election.}
#'   \item{seccion}{The section is the miminal electoral geographical unit that
#'   contains the polling station.}
#'   \item{casilla}{Type of polling station, Basic (B) is the
#'   basic and most common type, Contigua (C) these polling stations arise when
#'   the nominal list surpases 750 people, Extraoridinaria (E) these are
#'   installed when people can not reach their planned station and Especial (S)
#'   polling stations are where people can vote outside their asigned polling
#'   station}
#'   \item{tipo_seccion}{Section type (in 2017), whether the section of the
#'   station is rural (R), urban (U) or mixed (M)}
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
#'   \item{region}{Inidcates whether the local district of the polling station
#'   is in the western (1) or eastern (2) side of the state.}
#'   \item{casilla_ex}{Binary variable indicating if the polling station is of
#'   type Extraordinaria}
#'   \item{rural}{Binary variable indicating if the polling station is on a
#'   rural section}
#' }
#' @inherit gto_2012
#' @source \url{https://cartografia.ife.org.mx}
"chis_2012"
#'
# chiapas_2012 <- read_delim("~/Dropbox/COTECORA 2017-2018/Resultados electorales estatales/_07_Chiapas_2012.txt",
#     "|", escape_double = FALSE, trim_ws = TRUE)
#
# chis_2012 <- chiapas_2012 %>%
#     mutate(
#         casilla_id = 1:n(),
#         distrito_fed_17 = DISTRITO_FEDERAL_2017,
#         distrito_fed_12 = DISTRITO_FEDERAL_2012,
#         distrito_loc_17 = DISTRITO_LOCAL_2017,
#         distrito_loc_12 = DISTRITO_LOCAL_2012,
#         are = stringr::str_c(distrito_fed_12,
#             ID_AREA_RESPONSABILIDAD_2E_2012, sep = "-"),
#         seccion = SECCION,
#         casilla = dplyr::case_when(
#             stringr::str_detect(CASILLA, "(BAS)|(CONT)") ~ "B-C",
#             stringr::str_detect(CASILLA, "(EX)") ~ "E",
#             stringr::str_detect(CASILLA, "(ESP)") ~ "S"
#         ),
#         tipo_seccion = TIPO_SECCION_21ago_2017,
#         estrato = distrito_fed_17,
#         estrato = ifelse(estrato == 10,
#             paste(distrito_fed_17, distrito_loc_17, sep = "-"), estrato),
#         estrato = as.numeric(as.factor(estrato)),
#         pri_pvem_pna = PRI + Verde + Nva_Alianza,
#         pan = PAN,
#         prd_pt_mc = PRD_PT_MovCiud,
#         poc = Por_Chiapas,
#         otros = NO_REG + NULOS,
#         total = pri_pvem_pna + pan + prd_pt_mc + poc + otros,
#         ln = LN,
#     ) %>%
#     dplyr::group_by(seccion) %>%
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
#         region = dplyr::case_when(
#             distrito_loc_17 %in% c(4, 6, 7, 8, 9, 10, 17, 19, 20, 21, 22, 24)
#             ~ 1,
#             TRUE ~ 2
#         ),
#         casilla_ex = (casilla == "E") * 1,
#         rural = dplyr::case_when(tipo_seccion == "R" ~ 1, TRUE ~ 0),
#         ln_total = ifelse(ln == 0, total, ln)
#     )  %>%
#     dplyr::select(casilla_id:ln, tamano_md:ln_total)
