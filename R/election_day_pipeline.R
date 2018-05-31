#' @importFrom magrittr %>%
#' @importFrom rlang !! !!! :=
#' 
table_frame <- dplyr::data_frame(estado = c("00","07", "11", "17"),
               marco = c("marco_nal_2018", "marco_chis_2018",
                "marco_gto_2018", "marco_mor_2018"),
               candidatos = 
                 list(c("RAC", "JAMK", "AMLO", "JHRC"),
                 c("JAAB", "RAAG", "RCEC", "LFCC", "JAOR"),
                 c("DSRV", "GSG", "FACG", "FRSP", "MBSL"),
                 c("VMCS", "JAMO", "MRGC", "NLMLC", "CBB", "JAVJ", "MRA", "FDH")))
 
#' @export
process_batch <- function(path_name, file_name){
  all_data_filename = "./procesados/remesas.rds"
  new_name <- paste("./procesados/", file_name, "_procesado.rds", sep="")
  data_in <- readr::read_csv(path_name)
  # do processing ########
  tipo <- stringr::str_sub(file_name, 8, 9)
  estado_str <- stringr::str_sub(file_name, 10, 11)
  table_frame_in <- dplyr::filter(table_frame, estado == estado_str)
  marco_name <- table_frame_in %>% dplyr::pull(marco)
  candidatos <- table_frame_in$candidatos[[1]]
  marco <- get(data(list = marco_name, package = "quickcountmx"))
  # get id
  #print(head(data_in))
  data_out <- data_in %>% dplyr::mutate(id =
    stringr::str_c(ID_ESTADO, SECCION, ID_CASILLA, TIPO_CASILLA, 
                 EXT_CONTIGUA, sep = "-")) %>% 
    dplyr::select(id, dplyr::one_of(candidatos)) %>% 
    dplyr::right_join(marco)
  
  #######################
  saveRDS(data_out, file = new_name)
  df_new <- dplyr::data_frame(archivo = file_name, hora = Sys.time(), 
                              datos = list(data_out), originales = list(data_in))
  if(file.exists(all_data_filename)){
    df_prev <- readRDS(all_data_filename)
    df_agg <- dplyr::bind_rows(df_prev, df_new)
  } else {
    df_agg <- df_new
  }
  saveRDS(df_agg, all_data_filename)
  # run model
  # run_model()
}
