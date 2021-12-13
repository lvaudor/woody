#' Imports wind data
#' @param path the path towards the wind data file
#' @export
#' @return Vdata
import_Vdata=function(path,site=path){
  Vdata=readr::read_delim(path,";",escape_double=FALSE,trim_ws=TRUE) %>%
    dplyr::mutate(Date=lubridate::ymd(Date)) %>%
    dplyr::mutate(FFM=tidyr::replace_na(FFM,0))
  return(Vdata)
}
