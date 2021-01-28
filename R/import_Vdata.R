#' Imports wind data
#' @param path the path towards the wind data file
#' @export
#' @return Vdata
#' @examples
import_Vdata=function(path,site=path){
  Vdata=readr::read_delim(path,";",escape_double=FALSE,trim_ws=TRUE) %>%
    mutate(Date=lubridate::ymd(Date))
  return(Vdata)
}