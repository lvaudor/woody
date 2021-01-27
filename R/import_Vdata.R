#' Imports wind data
#' @param path the path towards the wind data file
#' @export
#' @return Vdata
#' @examples
import_Vdata=function(path,site=path){
  if(!stringr::str_detect(path,"\\/$")){path=stringr::str_c(path,"/")}
  Vdata=read_delim(path,";",escape_double=FALSE,trim_ws=TRUE)
  return(Vdata)
}
