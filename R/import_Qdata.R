#' Imports data regarding occurrence of wood from files under path (path>1 directory=1 event>all logs of the event)
#' @param path the path towards the Qdata file
#' @param site the name of the site considered
#' @importFrom magrittr %>%
#' @export
#' @return Qdata
#' @examples
#' Qdata=import_Qdata("data-raw/Qdata/Qdatc_Ain.csv", site="Ain")
#' Qdata=import_Qdata("data-raw/Qdata/Qdatc_Allier.csv", site="Allier")
import_Qdata=function(path,site=path){
  Qdata=readr::read_csv2(path) %>%
    dplyr::select(station,Time,Q) %>%
    dplyr::mutate(site=site)
  return(Qdata)
}
