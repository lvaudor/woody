#' Reads one wood log file. To be used from import_Wdata()
#' @param file the path to one log file
#' @return the data from file
#' @examples
#' woody:::read_1_wood_log_file("data-raw/wood_data_Ain/event20071122/Wood Log 20071122 15h11m.txt")
#' woody:::read_1_wood_log_file("data-raw/wood_data_Ain/event20120101/Event_20120101.xlsx")
read_1_wood_log_file=function(file){
  if(stringr::str_sub(file,-5)==".xlsx"){
    result=readxl::read_excel(file) %>%
      dplyr::select(Time,Length)
    }
  if(stringr::str_sub(file,-4)==".txt"){
    result=readr::read_table2(file,skip=1) %>%
      dplyr::mutate(Time=paste0(lubridate::dmy(Date),
                                " ",
                                as.character(Time))) %>%
      dplyr::mutate(Time=lubridate::ymd_hms(Time)) %>%
      dplyr::select(Time,Length=`Length(m)`)
  }
  return(result)
}
