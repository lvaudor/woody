#' Interpolates Qdata to add discharge variables to Wdata
#' @export
#' @param Wdata
#' @param Qdata
#' @return Wdata completed with variables Q, T_Q, and S and rT_Q
#' @examples
complete_Wdata_with_Qdata=function(Wdata,Qdata, newvars=c("Q","T_Q","S","rT_Q")){
    result=Wdata
    for(var in newvars){
      result=result %>%
        mutate(!!var:=approx(Qdata$Time,Qdata[[var]], xout=Wdata$Time)$y)
    }
    return(result)
}
