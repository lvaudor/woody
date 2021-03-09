#' Interpolates Ddata to add descriptor variables to Wdata
#' @export
#' @param Wdata
#' @param Ddata descriptive data
#' @return Wdata completed with variables Q, T_Q, and S and rT_Q
#' @examples
complete_Wdata_with_Ddata=function(Wdata,Ddata, newvars=c("Q","T_Q","S","rT_Q")){
    result = Wdata
    for (var in newvars) {
        varfactor = class(Ddata[[var]]) != "numeric"
        if (varfactor) {
            levs = levels(as.factor(Ddata[[var]]))
            Ddata[[var]] = as.numeric(Ddata[[var]])
        }
        result[[var]] = approx(Ddata$Time,
                               Ddata[[var]],
                               xout = Wdata$Time)$y
        if (varfactor) {
            result[[var]] = factor(levs[result[[var]]],
                                   levels=levs)
        }
    }
    return(result)
}
