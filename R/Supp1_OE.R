#' A cheater function for getting only the Supp1_OE scores
#'
#' @param bugs input taxonomy data
#' @param stations input station data
#' @param rand random integer for subsampling
#'
#' @return A data.frame for the Supp1_OE report
#' @export
#'
#' @examples
#' data(bugs_stations) 
#' Supp1_OE(bugs = bugs_stations[[1]], stations = bugs_stations[[2]])
Supp1_OE <- function(bugs, stations, rand = sample.int(10000, 1)){

  oe <- new("oe", bugs, stations)
  oe_s <- subsample(oe, rand)
  oe_s <- score(oe_s)
  
  predict <- predict(oe_stuff[[1]],newdata=unique(oe_s@predictors[,oe_stuff[[4]]]),type='prob')
  colnames(predict) <- paste0("pGroup", 1:11)
  
  oe_s@predictors$StationCode <- as.character(oe_s@predictors$StationCode)
  E <- cbind(StationCode = unique(oe_s@predictors$StationCode), 
             predict %*% apply(oe_stuff[[2]],2,function(x){tapply(x,oe_stuff[[3]],function(y){sum(y)/length(y)})}))
  E <- merge(oe_s@predictors[, c("StationCode", "SampleID")], melt(as.data.frame(E), id.vars="StationCode"),
             all=TRUE, by = "StationCode")

  oe_s@oesubsample$Replicate_mean <- apply(oe_s@oesubsample[, paste("Replicate", 1:20)], 1, mean)
  O <- dcast(oe_s@oesubsample, SampleID + StationCode ~ STE, value.var="Replicate_mean", sum, na.rm=TRUE)
  O <- melt(O, id.vars=c("SampleID", "StationCode"))
  
  result <- merge(E, O, by=c("variable", "StationCode", "SampleID"), all=TRUE)
  names(result) <- c("OTU", "StationCode", "SampleID", "CaptureProb", "MeanObserved")
  result$CaptureProb<-as.numeric(result$CaptureProb)
  result$MeanObserved[is.na(result$MeanObserved)] <- 0
  out <- result[, c("StationCode", "SampleID", "OTU", "CaptureProb", "MeanObserved")]
  
  return(out)

}




