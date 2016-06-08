#' Freeqdist Function fit gor a given data
#'
#' @param x vector whose frequency disribution is required
#' @param ci number of class intervals
#'
#' @return none
#' @keywords freqdist
#' @export
#' @examples
#' freqdist()

freqdist<-function(x,ci) {
var <- as.matrix(x)
mn<-min(var)
mx<-max(var)
ll<- plyr::round_any(mn, ci, f = floor)               
ul<- plyr::round_any(mx, ci, f = ceiling)  
diff=ul-ll
clint<-(ul-ll)/ ci
var<-var
freq<-(table(cut(var, breaks = ci)))
totf<-length(x)
rl<-(freq*100)/totf
clf<-c(rep(1:1))
clf[1]<-freq[1]
for (i in 2:length(freq) ) {
clf[i] = clf[i-1]+freq[i]
}
cgf<-c(rep(1:1))
cgf[1]<-sum(freq)
for (i in 2:length(freq) ) {
cgf[i] = cgf[i-1]-freq[i]
}
result<-cbind(freq,rl,clf,cgf)
dfresult<-as.data.frame(result)
names(dfresult)=c("Freq.","Rel. Freq.","Cum. < Freq.","Cum. > Freq.")
dfresult
}




#' This function calculates both mean and standard deviation
#'
#' @param x vector whose mean and standard deviations are required
#' @return none
#'
#' @keywords meansd
#' @export
#' @examples
#' meansd()
 
meansd<-function(x){ 
 reshape::funstofun(mean, sd)(x)
}

#' A Regression Residual Analysis 
#'
#' @param obj  object obtained from the lm function
#'
#' @return none
#' 
#' @keywords resana
#' @export
#' @examples
#' resana()


resana <- function(obj) {
  yy<-obj$fitted.values+obj$residuals
  rmse<-sqrt(sum(obj$residuals*obj$residuals)/length(obj$residuals)) 
  r<-cbind("Actual.y"=yy,"Fitted.y"=obj$fitted.values,"Residuals"=obj$residuals)
  ra <- list(coefficients=obj$coefficients,r=r,rmse=rmse)
  cat("      Residuals Analysis","\n\n")
  print(r)
  cat("\n")
  cat("RMSE = ",rmse,"  \n")
  }

#' Extract Train and Test datasets for any data frame 
#'
#' @param df  dataframe for which train and test data sets are to be extrcted
#'
#' @param n  number of observations 
#'
#' @return list containing two data frames train and test
#' 
#' @keywords trntest
#' @export
#' @examples
#' trntest()


trntest <- function(df,n) {
lst<-list()
sub<-(sample(1:nrow(df),n))
lst$train<-iris[sub,]
lst$test<-iris[-sub,]
return(lst)
}

