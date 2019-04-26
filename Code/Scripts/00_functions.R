source("Scripts/00_functions_viz.R")


lu = function(x){
  length(unique(x))
}


replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated

# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}



female_fertility_model = function(cFF, ltFF, H, model = c("mult","add")){
  FF = c(cFF, ltFF, H)
  FF[is.na(FF)] = 0
  if(any((FF < 0) | (FF > 1))){ stop("fertility indicators must be between 0 and 1\n")}
  if(model == "mult"){
    return(cFF * ltFF * H)
  }else if(model == "add"){
    return((cFF + ltFF + H)/3)
  }else{stop("model name unknow \n")}
}


detect_seasonal_pattern = function(time = time, signal = signal, detrend_method = "smooth.spline", seasonal_trend_method = "GAMM", plot = TRUE, title = ""){
  dt = unique(round(diff(time), digits = 6))
  n_years = round(max(time) - min(time))
  if(length(dt)>1){stop("time vector is irregular \n")}
  if(length(time) != length(signal)){stop("time and signal vectors are of different length\n")}
  if((max(time)-min(time))<(3-1.5*dt)){stop("need at least 3 years of data \n")}
  
  # first, we detrend the signal with the specified method
  if(detrend_method == "smooth.spline"){
    trend = smooth.spline(signal, df = 2*n_years-1)$y
    detrended_signal = signal - trend
  }else{stop("detrend method not supported yet \n")}
  
  # then we run the wavelet analysis
  is_seasonal = wavelet_analysis(time = time, signal = detrended_signal, title = title, plot = plot)
  
  # if seasonal, get the seasonal pattern
  if(is_seasonal){
    if(seasonal_trend_method == "GAMM"){
      df = data.frame(time = time, day_of_year = time %%1, signal = detrended_signal)
      mod = gamm(signal ~ s(day_of_year, bs='cc') , data = df, method = "REML")
      seasonal_trend = predict.gam(mod$gam)
      remainder = detrended_signal - seasonal_trend
      peak = time[which.max(seasonal_trend)]
      amplitude = max(seasonal_trend) - min(seasonal_trend)
      
      time_series = data.frame(time = time, 
                               signal = signal, 
                               trend = trend, 
                               detrended_signal = detrended_signal,
                               seasonal_trend = seasonal_trend,
                               remainder = remainder)
      
      
      if(plot){
        par(mfrow = c(2,1), mar = c(0.5,2,2,0.2))
        plot(time, signal, type = "l" , main = "signal + trend")
        points(time, trend, type = "l", col = "green")
        
        plot(time, detrended_signal, type = "l", main = "detrended signal + seasonal")
        points(time, seasonal_trend, type = "l", col = "green")
      }
      
    }
  }else{
    peak = NA; amplitude = NA; time_series = data.frame()
  }
  


  
  output = list(is_seasonal = is_seasonal, time_series = time_series, peak = peak, amplitude = amplitude)
  return(output)
}




wavelet_analysis = function(time = time, signal = signal, title = "", plot = TRUE){
  
  if(nchar(title) == 0){ title = "Wavelet analysis"}
  
  signal_matrix = as.matrix(cbind(time, signal))
  
  # autocorrelation
  LAG1 = arima(signal, order = c(1,0,0))$coef[1]
  
  # wavelet analysis
  current_wt = wt(signal_matrix, 
                  dt = dt, dj = 15*dt, 
                  lag1 = LAG1, # pink noise
                  max.scale = 3,
                  # max.scale ## maybe look at this parameter if we have more than 3 years of data
                  sig.test = 1) # we want to run a significance test
  
  # significance levels
  sig_1yr <- wt.sig(signal_matrix,
                    dt=dt,
                    scale=current_wt$scale,
                    sig.test=1,
                    sig.level=0.95,lag1=LAG1)$signif
  
  # get the signficance level for the 1yr period
  # the contours are drawn at a modified significance value of 1
  # the modified significance level directly scales with the power, (higher power)==(higher significance level)
  # to get the signigicance used for the contours
  # 1. calc the variance
  sigma2 <- var(signal)
  # 2. take the average of the power across the observations
  power.avg<- apply(current_wt$power,1,mean)
  # 3. calculate the modified significance, the power is scaled by the variance and then divided by the significance level
  signif.modified <- power.avg/(sigma2*sig_1yr)
  power.modified.by.variance<- power.avg/sigma2
  
  # tolerance level for modified significance contours = 1
  # Sigificance contours will be drawn around all regions of the spectrum where spectrum/percentile >= tol. 
  # Default is 1
  if(plot){
    plot(x = current_wt$scale, y = signif.modified, type='l',ylim=c(0,2.5),xlab='period (yrs)', ylab='power') 
    abline(h=1,lty=2)
  }
  
  mod.sig<- data.frame(
    period=current_wt$period,
    scale=current_wt$scale,
    mod.sig=signif.modified)
  
  mod.sig<- subset(mod.sig,mod.sig>=1)
  sig.period.range<- rep(NA,2)
  if(nrow(mod.sig)>0){
    sig.period.range<- round(range(mod.sig$period),digits=1)
    is_seasonal<- (1>=sig.period.range[1] & 1<=sig.period.range[2])
  }else{is_seasonal<- FALSE}
  
  
  if(plot){
    plot(current_wt, main = paste0(title," - ",ifelse(is_seasonal,"","NOT "),"seasonal"))
#    abline(h = which.min(current_wt$period - 1), col = "green3", lty = 2, lwd = 2)
    }
  
 return(is_seasonal)
}



