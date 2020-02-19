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


expand_compressed_tracking = function(x){
  xx = lapply(x, rep, x$stretch_length) %>%  as.data.frame()
  xx$date = xx$start_date + ave(rep(1,nrow(xx)), xx$user_id , xx$stretch_num, FUN =cumsum) - 1
  xx$tracking = 1
  xx = dplyr::select(xx,-start_date)
  return(xx)
}



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


detect_seasonal_pattern = function(time = time, signal = signal, detrend_method = "smooth.spline", seasonal_trend_method = "GAMM", plot = TRUE, title = "", remove_weekly_patterns = TRUE){
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
  
  
  if(remove_weekly_patterns){
    has_weekly_pattern = wavelet_analysis(time = time, signal = detrended_signal, title = title, plot = plot, period = dt*7)
    if(has_weekly_pattern){
      if(seasonal_trend_method == "GAMM"){
        df = data.frame(time = time, day_of_week = round((time/dt) %% 7), y = detrended_signal)
        mod = mgcv::gamm(y ~ s(day_of_week, k = 7 ), data = df, method = "REML")
        weekly_trend = mgcv::predict.gam(mod$gam)
      }
    }else{weekly_trend = signal * 0}
  }else{
    weekly_trend = signal * 0
  }
  detrended_signal_no_weekly = detrended_signal - weekly_trend
  
  # then we run the wavelet analysis
  is_seasonal = wavelet_analysis(time = time, signal = detrended_signal_no_weekly, title = title, plot = plot, period = 1)
  
  # if seasonal, get the seasonal pattern
  if(is_seasonal){
    if(seasonal_trend_method == "GAMM"){
      df = data.frame(time = time, day_of_year = time %%1, y = as.vector(detrended_signal_no_weekly))
      mod = mgcv::gamm(y ~ s(day_of_year, bs='cc', k = floor(1/dt/10)) , data = df, method = "REML")
      seasonal_trend = mgcv::predict.gam(mod$gam)
      peak = time[which.max(seasonal_trend)]
      amplitude = max(seasonal_trend) - min(seasonal_trend)
    }
  }else{
    peak = NA; amplitude = NA; 
    seasonal_trend = 0*signal;
  }
  
  remainder = signal - trend - weekly_trend - seasonal_trend
  
  if(plot){
    n_row = ifelse(remove_weekly_patterns, 4,3)
    par(mfrow = c(n_row,1), mar = c(0.5,2,2,0.2))
    
    plot(time, signal, type = "l" , main = "signal + trend")
    points(time, trend, type = "l", col = "green3", lwd = 2)
    
    if(remove_weekly_patterns){
      plot(time, detrended_signal, type = "l" , main = "detrended signal + weekly trend")
      points(time, weekly_trend, type = "l", col = "green3", lwd = 2)
    }

    plot(time, detrended_signal_no_weekly, type = "l", main = "detrended signal + seasonal")
    abline(h = 0, lty = 2)
    points(time, seasonal_trend, type = "l", col = "green3", lwd = 2)
    
    plot(time, remainder, type = "l", main = "remainder")
    abline(h = 0, lty = 2)
    par(mfrow = c(1,1))
  }
  
  
  time_series = data.frame(time = time, 
                           signal = signal, 
                           trend = trend, 
                           weekly_trend = weekly_trend,
                           seasonal_trend = seasonal_trend,
                           remainder = remainder,
                           detrended_signal = detrended_signal,
                           detrended_signal_no_weekly = detrended_signal_no_weekly)
  
  
  output = list(is_seasonal = is_seasonal, time_series = time_series, peak = peak, amplitude = amplitude)
  return(output)
}




wavelet_analysis = function(time = time, signal = signal, title = "", plot = TRUE, period = 1){
  
  if(nchar(title) == 0){ title = "Wavelet analysis"}
  
  signal_matrix = as.matrix(cbind(time, signal))
  dt = unique(round(diff(time), digits = 6))
  
  
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

  }
  
  mod.sig<- data.frame(
    period=current_wt$period,
    scale=current_wt$scale,
    mod.sig=signif.modified)
  
  mod.sig<- subset(mod.sig,mod.sig>=1)
  sig.period.range<- rep(NA,2)
  if(nrow(mod.sig)>0){
    sig.period.range<- range(mod.sig$period) # round(range(mod.sig$period),digits=1)
    is_periodic<- ((sig.period.range[1] <= period) & (sig.period.range[2]>= period))
  }else{is_periodic<- FALSE}
  
  
  if(plot){
    par(mfrow = c(2,1))
    plot(x = current_wt$scale, y = signif.modified, type='l',ylim=c(0,2.5),xlab='period (yrs)', ylab='power') 
    abline(h=1,lty=2)
    abline(v = period, lty = 2, col = "green")
    
    plot(current_wt, main = paste0(title," - ",ifelse(is_periodic,"","NOT "),"periodic (p = ",period,")"))
#    abline(h = which.min(current_wt$period - 1), col = "green3", lty = 2, lwd = 2)
    par(mfrow = c(1,1))
    }
  
 return(is_periodic)
}




###### PERIODIC FISHER TEST



periodic.fisher.test = function(t = 1:365, x = 2+ 0.5 * cos(1:365/365*2*pi), p = 365 , print = FALSE, normalize = TRUE){
  
  if(is.na(t[1])){ t = 1:length(x)}
  if(any(is.na(x))){stop("x holds NA values\n")}
  
  DT  = mean(diff(t))
  if(any(diff(t)!= DT)){stop("need a signal measured at regular intervals\n")}
  
  time.interval = t[length(t)] - t[1] + DT
  
  # normalize the signal
  mx = mean(x)
  if(normalize){x = x/mx}
  
  # fourier transform
  ft = fft(x)
  #Mod
  P = Mod(ft)^2 
  
  # get the index for that particular period
  i = as.numeric(time.interval/p)
  if(abs(i - round(i))>0.4){i = floor(i); i = c(i, i+1)}else{i = round(i)}
  i = i[which.max(P[i])]
  i = 1 + i
  
  if(print){cat("i : ",i,"\n")}
  
  # fourier score for that particular period
  fs = P[i]/sum(P[2:floor((length(x)+1)/2)])
  if(print){cat("fs : ",fs,"\n")}
  
  
  # pvalue
  pval = (1-fs)^(length(x)/2-2)
  if(print){cat("pval : ",pval,"\n")}
  
  
  # relative amplitude
  rel.amp = 2*sqrt(P[i])/length(x)
  
  # phase  
  e = ft[i]
  
  if(is.na(e)){phase = NA}else{
    if(Re(e)<0) phase = (atan(Im(e)/Re(e))+pi)
    else phase = (atan(Im(e)/Re(e))) %% (2*pi)
    phase = t[1] + (-phase+2*pi)%%(2*pi) / 2 / pi * p
  }
  
  return(list(pval = pval, rel.amp = rel.amp, phase = phase))
}


reconstruct.rhythm = function(t = 1:365, x = 2+ 0.5 * cos(1:365/365*2*pi), p = c(365,7), pval.max = 0.05 , normalize = TRUE){
  
  if(p[1] == "all"){
    DT  = mean(diff(t))
    if(any(diff(t)!= DT)){stop("need a signal measured at regular intervals\n")}
    time.interval = t[length(t)] - t[1] + DT
    periods = time.interval/(1:(floor(length(t)/2)))}
  else{
    periods = p
  }
  y = x*0+mean(x)
  for(p in periods){
    PFT = periodic.fisher.test(t = t, x = x, p = p, normalize = normalize)
    if(PFT$pval < pval.max){
      cat(p,"\t\tpval: ",PFT$pval,"\n");
      phase = as.numeric(PFT$phase) - as.numeric(t[1]) + 1
      t = as.numeric(t) - as.numeric(t[1]) + 1;
      y = y + mean(x) * PFT$rel.amp * cos(2*pi/p*(t-phase))}
  }
  return(y)
  
}
