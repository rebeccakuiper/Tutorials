# Function to plot an Impulse Response Function 

plotIRF <- function(est = A_true, # p x p effects matrix. May be a drift matrix (default) or phi matrix
                    start = NULL, # p-vector of starting values
                    maxdt = 3, # length of the time-interval
                    step = .01, # size of TI steps to take in plotting
                    ct = TRUE, # Is est a drift matrix (ct = TRUE) or phi matrix (ct = FALSE)
                    ylim = NULL, # customise range of y axis 
                    lwd = 3, # set width of lines plotted
                    addlegend = TRUE, # plota legend or not?
                    main = "Impulse Response Function", # title of the plot
                    colvec = NULL # vector of colours - NULL results in automatic selection
                    ){
  library(RColorBrewer)
  p <- nrow(est)
  
  # Create storage/other
  if(ct) dt <- seq(0,maxdt,step)
  if(!ct) dt <- seq(0,ceiling(maxdt),1) 
  
   blank <- matrix(data = NA, length(dt), p)
   # blank <- matrix(data = NA, ceiling(maxdt), p)
  
  if(is.null(colvec))  colvec <- brewer.pal(n=p,name="Set1") # colors are columns of phi
  
  if(is.null(start)) start <- runif(p,0,1)
  
  # Get impulse response in matrix form
  if(ct){
    for(i in 1:length(dt)){
      blank[i,] <- t(expm(est*dt[i])%*%start)
    }
  }
  if(!ct){
    blank[1,] <- start
    for(i in 2:nrow(blank)){
    blank[i,] <- t((est%^%(i-1))%*%start)
    }
  }
  
  # get y-axis limits if not specified
  if(is.null(ylim)){
    ylim <- pretty(range(c(blank)))
    ylim <- c(ylim[1],ylim[length(ylim)])
  }
  
  #### IRF plot
    plot.new()
    plot.window(xlim=c(0,maxdt), ylim=ylim)
    axis(2); title(ylab = "Variable Value", line=2.5)
    axis(1); title(xlab = "Time (t)",line=2.5)
    abline(h=0)
    title(main = main)
  
  for(i in 1:p){
    lines(y = blank[, i], x = dt,col = colvec[i], lwd = lwd, lty = 1)
    if(!ct){
      points(y = blank[, i], x = dt,col = colvec[i], lwd = lwd)
    }
  }
  
  
  if(addlegend){
      l1obj <- legend("topright",
                      paste0("Y",1:p, "(t)"),
                      lty = 1,
                      col = colvec, lwd = lwd)
    }
  
} # end of Function
