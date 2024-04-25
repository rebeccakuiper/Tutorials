# Function to create a plot of phi(dt) parameters over a range of dt

phiplot <- function(est = A_pop, # either a drift matrix or phi matrix (lagged eeffects)
                    maxdt = 2.5, # Maximum time-interval 
                    step = .01,  # decreasing this number makes the phi-dt lines smoother
                    ct = TRUE, # is est a drfit matrix (ct = TRUE) or a phi matrix (ct = FALSE)?
                    seperate = TRUE, # plot AR and cross-lagged effects seperately?
                    add = FALSE, # add lines to existing plot? Default is no (add = FALSE)
                    ylim = NULL, # optional: supply a custom y-axis range
                    legtune = 0, # tuning parameter for position of the legend
                    lwd = 3, # thickness of the plotted lines
                    addlegend = TRUE, # add a legend or not?
                    main = NULL # optional: custom plot title
                    ){
  library(RColorBrewer)
  p <- nrow(est)

  # Create vector of titles
  if(is.null(main)){
    if(seperate){
      main <- c("Auto-regressive Parameters",
                "Cross-Lagged Parameters")
    }
    if(!seperate){
      main <- c("Lagged Parameter vs Time Interval")
    }
  }
  
  # Create storage/other
  dt <- seq(0,maxdt,step)
  blank <- array(data=NA,dim=c(p,p,length(dt)))
  colvec <- brewer.pal(n=p,name="Set1") # colors are columns of phi
  typevec <- seq(1:p) # line type determined by row

  if(ct){
    for(i in 1:length(dt)){
      blank[,,i] <- expm::expm(est*dt[i]) 
      }
        }
  if(!ct){
    blank[,,1:length(dt)] <- est 
  }
  
  # get y-axis limits if not specified
  if(is.null(ylim)){
    ylim <- pretty(range(c(blank)))
    ylim <- c(ylim[1],ylim[length(ylim)])
  }
  
  #### Lagged parameter plot for auto-regressive effects
  if(!add){
    plot.new()
    plot.window(xlim=c(0,maxdt), ylim=ylim)
    axis(2); title(ylab = expression(paste(Phi,"(", Delta, "t) values")), line=2.5)
    axis(1); title(xlab = expression(paste("Time-interval (", Delta, "t)", sep="")),
                   line=2.5)
    abline(h=0)
    if(seperate){ title(main = main[1]) }
    if(!seperate){ title(main = main[1]) }
  }
    for(i in 1:p){
    lines(y=blank[i,i,],x=dt,col=colvec[i],lwd=lwd, lty=typevec[i])
      }
  
  #### Cross-lagged effects
    # Make a new plot or add to previous
  if(!add){
    if(seperate){
      plot.new()
      plot.window(xlim=c(0,maxdt), ylim=ylim)
      axis(2); title(ylab = expression(paste(Phi,"(", Delta, "t) values")), line=2.5)
      axis(1); title(xlab = expression(paste("Time-interval (", Delta, "t)", sep="")),
                     line=2.5)
      title(main = main[2])
      abline(h=0)
                } 
        }
    # Plot lines
  for(i in 1:p){
    for(j in 1:p){
      if(i==j) next else
        lines(y=blank[i,j,],x=dt,col=colvec[j],lwd=lwd, lty=typevec[i])
      }
    }
    
# Add legend if making a new plot
    if(!add){
      if(addlegend){
  leg1 <- paste0("-> Y",1:p)
  leg2 <-  paste0("Y",1:p, " ->")
  
  l1obj <- legend("topright",
       leg1,
       lty = typevec,
       col = "grey", lwd = lwd)
  l2pos <- matrix(c(l1obj$rect$left - l1obj$rect$w, l1obj$rect$top,
           l1obj$rect$left, l1obj$rect$top - l1obj$rect$h),
           2,2,byrow=T)
  l2pos[,1] <- l2pos[,1] + legtune
  legend(l2pos,
         leg2,
         lty = 1,
         col = colvec,
         lwd = lwd)
      }
    }
  } # end of Function
