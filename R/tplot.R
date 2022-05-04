#' Plot a Phanerozoic time series
#'
#' @param x
#' @examples
#' Just specifying x and y
#' tplot(x = runif(100,30,410), y = rnorm(100,50,1))
#' Using stages, specifying first and last stage
#' tplot(x = runif(100,0,200), y = rnorm(100,0,1), time = c("Oxfordian","Campanian"), level = "stage")
#' @export

tplot <- function(x,y,time = NA, level = "period", start = NA, end = NA,
                  xlab = "x", ylab = "y", color = "black", type = "o",
                  pch = 19, cex = 1, lty = 0, lwd = 1) {

time[which(time == "Palaeogene")] <- "Paleogene"

all_period_start <- c(541,485.4,443.8,419.2,358.9,298.9,251.902,201.3,145,66,23.03)
all_period_end <- c(485.4,443.8,419.2,358.9,298.9,251.902,201.3,145,66,23.03,0)
all_periods <- c("Cambrian","Ordovician","Silurian","Devonian","Carboniferous","Permian","Triassic","Jurassic","Cretaceous", "Paleogene", "Neogene")
all_periods_short <- c("Cb","O", "S", "D", "C", "P",
                "T", "J", "K", "Pg", "N")
all_stages <- rev((time_bins(plot=FALSE))$interval_name)
all_stage_end <- rev((time_bins(plot=FALSE))$min_ma)
all_stage_start <- rev((time_bins(plot=FALSE))$max_ma)
all_stages_short <- substring(all_stages,1,2)
  if(level == "period") {
    if(is.na(time[1])) {
      periodindex <- which(max(x)>all_period_end & min(x)<all_period_start)
      cperiod <- all_periods[periodindex]
      time[1] <- cperiod[1]
      time[2] <- cperiod[length(cperiod)]
    } else  {
      periodindex <- which(all_periods == time[1]):which(all_periods == time[2])
      all_periods[periodindex]
    }
    if(length(time) == 2) {
      if(time[1] %in% all_periods) {
        ind <- which(all_periods==time[1])
        base <-  all_period_start[ind]
      }
      if(time[1] %in% all_stages) {
        ind <- which(all_periods==time[1])
        base <-  all_stage_start[ind]
      }
      if(time[2] %in% all_periods) {
        ind <- which(all_periods==time[2])
        top <-  all_period_end[ind]
      }
      if(time[2] %in% all_stages) {
        ind <- which(all_periods==time[2])
        top <-  all_stage_end[ind]
      }
     }
    }


    if(level == "stage") {
      if(is.na(time[1])) {
        stageindex <- which(max(x)>all_stage_end & min(x)<all_stage_start)
        cstage <- all_stages[stageindex]
        time[1] <- cstage[1]
        time[2] <- cstage[length(cstage)]
        } else  {
          stageindex <- which(all_stages == time[1]):which(all_stages == time[2])
          cstage <- all_stages[stageindex]
        }
        if(length(time) == 2) {

          if(time[1] %in% all_periods) {
            ind <- which(all_stages==time[1])
            base <-  all_periods_start[ind]
          }
          if(time[1] %in% all_stages) {
            ind <- which(all_stages==time[1])
            base <-  all_stage_start[ind]
          }
          if(time[2] %in% all_periods) {
            ind <- which(all_stages==time[2])
            top <-  all_periods_end[ind]
          }
          if(time[2] %in% all_stages) {
            ind <- which(all_stages==time[2])
            top <-  all_stage_end[ind]
          }
        }
      }


y1 <- min(y)-0.175*diff(range(y)) # lower boundary of plot area
y2 <- min(y)-0.05*diff(range(y))
y3 <- max(y)+0.05*diff(range(y))

    if(level == "period") {
      periodbase <- all_period_start[periodindex]
      periodtop <-  all_period_end[periodindex]
      periodmid <- (periodbase + periodtop) / 2


      plot(x,y, xlim = c(max(periodbase), min(periodtop)),
           ylim = c(y1,y3), yaxs = "i", xaxs = "i",
           xlab = xlab, ylab = ylab, col = color, type = type,
           pch = pch, cex = cex, lty = lty, lwd = lwd)


      for (i in 1:length(periodbase)) {
        polygon(c(periodbase[i],periodtop[i],periodtop[i],periodbase[i]),xpd=TRUE,
                c(y1,y1,y2,y2), col= "white", border= "black")
      }
      text(periodmid, mean(c(y1,y2)),
     labels = all_periods_short[periodindex], srt = 0, adj = c(0.5,0.5), cex=cex)
    }

  if(level == "stage") {
  stagebase <- all_stage_start[stageindex]
  stagetop <-  all_stage_end[stageindex]
  stagemid <- (stagebase + stagetop) / 2


  plot(x,y, xlim = c(max(stagebase), min(stagetop)),
       ylim = c(y1,y3), yaxs = "i", xaxs = "i",
       xlab = xlab, ylab = ylab, col = color, type = type,
       pch = pch, cex = cex, lty = lty, lwd = lwd)


  for (i in 1:length(stagebase)) {
    polygon(c(stagebase[i],stagetop[i],stagetop[i],stagebase[i]),xpd=TRUE,
            c(y1,y1,y2,y2), col= "white", border= "black")
  }
  text(stagemid, mean(c(y1,y2)),
       labels = all_stages_short[stageindex], srt = 0, adj = c(0.5,0.5), cex=cex)
  }
}

