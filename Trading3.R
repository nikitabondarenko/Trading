rm(list=ls())
#setwd('/Users/afoss/Documents/OpenData/Orderbook')
setwd("C:/Users/Nikita Bondarenko/Documents/Open Data Group")
library(data.table)
library(ggplot2)
library(gtable)
library(grid)
library(Hmisc)

options(scipen = 15, digits.secs = 6)
dat <- fread('book_ts.csv', header = TRUE)

convert_time_fast <- function(tim){  
  b <- tim - tim%/%10^12*10^12
  # hhmmssffffff
  ms <- b%%10^6; b <-(b-ms)/10^6
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600+ms/(3600*10^6))
}

dat[,Tc:= convert_time_fast(Time)]
dat[,LastPrice:= LastPrice/100]
dat[,MidPrice:= MidPrice/100]

hhmmss2Tc <- function(tim){  
  b <- tim - tim%/%10^12*10^12
  # hhmmss
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600)
}


# builds deltas
dat[,dTotBidSize:= TotBidSize-Lag(TotBidSize)]
dat[,dTotOfferSize:= TotOfferSize-Lag(TotOfferSize)]
dat[,dTotBidCt:= TotBidCt-Lag(TotBidCt)]
dat[,dTotOfferCt:= TotOfferCt-Lag(TotOfferCt)]

# filters for market hours
dat2 <- copy(dat[Tc %between%c(15,20.25)])
Tc <- dat2$Tc
LastPrice <- dat2$LastPrice


# finds suspicious orders
large_offers <- dat2[dTotOfferSize > .02 * Lag(TotOfferSize) & 
              dTotOfferCt == 1 & abs(LastSize) != abs(dTotOfferSize), ]
offer_suspect <- rbind(large_offers, dat2[dTotOfferSize %in% large_offers[,dTotOfferSize * -1] & 
              dTotOfferCt == -1 & abs(LastSize) != abs(dTotOfferSize), ])

large_bids <- dat2[dTotBidSize > .02 * Lag(TotBidSize) & dTotBidCt == 1 & 
              abs(LastSize) != abs(dTotBidSize), ]
bid_suspect <- rbind(large_bids, dat2[dTotBidSize %in% large_bids[,dTotBidSize * -1] & 
              dTotBidCt == -1 & abs(LastSize) != abs(dTotBidSize), ])

suspect <- unique(rbind(offer_suspect, bid_suspect))
setkey(suspect, V1)

# forces time to character for message matching
suspect[ ,chrTime:=as.character(Time, format = '%Y%m%d%H%M%OS6')]
suspect[, chrTime:=gsub('\\.','',chrTime)]

# re-examines messages
msgs <- fread('Capture_20110701_ES_OUT.txt', header = FALSE, col.names = c('Time','Prod','Type','Action','Level','Price','Size','KI','OrdCt','FIXMsgID'), colClasses = c('V1' = 'character'))

filtered_msgs <- msgs[Time %in% suspect[,chrTime] & Type != 2 & Prod == 'ESU1']
setkey(filtered_msgs, Type, Price, FIXMsgID)
filtered_msgs[,Time:= convert_time(Time)]
filtered_msgs[,Time:= as.POSIXct(Time)]

# gets prices to examine
prcs <- unique(filtered_msgs[,Price])

# looks at messages for a given price
filtered_msgs[Price %in% prcs[6]]


##########
#Plotting#
##########

# selects timeframe for plot
%dat4 <- dat[Time %between%c('2011-07-01 15:05:58','2011-07-01 15:06:05'),]
dat4 <- dat[Tc %between%c(hhmmss2Tc(150558),hhmmss2Tc(150605)),]

# plots overlay of bid/offer size and trades w/ price
grid.newpage()

p1 <- ggplot() + 
  geom_line(aes(Tc, -1*TotBidSize, color = 'BidSz'), dat4) +  
  geom_line(aes(Tc, TotOfferSize, color = 'OfferSz'), dat4)

p2 <- ggplot() +
  geom_point(aes(Tc, LastPrice, color = as.factor(LastAgg)), dat4) + 
  theme_bw() %+replace% theme(panel.background = element_rect(fill = NA))


g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)