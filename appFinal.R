library(data.table)
library(ggplot2)
library(shiny)


time2Tc <- function(tim){  
  b <- tim - tim%/%10^12*10^12
  # hhmmssffffff
  ms <- b%%10^6; b <-(b-ms)/10^6
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600+ms/(3600*10^6))
}

hhmmss2Tc <- function(tim){  
  b <- tim - tim%/%10^12*10^12
  # hhmmss
  ss <- b%%10^2; b <-(b-ss)/10^2
  mm <- b%%10^2; hh <-(b-mm)/10^2
  # if hours>=22, subtract 24 (previous day)
  hh <- hh - (hh>=22)*24
  return(hh+mm/60+ss/3600)
}

plotsuspectmsgs <- function(msgs){  
  msgs = msgs[order(Tc)]
  Tc = msgs$Tc
  Type = msgs$Type
  Action = msgs$Action
  Level = msgs$Level
  Price = msgs$Price
  Size = msgs$Size
  OrdCt = msgs$OrdCt   
  
  
  #xb = which(Type==0)
  #xa = which(Type==1)
  xt = which(Type==2)
  
  Tick = (Price - Price[xt[1]])*4
  cx = which(Level==1 & Type==0)
  Bid0 = Tick[cx[1]]
  cx = which(Level==1 & Type==1)
  Offer0 = Tick[cx[1]]
  limTc = c(min(Tc),max(Tc))
  limSize = c(0,max(Size))
  limOrd = c(0,max(OrdCt))
  limTick = c(min(Tick[xt]),max(Tick[xt]))
  
  grid.newpage()
  par(mfrow=c(3,2))
  #cl <- topo.colors(5)
  cl <- rainbow(5)
  
  plot(0,0,xlim=limTc,ylim = limSize,type = "n",xlab="Tc",ylab="Size",col.lab="blue")
  title(main="Bid Size", col.main="blue", font.main=1)
  for (i in 1:5){
    cx = which(Tick==Bid0+1-i & Type==0)
    lines(Tc[cx],Size[cx],col = cl[i])
  }
  plot(0,0,xlim=limTc,ylim = limSize,type = "n",xlab="Tc",ylab="Size",col.lab="blue")
  title(main="Offer Size", col.main="blue", font.main=1)
  for (i in 1:5){
    cx = which(Tick==Offer0-1+i & Type==1)
    lines(Tc[cx],Size[cx],col = cl[i])
    a = Tc[cx]
  }
  
  
  plot(0,0,xlim=limTc,ylim = limOrd,type = "n",xlab="Tc",ylab="Order Count",col.lab="blue")
  title(main="Bid Order Count", col.main="blue", font.main=1)
  for (i in 1:5){
    cx = which(Tick==Bid0+1-i & Type==0)
    lines(Tc[cx],OrdCt[cx],col = cl[i])
  }
  plot(0,0,xlim=limTc,ylim = limOrd,type = "n",xlab="Tc",ylab="Order Count",col.lab="blue")
  title(main="Offer Order Count", col.main="blue", font.main=1)
  for (i in 1:5){
    cx = which(Tick==Offer0-1+i & Type==1)
    lines(Tc[cx],OrdCt[cx],col = cl[i])
  }
  
  plot(Tc[xt],Tick[xt],xlim=limTc,ylim=limTick,pch='.',type='s',xlab="Tc", col="red",ylab="Price",col.lab="blue")
  title(main="Price (in Ticks)", col.main="blue", font.main=1)  
}


#msgs <- fread('Capture_20110701_ES_OUT.txt', header = FALSE, 
#        col.names = c('Time','Prod','Type','Action','Level','Price','Size','KI','OrdCt','FIXMsgID'))
# filters for market hours
#msgs[,Tc:= time2Tc(Time)]
#msgs <- msgs[Tc %between% c(15,20.25) & Prod == 'ESU1']
#msgs[,Price:= Price/100]
#msgs = msgs[order(Tc)]

#msgsT <- msgs[Type==2]
#TcT = msgsT$Tc
#msgsT <- msgsT[seq(1, length(TcT), 100)]




ui <- fluidPage(
  titlePanel("Trading Data for E-mini S&P 500"),
  h1("June 30, 2011"),
  strong("Visual Shiny App to assist locating spoofing"),
  br(),
  
  sidebarLayout(sidebarPanel(

  numericInput("Tc0", "Please Enter a Number:", value = 15)
  ),
  
  mainPanel(plotOutput("plot1"),
            br(), br(),
            plotOutput("plot2")
            ))
)


server <- function(input, output, session) {

  
  msgsP <- reactive({
    msgsP <- msgs[Tc %between%  c(input$Tc0,input$Tc0+.1)]
  })
  
  output$plot1 <- renderPlot({
    ggplot(msgsT,
           aes(x = Tc,
           y = Price)) + geom_line()+geom_point()+
           xlab("Time") +
           ylab("Price")
  })
  
  output$plot2 <- renderPlot({
    plotsuspectmsgs(msgsP())
  })

}


shinyApp(ui = ui, server = server)