# server.R

source('Helpers.R')

shinyServer(function(input, output) {
  # For the Population density
  output$pop.head <- renderText({
    paste("Suppose we have a ", input$dist, " population distribution ", ',', sep='')
  })

  output$dens <- renderPlot({
    x = seq(-2, 30, length=400)
    d = dens(x, input$dist)
    plot(x,d, yaxt='n', ylab='', xlab='', type='l', lwd=4)
    if(input$pop.mean.box==TRUE){abline(v=pop.mean(), lwd=3, col='red')}
  })

  # For a single sample
  samp.1 <- reactive({
    sing.samp(input$N, input$dist)
  })
  
  output$samp.head <- renderText({
    paste("One sample of size ", input$N,' from this population is:', sep='')
  })
  
  output$samp <- renderPlot({
    hist(samp.1(), col='steelblue', breaks=15, xlim=c(-2,30), main="", xlab='')
    if(input$pop.mean.box==TRUE){abline(v=pop.mean(), lwd=4, col='red')}
    if(input$samp.mean.box==TRUE){abline(v=mean(samp.1()), lwd=4, col='green')}
  })
  
  output$samp.stats <- renderTable({
    data.frame(mean=mean(samp.1()), sd = sd(samp.1()),
               Q1 = quantile(samp.1(), 0.25),
               median = median(samp.1()),
               Q3 = quantile(samp.1(), 0.75),
               row.names = 'values'
               )
  })
  
  # Now for the Sampling distribution
  output$samp.dist.head <- renderText({
    paste("The distribution of the ", input$theta, 
          " with a sample of size ", input$N,
          ' from a ', input$dist ,' population is:', sep='')
  })
  
  pop.mean <- reactive({
  switch(input$dist,
         norm = norm.pars$mu,
         unif = (unif.pars$min + unif.pars$max)/2,
         chisq = chisq.pars$df,
         weib = weib.pars$scale*gamma(1+1/weib.pars$scale)
  )
  })
  
  output$samp.dist <- renderPlot({
    tmp = samp.dist(n = input$N, dis=input$dist, stat=input$theta)
#     tmp.dens = density(tmp, bw=0.4)
#     plot(tmp.dens, lwd=4, main="", yaxt='n', ylab='', xlab='', 
#          xlim=c(0,30))
    hist(tmp, main="", yaxt='n', ylab='', xlab='', xlim=c(0,30),
         col='steelblue')
    if(input$pop.mean.box==TRUE){abline(v=pop.mean(), lwd=3, col='red')}
    if(input$samp.mean.box==TRUE){abline(v=mean(samp.1()), lwd=4, col='green')}
  })
  
  
})

