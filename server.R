# server.R

source('Helpers.R')

shinyServer(function(input, output) {
  
  dist.name <- reactive({
    switch(input$dist,
           norm = "Normal",
           unif = "Uniform",
           chisq = "Slightly Skewed",
           weib = "Very Skewed"
    )
  })

  pop.mean <- reactive({
    switch(input$dist,
           norm = norm.pars$mu,
           unif = (unif.pars$min + unif.pars$max)/2,
           chisq = chisq.pars$df,
           weib = weib.pars$scale*gamma(1+1/weib.pars$shape)
    )
  })
  
  pop.med <- reactive({
    switch(input$dist,
           norm = norm.pars$mu,
           unif = (unif.pars$min + unif.pars$max)/2,
           chisq = qchisq(0.5, df = chisq.pars$df),
           weib = qweibull(0.5, shape = weib.pars$shape, scale = weib.pars$scale)
    )
  })
  
  pop.Q1 <- reactive({
    switch(input$dist,
           norm = qnorm(0.25, norm.pars$mu,norm.pars$s),
           unif = qunif(0.25, min=unif.pars$min, max=unif.pars$max),
           chisq = qchisq(0.25, df = chisq.pars$df),
           weib = qweibull(0.25, shape = weib.pars$shape, scale = weib.pars$scale)
    )
  })
  
  pop.Q3 <- reactive({
    switch(input$dist,
           norm = qnorm(0.75, norm.pars$mu,norm.pars$s),
           unif = qunif(0.75, min=unif.pars$min, max=unif.pars$max),
           chisq = qchisq(0.75, df = chisq.pars$df),
           weib = qweibull(0.75, shape = weib.pars$shape, scale = weib.pars$scale)
    )
  })
  
  pop.par.line <- reactive({
    tmp = input$dist
    switch(input$theta, 
           mean= pop.mean(),
           median=pop.med(),
           Q1 = pop.Q1(),
           Q3 = pop.Q3()
    )
  })
  

  
  # For the Population density
  output$pop.head <- renderText({
    paste("Suppose we have a ", dist.name(), " population distribution ", ',', sep='')
  })

  output$dens <- renderPlot({
    x = seq(-2, 30, length=400)
    d = dens(x, input$dist)
    plot(x,d, yaxt='n', ylab='', xlab='', type='l', lwd=4)
    if(input$pop.par.box==TRUE){abline(v=pop.par.line(), lwd=3, col='red')}
  })

  # For a single sample
  samp.1 <- reactive({
    sing.samp(input$N, input$dist)
  })
  
  samp.par.line <- reactive({
    switch(input$theta, 
           mean= mean(samp.1()),
           median=median(samp.1()),
           Q1 = quantile(samp.1(), probs = 0.25),
           Q3 = quantile(samp.1(), probs = 0.75))
  })
  
  output$samp.head <- renderText({
    paste("One sample of size ", input$N,' from this population is:', sep='')
  })
  
  output$samp <- renderPlot({
    hist(samp.1(), col='steelblue', breaks=15, xlim=c(-2,30), main="", xlab='')
    if(input$pop.par.box==TRUE){abline(v=pop.par.line(), lwd=3, col='red')}
    if(input$samp.mean.box==TRUE){abline(v=samp.par.line(), lwd=4, col='green')}
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
          ' from a ', dist.name() ,' population is:', sep='')
  })
  
  
  output$samp.dist <- renderPlot({
    tmp = samp.dist(n = input$N, dis=input$dist, stat=input$theta)
#     tmp.dens = density(tmp, bw=0.4)
#     plot(tmp.dens, lwd=4, main="", yaxt='n', ylab='', xlab='', 
#          xlim=c(0,30))
    hist(tmp, main="", yaxt='n', ylab='', xlab='', xlim=c(-2,30),
         col='steelblue')
    if(input$pop.par.box==TRUE){abline(v=pop.par.line(), lwd=3, col='red')}
    if(input$samp.mean.box==TRUE){abline(v=samp.par.line(), lwd=4, col='green')}
  })
  
  
})

