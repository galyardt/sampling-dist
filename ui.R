shinyUI(fluidPage(
  titlePanel("Sampling Distribution Applet"),
  
  sidebarLayout(
    sidebarPanel( 
      selectInput('dist', label=h4("Distribution"), 
                  choices = c("Normal"='norm', "Uniform"='unif', "Slightly Skewed"='chisq', "Very Skewed" ='weib')),
      br(),
      sliderInput('N', label=h4('Sample Size'), min=5, max=100, value = 50),
      br(),
      selectInput('theta', label=h4("Parameter of Interest"), 
                  choices = c("mean", "median", "Q1", "Q3")),
      br(),
      checkboxInput('pop.mean.box', "Show Population Mean", FALSE),
      br(),
      checkboxInput('samp.mean.box', "Show Mean of Example Sample", FALSE)
      ),
    
    mainPanel(
      # main population
      h2(textOutput('pop.head')),
      p("So that the population density looks like this:"),
     plotOutput("dens"),
      
      # Single Sample
      h2(textOutput("samp.head")),
      plotOutput("samp"),
      h4("The summary statistics for this sample are:"),
      tableOutput("samp.stats"),
 
     # Sampling Distribution
     h2(textOutput("samp.dist.head")),
     plotOutput("samp.dist")
      )
  )
))