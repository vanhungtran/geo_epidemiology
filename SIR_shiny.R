require(shiny)
require(deSolve)
require(phaseR)


#This creates the User Interface (UI)
ui = pageWithSidebar(
  #The title
  headerPanel("The SIR model"),
  #The sidebar for parameter input
  sidebarPanel(
    #Sliders:
    sliderInput("beta", "Transmission (yr^-1):", 300,
                min = 0, max = 1000),
    sliderInput("infper", "Infectious period (days)", 5,
                min = 1, max = 100),
    sliderInput("mu", "birth rate:", 5,
                min = 0, max = 100),
    sliderInput("T", "Time range:",
                min = 0, max = 1, value = c(0,1))
  ),
  #Main panel for figures and equations
  mainPanel(
    #Multiple tabs in main panel
    tabsetPanel(
      #Tab 1: Time plot (plot1 from server)
      tabPanel("Time", plotOutput("plot1")),
      #Tab 2: Phase plot (plot2 from server)
      tabPanel("Phase plane", plotOutput("plot2",
                                         height = 500)),
      #Tab 3: MathJax typeset equations
      tabPanel("Equations",
               withMathJax(
                 helpText("Susceptible $$\\frac{dS}{dt} =
\\mu (N - S) - \\frac{\\beta I S}{N}$$"),
                 helpText("Infecitous $$\\frac{dI}{dt} =
\\frac{\\beta I S}{N} - (\\mu+\\sigma) I$$"),
                 helpText("Removed $$\\frac{dR}{dt} =
\\gamma I - \\mu R$$"),
                 helpText("Reproductive ratio $$R_0 =
\\frac{1}{\\gamma+\\mu} \\frac{\\beta N}{N}$$")
               ))
    ))) #End of ui()
# This creates the 'behind the scenes' code (Server)
server = function(input, output) {
  #Gradient function for SIR model
  sirmod=function(t, x, parms){
    S=x[1]
    I=x[2]
    R=x[3]
    
    beta=parms["beta"]
    mu=parms["mu"]
    gamma=parms["gamma"]
    N=parms["N"]
    dS = mu * (N - S) - beta * S * I / N
    dI = beta * S * I / N - (mu + gamma) * I
    dR = gamma * I - mu * R
    res=c(dS, dI, dR)
    list(res)
  }
 
  
  
   #Gradient function used for phaseR phase-plot
  simod=function(t, y, parameters){
    S=y[1]
    I=y[2]
    beta=parameters["beta"]
    mu=parameters["mu"]
    gamma=parameters["gamma"]
    N=parameters["N"]
    dS = mu * (N - S) - beta * S * I / N
    dI = beta * S * I / N - (mu + gamma) * I
    res=c(dS, dI)
    list(res)
  }
 
  
  
   #Plot1: renderPlot to be passed to UI tab 1
  output$plot1 = renderPlot({
    #input\$xx's are pulled from UI
    times = seq(0, input$T[2], by=1/1000)
    parms = c(mu = input$mu, N = 1, beta = input$beta,
              gamma = 365/input$infper)
    start = c(S=0.999, I=0.001, R = 0)
    R0 = round(with(as.list(parms), beta/(gamma+mu)), 1)
    #Integrate ode with parameters pulled from UI
    out=ode(y=start, times=times, func=sirmod,
            parms=parms)
    out=as.data.frame(out)
    #Plot1
    sel=out$time>input$T[1]&out$time<input$T[2]
    plot(x=out$time[sel], y=out$S[sel], ylab="fraction",
         xlab="time", type="l", ylim=range(out[sel,-c(1,4)]))
    title(paste("R0=", R0))
    
    
    
    lines(x=out$time[sel], y=out$I[sel], col="red")
    lines(x=out$time[sel], y=out$R[sel], col="green")
    legend("right", legend=c("S", "I", "R"),
           lty=c(1,1,1), col=c("black", "red", "green"))
  })
  
  
  #Plot2: renderPlot to be passed to UI tab 2
  output$plot2 = renderPlot({
    times = seq(0, input$T[2], by=1/1000)
    parms = c(mu = input$mu, N = 1, beta = input$beta,
              gamma = 365/input$infper)
    start = c(S=0.999, I=0.001, R = 0)
    R0 = round(with(as.list(parms), beta/(gamma+mu)), 1)
    #Integrate simod
    out=ode(y=start[-3], times=times, func=simod,
            parms=parms)
    out=as.data.frame(out)
    
    
    
    
    #Plot2
    plot(x=out$S, y=out$I, xlab="Fraction suceptible",
         ylab="Fraction infected", type="l")
    title(paste("R0=", R0))
    #Add vector field
    fld=flowField(simod, x.lim=range(out$S), y.lim=
                    range(out$I), parameters=parms, system="two.dim",
                  add=TRUE, ylab="I", xlab="S")
    #Add isoclines
    abline(v=1/R0, col="green")
    curve(parms["mu"]*(1-x)/(parms["beta"]*x), min(out$S),
          max(out$S), add=TRUE, col="red")
    legend("topright", legend=c("I-socline",
                                "S-isocline"), lty=c(1,1), col=c("red", "green"))
  })
} #End of server()



shinyApp(ui, server)