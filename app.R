library(shiny)

server <- function(input, output) {
  x<-c(1,1,1,3,3,3,4,4,7,7,7,7,9,9,12,12,13,13,14,14,14,18,18,18)
  y <- reactive({ 
    input$rand
    input$beta0+input$beta1*x+rnorm(length(x),0, input$sigma) 
    })
  
  ## Simulations to look at the distributions of the estimated coefficients. 
  
  sim <- reactive({
    bcoef<-matrix(data = 0, nrow = input$nsim, ncol = 2)
    for(i in 1:input$nsim){
      newy<-input$beta0+input$beta1*x+rnorm(24,0, input$sigma)
      new.slr<-lm(newy~x)
      bcoef[i,]<-coef(new.slr)
    }
    bcoef
  })
  
  
  
  
  output$simPlot <- renderPlot({
    input$simulate
    isolate({
    plot(y() ~ x, type = 'n', xlab="Predictor", ylab="Response variable", ylim = c( min(y()) - 2*input$sigma, max(y()) + 2*input$sigma))
    bcoef <- sim()
    apply(bcoef, 1, function(x) lines(c(0:40/2),x[1]+x[2]*(c(0:40/2)), lty=1, lwd=1, col = rgb(0,0,0,.1)) )
    lines(input$beta0+input$beta1*c(0:40/2)~c(0:40/2), lty=1,lwd = 2, col = 'red')
    legend('topleft', col = c('red', 'black'), lwd = c(2,1), legend = c('true function', 'fits generated from random samples'))
    })
  })
  
  output$regPlot <- renderPlot({
    plot(y() ~ x, xlab="Predictor", ylab="Response variable")
    slr<-lm(y()~x)
    lines((coef(slr)[1]+coef(slr)[2]*(c(0:40/2)))~c(0:40/2), lty=2, lwd=3, col = 'blue')
    lines(input$beta0+input$beta1*c(0:40/2)~c(0:40/2), lty=1)
    legend('topleft', lwd = c(3,1), lty = c(2,1), legend = c('fitted', 'true function'), col = c('blue','black'))
    
  })
  
  output$coefPlot <- renderPlot({
    input$simulate
    isolate({
        bcoef <- sim()
  
        par(mfrow = c(1,2))
        hist(bcoef[,1], main = 'beta 0'); abline(v = input$beta0, col = 'red')
        hist(bcoef[,2], main = 'beta 1'); abline(v = input$beta1, col = 'red')
    }) 
})
}

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      h3('"True" Function Parameters'),
      sliderInput("beta0", "beta0 (intercept) :", min = -1000, max = 1000, value = 200),
      sliderInput("beta1", "beta1 (slope) :", min = -1000, max = 1000, value = 50),
      h3("Random Variation (noise)"),
      sliderInput("sigma", "sigma :", min = 0, max = 1000, value = 300),
      br(),
      actionButton('rand', 'Generate New Sample'),
      h3("Simulations"),
      numericInput("nsim", label = h4("number of iterations"), value = 100),
      actionButton('simulate', 'Run Simulation')
    ),
    mainPanel(plotOutput("regPlot"), h3("simulation results"),plotOutput("simPlot"), plotOutput('coefPlot'))
  )
))

shinyApp(ui = ui, server = server)
