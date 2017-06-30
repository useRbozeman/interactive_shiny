#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$power_plot <- renderPlot({
    
    # inputs
    effect <- input$delta/(input$sigma/sqrt(input$n)) # Effect Size

    ## Put sidedness into function
    # alt_sided = "two"
    
    n_grid <- 10000
    lim_a <- -4
    lim_b <- 8
    
    ## Build grids
    grid_x <- seq(from = lim_a,
                  to = lim_b,
                  length.out = n_grid)
    grid_x1 <- seq(qt((1-input$alpha), input$n-1, 0), lim_b, length.out = 400)
    grid_x2 <- seq(lim_a, qt((1-input$alpha), input$n-1, 0), length.out = 400)
    
    plot(x = grid_x,
         y = dt(grid_x, input$n-1, 0),
         type = "l",
         xlim = c(lim_a, lim_b),
         ylim = c(0, 1.2*max(dt(grid_x,input$n-1,0))),
         ylab = "P(T = t)",
         xlab = "t")
    
    text(0, 1.1*max(dt(grid_x, input$n-1, 0)), "H0")
    text(effect, 1.1*max(dt(grid_x, input$n-1, effect)), "Ha")
    text(effect, .5*max(dt(grid_x, input$n-1, effect)), "Power")
    polygon(x = c(grid_x1, rev(grid_x1)),
            y = c(dt(grid_x1, input$n-1, 0), rep(0, length(grid_x1))),
            col = rgb(.2, .8, .2, alpha = .4),
            border = "black")
    lines(x = grid_x,
          y = dt(grid_x, input$n-1, effect))
    polygon(x = c(grid_x1, rev(grid_x1)),
            y = c(dt(grid_x1, input$n-1, effect), rep(0, length(grid_x1))),
            col = rgb(.1, .1, .9, alpha = .2))
    polygon(x = c(grid_x2, rev(grid_x2)),
            y = c(dt(grid_x2, input$n-1, effect), rep(0, length(grid_x2))),
            col = rgb(.9, .1, .1, alpha = .2))
    abline(v = qt((1 - input$alpha), input$n-1, 0),
           col = rgb(1,0,0),
           lty = 2)
    text(1.1*qt((1 - input$alpha), input$n-1, 0),
         .5*dt(qt((1 - input$alpha), input$n-1, 0), input$n-1,0 ),
         expression(paste(alpha)))
    text(.8*qt((1 - input$alpha), input$n-1, 0),
         .4*dt(qt((1 - input$alpha), input$n-1, 0), input$n-1,effect ),
         expression(paste(beta)))
    
  })
  output$power_plot2 <- renderPlot({
    effect <- input$delta/(input$sigma/sqrt(input$n)) # Effect Size
    n_grid <- 2:200
    plot(x = n_grid, 
         y = pt(qt((1-input$alpha),n_grid-1,0),
                n_grid-1,
                (input$delta/(input$sigma/sqrt(n_grid))), lower.tail = FALSE),
         type = "l",
         ylim = c(0, 1))
    points(input$n, 
           pt(qt((1-input$alpha),input$n-1,0), input$n-1, effect, lower.tail = FALSE),
           pch = 20,
           col = rgb(.7, .1, .1))
  })
    output$pwr_tbl <- renderTable({
      effect <- input$delta/(input$sigma/sqrt(input$n)) # Effect Size
      pwr <- pt(qt((1-input$alpha), input$n-1,0),
                input$n-1,
                (input$delta/(input$sigma/sqrt(input$n))), lower.tail = FALSE)
      tb1 <- cbind(c("Sigma", "Delta", "Effect","Sample Size", "ncp", "Alpha", "Power"),
                   c(input$sigma, input$delta, input$delta/input$sigma, input$n, effect, input$alpha, pwr))
      tb1 <- as.table(tb1)
      tb1
    }, include.rownames = FALSE, include.colnames = FALSE)
})
