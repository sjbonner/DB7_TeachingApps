#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(mvtnorm)
library(shiny)
library(ggplot2)
library(rgl)
library(patchwork)



myx <- seq(-4,4,0.2)
mymat <- expand.grid(x = myx, y = myx)


userMatrix <- matrix(c(0.86,0.4,-0.3,0, -0.5,0.65,-0.57,0, 0,0.65,0.75,0, 0,0,0,1), ncol = 4)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(
        ".irs-bar {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}",
        ".irs-bar-edge {",
        "  border-color: transparent;",
        "  background-color: transparent;",
        "}"
    ),
    
    # Application title
    titlePanel("Conditional Distributions from Multivariate Normal"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                "x",
                min = -4,
                max = 4,
                value = 2, step = 0.5),
            sliderInput("y",
                "y",
                min = -4,
                max = 4,
                value = -2, step = 0.5),
            sliderInput("rho",
                "correlation",
                min = 0,
                max = 1,
                value = 0.6, step = 0.05)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            rglwidgetOutput("distPlot", height = "400px"),
            plotOutput("plot2", height = "200px")
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    
    z2 <- reactive({
        sigma <- matrix(c(2, input$rho*2, input$rho*2, 2), ncol = 2)
        z <- dmvnorm(mymat, sigma = sigma)
        z2 <- matrix(z, ncol = length(myx), byrow = FALSE)
    })
    
    scene1 <- reactive({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        cols <- cut(z2, breaks = 20, labels = FALSE)
        
        persp3d(myx, myx, z2, col = terrain.colors(max(cols))[cols], 
            xlab = "x", ylab = "y", zlab = "z")
        lines3d(x = myx[ind], y = myx, z = z2[ind,], lwd = 3, col = "blue")
        lines3d(x = myx, y = myx[indy], z = z2[,indy], lwd = 3, col = "darkorchid")
        
        save <- options(rgl.inShiny = TRUE)
        on.exit(options(save))
        
        par3d(userMatrix = userMatrix)
        
        scene1a <- scene3d()
        rgl.close()
        return(scene1a)
    })
    
    output$distPlot <- renderRglwidget({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        
        scene1 <- scene1()
        
        rglwidget(scene1)
    })
    
    output$plot2 <- renderPlot({
        ind <- min(which(myx >= input$x))
        indy <- min(which(myx >= input$y))
        z2 <- z2()
        
        gg1 <- ggplot() + geom_line(aes(x = myx, y = z2[ind,]), 
            size = 1, colour = "blue") + 
            labs(title = "Y | X", x = "y", y = paste0("f(y | x = ", ind)) + 
            coord_cartesian(ylim = c(0, max(z2))) + 
            theme_bw()
        gg2 <- ggplot() + geom_line(aes(x = myx, y = z2[,indy]), 
            size = 1, colour = "darkorchid") +
            labs(title = "X | Y", x = "x", y = paste0("f(x | y = ", indy)) + 
            coord_cartesian(ylim = c(0, max(z2))) + 
            theme_bw()
        
        gg1 + gg2
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
