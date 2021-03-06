# pvalues

library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Visualization of p-values"),
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
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("zobs",
                "Zobs value",
                min = -4,
                max = 4,
                value = 1.96,
                step = 0.05),
            radioButtons(inputId = "alt", label = "Alternative", 
                choices = c("Less Than", "Greater Than", "Not Equals"), 
                selected = "Less Than"),
            checkboxInput("correct2", "Use absolute value for 2-sided", value = FALSE)
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            tags$div(HTML("Some questions:
                <ol><li>What happens to the p-value when you switch from less than to greater than?</li>
                <ol><li>What happens to the p-value when you switch from less than to two-sided?</li>
                <ol><li>When zobs < 0, why is the p-value larger than 1 if 'Use absolute value' is not checked?</li>
                
                </ol"))
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        zobs <- input$zobs
        
        xseq <- seq(-4,4,0.01)
        yseq <- dnorm(xseq)
        
        if(input$alt == "Less Than"){
            xrib1 <- seq(-4, zobs, 0.01)
            yrib1 <- dnorm(xrib1)
            xrib2 <- numeric(0)
            yrib2 <- numeric(0)
            roundp <- round(pnorm(zobs), 3)
            mytitle <- paste0("p-value is ", roundp)
            addtox <- c(zobs)
        } else if(input$alt == "Greater Than"){
            xrib1 <- seq(zobs, 4, 0.01)
            yrib1 <- dnorm(xrib1)
            xrib2 <- numeric(0)
            yrib2 <- numeric(0)
            roundp <- round(1 - pnorm(zobs), 3)
            mytitle <- paste0("p-value is ", roundp)
            addtox <- c(zobs)
        } else {
            zobs1 <- zobs
            if(input$correct2) zobs1 <- abs(zobs)
            xrib1 <- seq(-4, -zobs1, 0.01)
            yrib1 <- dnorm(xrib1)
            xrib2 <- seq(zobs1, 4, 0.01)
            yrib2 <- dnorm(xrib2)
            roundp <- round(pnorm(-zobs1) + 1 - pnorm(zobs1), 3)
            mytitle <- paste0("p-value is ", roundp)
            if(roundp >= 1) mytitle <- paste0(mytitle, ", but that probably isn't right.")
            addtox <- c(-zobs, zobs)
        }
        
        
        
        ggplot() + 
            geom_ribbon(aes(xmin = xrib1, x=xrib1, ymin = 0, ymax = yrib1),
                fill = "darkorchid", alpha = 0.4, colour = 1)+
            geom_line(aes(x = xseq, y = yseq), size = 1) +
            #scale_x_continuous(breaks = c(seq(-4,4,2), addtox)) +
            theme_bw() + 
            annotate("text", x = addtox, y = rep(-0.01, length(addtox)), 
                label = addtox) +
            labs(x = "x", y = "dnorm(x)", title = mytitle,
                caption = "Created by Devan Becker\nGithub: DBecker7/DB7_TeachingApps") + 
            theme(title = element_text(size = 16), 
                axis.text = element_text(size = 14))  +
            if(input$alt == "Not Equals") {
                geom_ribbon(aes(xmin = xrib2, x=xrib2, ymin = 0, ymax = yrib2),
                    fill = "darkorchid", alpha = 0.4, colour = 1)
            } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
