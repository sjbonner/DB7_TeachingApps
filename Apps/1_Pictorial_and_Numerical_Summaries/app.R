# MeasureCenter

library(shiny)
library(dplyr)
library(ggplot2)


# as a function
perturb <- function(x, skew){
  if(skew ==0)
    return(x)
  
  if(skew < 0)
    return(x * (abs(skew)^(x < 0)))
  
  else
    return(x * skew^(x > 0))
}

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
  titlePanel("Measures of Location and Variability"),
  
                                        # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("seed",
                   "Random seed:",
                   value = as.integer(runif(1,0,1e7))),
      sliderInput("n",
                  "n:",
                  min = 50,
                  max = 500,
                  value = 100,
                  step = 10),
      sliderInput("skew",
                  "Skew:",
                  min = -5,
                  max = 5,
                  value = 0,
                  step = 0.25,
                  animate = list(interval = 700)),
      sliderInput("outliers",
                  "Outliers:",
                  min = 0,
                  max = 10,
                  value = 0,
                  step = 1,
                  animate = list(interval = 10)),
      sliderInput("outlierseverity",
                  "Outlier severity:",
                  min = 0,
                  max = 30,
                  value = 10,
                  step = .25,
                  animate = list(interval = 100)),
      checkboxGroupInput("elements",
                         "Output Elements:",
                         choices = c("Histogram" = "histogram",
                           "Boxplot" = "boxplot",
                           "Measures of Location" = "center",
                           "Measures of Spread" = "spread",
                           "Five Number Summary" = "fivenum"),
                         selected = "histogram"),
      submitButton("Update View", icon("refresh"))
    ),
        
    ## Show a plot of the generated distribution
    mainPanel(
      tags$div(HTML("This app allows you to explore how pictorial and numerical summaries of a unimodal distribution change as you modify the characteristics of the distribution. Variables you can control include the size of the sample (n), the degree of skewness, and the number of outliers added into the distribution and their severity. Changing the random seed will generate different samples. You may use the checkboxes to select which output to display. <br/><br/>Try to guess how the summaries will change before you hit the update button!")),
      plotOutput("distPlot", height = "250px"),
      plotOutput("boxPlot", height = "250px"),
      htmlOutput("summText"),
      tags$div(HTML("Created by Simon Bonner based on code by Devan Becker.")),
      tags$div(HTML("GitHub: <a href =\"https://github.com/sjbonner/DB7_TeachingApps\" target =\"_blank\">sjbonner/DB7_TeachingApps<a/>."))
          ## <ol>
          ##     <li>Set sd = 1. What value of the IQR makes this look the most 'Normal'?</li>
          ##     <li>Calculate the IQR of a theoretical normal distribution.</li>
          ##     <li>For these simulations, I made Q2 = 0, Q1 = Q2 - IQR/2, and Q3 = Q2 + IQR/2. Is the mean restricted to be 0?</li>
          ##     <li>What's the theoretical lower limit on the variance for a given IQR, such that Q1 = -Q3 and Q2 = 0? Hint: imagine you have 10 values below Q1, 10 between Q1 and Q2, etc., then rearrange them to be closest to 0.</li>
          ## </ol>"))
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Create data
  getdata <<- reactive({
    set.seed(input$seed)

    half_x <- rnorm(input$n/2, 0, 1)
    outliers <- rnorm(input$outliers, input$outlierseverity * ifelse(input$skew < 0, -1, 1),1)
    
    tibble(x=c(half_x, -half_x, outliers)) %>%
      mutate(x = perturb(x,input$skew))
  })

  output$distPlot <- renderPlot({

    if(!("histogram" %in% input$elements))
      return(NULL)

    mydata <- getdata()
    
    bw <- ifelse(input$n > 100, 0.25, 0.75)
    lim <- bw * ceiling(max(abs(mydata$x))/bw)
    breakseq <- seq(-lim - bw, lim + bw, bw)
        
        meanx <- mean(mydata$x)
        medianx <- median(mydata$x)
        height <- max(table(cut(mydata$x, breakseq)))
        
        ggplot(mydata, aes(x = x)) + 
          geom_histogram(breaks = breakseq) +
          ylim(c(0, 1.2 * height)) +
          xlim(c(-lim - bw, lim + bw)) + 
          ylab("Frequency") + 
          annotate("segment", x = c(meanx, medianx), y = c(0,0),
                   xend = c(meanx, medianx), yend = c(height, height),
                   size = 1, colour = c(4,2)) +
          annotate("text", x = medianx, y = 1.15 * height, label = "Median", 
                   hjust = .5, colour = 2) + 
          annotate("text", x = meanx, y = 1.05 * height, label = "Mean", 
                   hjust = .5, colour = 4) 

        ## if(input$dens) lines(density(x), col = 2, lwd = 2)
        
        ## mx <- mean(x); sx <- sd(x)
        ## if(input$norm) curve(dnorm(x, mean = mx, sd = sx), 
        ##     add = TRUE, col = 3, lwd = 2)
        ## q <- c(q1(x), q2(x), q3(x))
        ## abline(v = q, col = 4, lwd = 2)
        ## axis(1, at = q, labels = c("Q1", "Q2", "Q3"), col.axis = 4)
  })

  output$boxPlot <- renderPlot({
    if(!("boxplot" %in% input$elements))
      return(NULL)
    
    mydata <- getdata()

    bw <- ifelse(input$n > 100, 0.25, 0.75)
    lim <- bw * ceiling(max(abs(mydata$x))/bw)
        
    ggplot(mydata, aes(x=x)) +
      geom_boxplot() +
      ylab("") + 
      xlim(c(-lim - bw, lim + bw))
  })

  output$summText <- renderUI({
    mydata <- getdata()

    html <- ""
    
    if("center" %in% input$elements){
      str0 <- "Measures of Location"
      str1 <- paste("  Mean:", round(mean(mydata$x),2))
      str2 <- paste("  Median: ", round(median(mydata$x),2))
      str3 <- paste("  Trimmed Mean (5%): ", round(mean(mydata$x,.025),2))
      str4 <- paste("  Trimmed Mean (10%): ", round(mean(mydata$x,.05),2))
    
      html <- paste(html,str0,str1, str2, str3, str4,"<br/><br/>", sep = "<br/>")
    }

    if("spread" %in% input$elements){
      str0 <- "Measures of Spread"
      str1 <- paste("  Variance:", round(var(mydata$x),2))
      str2 <- paste("  Standard Deviation: ", round(sd(mydata$x),2))
      str3 <- paste("  Fourth Spread (IQR): ", round(c(-1,1) %*% quantile(mydata$x,c(.25,.75)),2))
    
      html <- paste(html,str0,str1, str2, str3, "<br/><br/>" ,sep = "<br/>")
    }

    if("fivenum" %in% input$elements){
      str0 <- "Five Number Summary"
      str1 <- paste("  Minimum:", round(min(mydata$x),2))
      str2 <- paste("  First quartile:", round(quantile(mydata$x,.25),2))
      str3 <- paste("  Median:", round(median(mydata$x),2))
      str4 <- paste("  Third quartile:", round(quantile(mydata$x,.75),2))
      str5 <- paste("  Maximum:", round(max(mydata$x),2))
    
      html <- paste(html,str0,str1, str2, str3, str4, str5,sep = "<br/>")
    }
     
    HTML(html)     
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
