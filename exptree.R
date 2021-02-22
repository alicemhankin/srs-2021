library(shiny)
require(randomcoloR)

#defines UI
ui <- fluidPage(
    
    titlePanel("~Continuous Time"),
    
    #creates sidebar 
    sidebarLayout(
        sidebarPanel(
            
            numericInput("seeds",
                         "Select seed (0 for random):",
                         min = 0,
                         max = 10000,
                         value = 8),
            
            sliderInput("par_q",
                        "Probability nothing happens (in one generation):",
                        min = 0,
                        max = 1,
                        value = 0.95),

            
            sliderInput("par_r",
                        "Birth event parameter (geometric):",
                        min = 0,
                        max = 1,
                        value = 0.5),
            
            htmlOutput("text"),
            
            numericInput("maxiteration",
                        "Maximum number of iterations (keep very low in supercritical case!):",
                        min = 1,
                        value = 500),
            
            #actionButton("regenerate", "Re-generate", icon = icon("sync"), style="color: #fff; 
            #background-color: #28a745; border-color: #28a745;"),
            
            actionButton("regenerate", "Re-generate", icon = icon("sync"), style=paste("color: 
            #fff; background-color: ", randomColor(1, luminosity = "dark"), "; border-color: #ffffff;", sep="")),
            
        ),
        
        #creates tabs and plots
        mainPanel(plotOutput("plot"))
        
    )
)

# Defines server logic
server <- function(input, output) {
    
    #reactive expression
    y <- reactive({
        input$regenerate
        
        #initializes variables
        pop_over_time = c(1)
        end = 0
        iteration = 1
        
        #sets seed
        if (input$seeds!=0){
            set.seed(input$seeds)
        }
        
        #loop for each new generation
        while (end == 0 & iteration<input$maxiteration){
            
            parents = tail(pop_over_time, 1)
            current_pop = 0
            
            if (parents != 0){
                
                #for each individual in current generation, determine the number of offspring
                for (i in 1:parents){
                    
                    r = runif(1)
                    if (r < input$par_q){
                        offspring = 1
                    }else{
                        offspring = rgeom(1,input$par_r)
                    }
                    
                    current_pop=current_pop + offspring
                }
            }
            
            #Ends if population hits zero
            pop_over_time = c(pop_over_time, current_pop)
            if (tail(pop_over_time, 1) == 0){
                end = 1
            }

            iteration = iteration + 1
        }        
        
        #returns values
        return(pop_over_time)
        
    })
    
    #line graph
    output$plot <- renderPlot({
        x<-y()
        
        col = "coral2"
        #col = randomColor(count=1, luminosity = "dark") #https://github.com/rstudio/shiny/issues/3289
        
        #plots line graph
        plot(0:(length(x)-1),x, col=col, type='l', ylab="Population", xlab="Time")
        
    })
    
    #outputs birth event mean
    output$text <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; given there's a birth event, the average number of offspring is ",
                      round((1-input$par_r)/input$par_r,4))
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
        
}

#runs the application 
shinyApp(ui = ui, server = server)
