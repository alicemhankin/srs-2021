library(shiny)
library(TurtleGraphics)
library(ggplot2)
library(gridExtra)
library(gtable)
library(randomcoloR)
library(stringr)
library(expm)

#finds long term proportion of infectious to non-infectious given starting state
findmatrix <- function(parameter, vect, p , q){
    
    a = matrix(nrow=2, ncol=2)
    a[1,][1] = 1-p
    a[2,][1] = p
    a[2,][2] = 1/(1+q)
    a[1,][2] = parameter/(1+q) 
    
    a = a%^%(100)  #I hope this is right?? There is likely a much better way to do this. 100 was picked at random
    a = a %*% vect
    return(a)
}

#defines user interface
ui <- fluidPage(
    
    #title
    titlePanel("Multi-Type Branching Process"),
    
    #sidebar
    sidebarLayout(
        sidebarPanel(
            
            numericInput("seeds",
                         "Input seed (0 for random):",
                         min=0, value = 2),
            
            numericInput("flip",
                         "After how many generations does the parameter change?:",
                         min=1, value = 20),
            
            numericInput("par_before",
                        "Geometric mean for spread of infection (before):",
                        min=1, value = 2.5),
            
            htmlOutput("text2"),
            
            numericInput("par_after",
                        "Geometric mean for spread of infection (after):",
                        min=0, max = 1, value = 0.3),
            
            htmlOutput("text3"),
            
            sliderInput("par_p",
                        "Probability of non-infectious becoming infectious:",
                        min = 0,
                        max = 1,
                        value = 1/3),
            
            sliderInput("par_q",
                        "Probability of infectious dying:",
                        min = 0,
                        max = 1,
                        value = 1/3),
            
            numericInput("iterations",
                        "Maximum iterations:",
                        min = 1,
                        max = 1000,
                        value = 200),
            
            #regenerate button
            actionButton("regenerate", "Re-generate", icon = icon("sync"), style=paste
                         ("color: #fff; background-color: ", randomColor(1, luminosity = "dark"),
                         "; border-color: #ffffff;", sep="")),
            
        ),
        
        mainPanel(plotOutput("plot1"), htmlOutput("text4"))
    )
)

#defines server logic
server <- function(input, output) {
    
    #reactive expression
    y <- reactive({
        input$regenerate
        
        #sets colours
        colA = "turquoise"          #non-infectious colour
        colB = "darksalmon"         #infectious colour
        
        #initializes variables
        strlist = c("A")
        end=0
        iteration = 1
        new_r = input$par_before
        
        #sets seed
        if (input$seeds!=0){
            set.seed(input$seeds)
        }
        
        #repeats for every generation up to maximum as long as population doesn't hit zero
        while (iteration<input$iterations & end!=1){
            
            #changes parameter (if needed)
            if (iteration == input$flip){
                new_r = input$par_after
            }
            
            newstring = ""
            end=1
            
            #for every existing node, determine what happens in the next generation and records in list
            for (i in strsplit(tail(strlist,1), "")[[1]]){
                
                r = runif(1)
                if(i == "A"){
                    if(r < input$par_p){
                        newstring = paste(newstring, "B", sep = "")
                        end=0
                    }else{
                        newstring = paste(newstring, "A", sep = "")
                        end=0
                    }
                }else{
                    
                    if (r > input$par_q){
                        var = rgeom(1,1/(1+new_r))
                        x = paste(c("B", replicate(var, "A")), collapse = "")
                        newstring = paste(newstring, x, sep = "")
                        end = 0
                    }
                }
            }
            strlist = c(strlist, newstring)
            iteration=iteration+1
        } 
        
        #finds total number of generations
        tot = length(strlist)
    
        #returns relevant values as list
        x = list(tot, strlist, colA, colB)
        names(x) = c("tot", "strlist", "colA", "colB")
        return(x)
    })
    
    #plots population over time
    output$plot1 <- renderPlot({
        x<-y()
        
        strlist = x$strlist
        tot = x$tot
        notinf = c()
        inf = c()
        
        #removes empty space if needed
        if(strlist[length(strlist)]==""){
            strlist = strlist[-length(strlist)]
            tot = tot-1
        }
        
        #finds infectious/non-infectious populations each generation
        for (i in 1:length(strlist)){
            a = 0
            b = 0 
            strlist_split = strsplit(strlist[i], "")
            for (j in strlist_split[[1]]){
                if (j =="A"){
                    a = a+1
                }else{
                    b=b+1
                }
                
            }
            notinf = c(notinf,a)
            inf = c(inf,b)
        }
        
        #creates dataframe and plots data
        df = data.frame(Time = rep(seq(tot),2), Population = c(notinf, inf), 
                        Type = c(rep("Non-Infectious",length(notinf)), rep("Infectious",length(inf))))
        ggplot(df,aes(x=Time, y=Population))+ theme_minimal() +
            geom_col(aes(fill=Type), width=0.7) + scale_fill_manual(values = c(x$colB, x$colA))
        
        
    })
    
    #outputs geometric parameter (for before the change) given the mean
    output$text2 <- renderUI({

        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; geometric parameter of ~",
                      round(1/(1+input$par_before),3), sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))

    })
    
    #outputs geometric parameter (for after the change) given the mean
    output$text3 <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; geometric parameter of ~",
                      round(1/(1+input$par_after),3), sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    
    #outputs the expected vs simulated proportions of infectious to non-infectious
    output$text4 <- renderUI({
        x<-y()

        #finds what we'd expect before the change
        vect = matrix(nrow=2, ncol=1, c(1,0))
        a = findmatrix(input$par_before, vect, input$par_p, input$par_q)
        
        #finds what we actually got before the change
        numA = numB = 0
        which = min(input$flip, length(x$strlist))
        for(i in 1:which){
            for (j in 1:nchar(x$strlist[i])){
                if (substring(x$strlist[i],j,j)=="A"){
                    numA = numA + 1
                }else if (substring(x$strlist[i],j,j)=="B"){
                    numB = numB + 1
                }
            }
        }
        
        #string containing expected and simulated results
        str1<- paste("The expected percentage of infectious individuals <b>before</b> the parameter change
        is ", 100*round(a[2]/sum(a),3), "% and the simulated percentage is
                   ", 100*round(numB/(numA+numB),3), "%", sep="")
        
        #adds string with results for post change of variables only if necessary
        if (which < length(x$strlist)){
            
            #what we'd expect after the change
            vect = matrix(nrow=2, ncol=1, c(a[1],a[2]))
            b = findmatrix(input$par_after, vect, input$par_p, input$par_q)
            
            #what we actually got after the change
            numA = numB = 0
            for(i in which:length(x$strlist)){
                for (j in 1:nchar(x$strlist[i])){
                    if (substring(x$strlist[i],j,j)=="A"){
                        numA = numA + 1
                    }else if (substring(x$strlist[i],j,j)=="B"){
                        numB = numB + 1
                    }
                }
            }
            
            #string containing expected and simulated results
            str2 = paste("The expected percentage of infectious individuals <b>after</b> the parameter
                         change is ",
                         100*round(b[2]/sum(b),3), "% and the simulated percentage is ",
                         100*round(numB/(numA+numB),3), "%", sep="")
        }else{
            str2 = ""
        }
        
        #outputs text as html
        HTML(paste(str1, str2, sep="<br/><br/>"))
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
