library(shiny)
library(ggplot2)
library(gridExtra)
library(randomcoloR)
library(plotrix)

#finds the nth iteration of the generating function
gnx <- function(x, n, param, geom_rv){
    if (n!=0){
        for(i in 1:n){
            x = (param*geom_rv)/(1-(1-geom_rv)*x)
        }
    }
    return(x)
}

#draws time histogram and line graph
histline <- function(hist, line, col1, col2, title, xlabel){
    df = data.frame(x=hist)
    df2 = data.frame(y=line, x=0:(length(line)-1))
    ggplot(df, aes(x=x))+ theme_minimal()+ geom_histogram(colour = "white", binwidth=1, fill=col1) +
        geom_line(data = df2, aes(x=x, y=y), size=1, colour=col2)+
        xlab(xlabel) + ylab("Frequency")+ ggtitle(title)+
        theme(plot.title = element_text(size=18, face="bold"))
    
}

#draws population histogram
bin <- function(toplot, col1, xaxis,title){
    df = data.frame(x=toplot)
    ggplot(df, aes(x=x))+ theme_minimal() + geom_histogram(colour = col1, binwidth=1, fill=col1)+
        xlab(xaxis) + ylab("Frequency")+ ggtitle(title)+ theme(
            plot.title = element_text(size=18, face="bold"),
            #axis.title.x = element_text(color=x$colours$col3, size=14, face="bold"),
            #axis.title.y = element_text(color="#993333", size=14, face="bold")
        )
}

#defines user interface
ui <- fluidPage(
    
    #title
    titlePanel("Branching Process Simulation"),
    
    #creates sidebar
    sidebarLayout(
        sidebarPanel(
            numericInput("repeats",
                         "Number of repeats:",value=100, min=1, max=100000),
            
            numericInput("seeds",
                         "Set seed (0 for random):",value=0, min=0, max = 1000000000),
            
            sliderInput("maxiterations",
                        "Maximum number of iterations:",
                        min = 1,
                        max = 20,
                        value = 10),
            
            sliderInput("geom_rv",
                        "Geometric parameter:",
                        min = 0.01,
                        max = 1,
                        value = 0.35),
            
            htmlOutput("mean"),
            
            sliderInput("param",
                        "Detection probability:",
                        min = 0,
                        max = 0.99,
                        value = 0.05),
            
            #regenerate button
            actionButton("regenerate", "Re-generate", icon = icon("sync"), 
                         style=paste("color: #fff; background-color: ", 
                                     randomColor(1, luminosity = "dark"), "; border-color: #ffffff;", sep="")),
            
        ), 
        
        
        mainPanel(
            
            #tabs
            tabsetPanel(
                
                tabPanel("Line Graph",
                         splitLayout(sliderInput("a","Transparency:", min = 0,max = 1,value = 0.6),
                                     htmlOutput("proportion"),
                                     cellArgs = list(style='white-space: normal;')
                         ),
                         plotOutput("linegraph", height="150mm")
                ),
                
                tabPanel("Population/time at detection",
                         htmlOutput("det_text"),
                         splitLayout(cellWidths=c("80%", "20%"),
                                     plotOutput("det1"),
                                     htmlOutput("det_text_2"),
                                     cellArgs = list(style='white-space: normal;')
                         ),
                         plotOutput("det2"),
                         plotOutput("det3")
                ),
                
                tabPanel("Population/time at extinction",
                         htmlOutput("ext_text"),
                         splitLayout(cellWidths=c("80%", "20%"),
                                     plotOutput("ext1"),
                                     htmlOutput("ext_text_2"),
                                     cellArgs = list(style='white-space: normal;')
                         ),
                         plotOutput("ext2"),
                         plotOutput("ext3")
                )
            )
        )
    )
)

#defines server function
server <- function(input, output) {
    
    #reactive expression
    y <- reactive({
        input$regenerate
        
        #hardcodes colours used
        col1 = "#FF0000"                               #detection secondary colour
        col2 = "#FFC0CB"                               #detection primary colour
        col3 = "blue"                                  #extinction secondary colour
        col4 = "lightblue"                             #extinction primary colour
        colours = list(col1, col2, col3, col4)
        names(colours) = c("col1", "col2", "col3", "col4")
        
        #initializes variables
        masterlist = list()
        max_height=0
        detection_times = extinction_times = status = c()
        detected_pops = extinction_pops = detected_tots = extinction_tots = c()
        
        #each time this loops is another random branching process
        for (i in 1:input$repeats){
            
            #sets seed
            if (input$seeds!=0){
                set.seed(paste(input$seeds,i,sep=""))
            }
            
            #initializes variables
            population = c(1)
            has_died_out = is_detected = iterations=0
            
            #checks if first node is detected or not
            r = runif(1)
            if (r<input$param){
                is_detected = 1
            }                            
            
            #finds population over time
            while (has_died_out == 0 & is_detected == 0 & iterations < input$maxiterations){
                current_pop = tail(population, 1)
                
                #finds population of next generation
                if (current_pop!=0){
                    
                    new_pop = sum(rgeom(current_pop, input$geom_rv))
                }
                
                population = c(population, new_pop)
                
                #finds maximum height
                if (new_pop>max_height){
                    max_height = new_pop
                }
                
                #determines if loop should end or not
                if (new_pop == 0){
                    has_died_out = 1
                }else{
                    is_detected = rbinom(size = tail(population, 1), n=1, prob = input$param)
                }
                
                
                iterations = iterations + 1
            }
            
            #appends populations/times to lists
            length = length(population)
            if (tail(population,1)==0){                                           #died out undetected
                extinction_times = c(extinction_times, length-2)
                extinction_pops = c(extinction_pops, tail(population, 2)[1])
                extinction_tots = c(extinction_tots, sum(population))
                status = c(status, 1) 
            }else if(is_detected!=0){                                             #died out detected
                detection_times = c(detection_times, length-1)
                detected_pops = c(detected_pops, tail(population, 1))
                detected_tots = c(detected_tots, sum(population))
                status = c(status, 2)
            }else{                                                                #hasn't died out yet
                status=c(status,0)
            }
            masterlist = c(masterlist, list(population))
        }
        
        #finds maximum time
        max_time = 0
        for (i in masterlist){
            if (length(i)>max_time){
                max_time = length(i)
            }
        }
        
        #list representing populations
        populations = list(detected_pops, detected_tots, extinction_pops, extinction_tots)
        names(populations) = c("detected_pops", "detected_tots", "extinction_pops", "extinction_tots")
        
        #returns values
        x=list(masterlist, max_time, max_height, detection_times, extinction_times, populations, status, colours)
        names(x) = c("masterlist", "max_time", "max_height", "detection_times", "extinction_times", "populations", 
                     "status", "colours")
        x
        
    })
    
    #line graph
    output$linegraph <- renderPlot({
        x<-y()
        
        #plots line graph
        plot(0,0, type = "n", xlim = c(0, x$max_time-1), ylim = c(0, x$max_height), xlab="Time", ylab ="Population")
        cl = rainbow(length(x$masterlist), alpha=input$a)
        for (i in 1:length(x$masterlist)){
            lines(seq(0,length(x$masterlist[[i]])-1), x$masterlist[[i]], col=cl[i])
        }
        
    })
    
    #displays mean
    output$mean <- renderUI({
        x<-y()
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; the mean is ",
                      round((1-input$geom_rv)/input$geom_rv,3))
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    
    #displays detected/undetected proportion
    output$proportion <- renderUI({
        
        x<-y()
        
        str3 <- paste("The rest are still alive as of the maximum time.", sep="")
        str2 <- paste(length(x$extinction_times), " died out due to their populations hitting zero, undetected.", sep="")
        str1 <- paste(length(x$detection_times), " of the ", input$repeats, " repeats got detected. ", sep = "")
        str0 <- paste("&nbsp;", "&nbsp;", sep = '<br/>')
        HTML(paste(str0, str1, str2, str3, sep = '<br/>'))
        
    })
    
    #explains histograms
    output$det_text <- renderUI({
        
        x<-y()
        if (length(x$detection_times)==0){
            HTML(paste("&nbsp; <br/> <p style='color:pink;'>None of the trees ever got detected! Try increasing
                       the detection probability.</p>"))
        }else{
            HTML(paste("&nbsp; <br/> "))
            HTML(paste("&nbsp; <br/> <p style='font-size:150%;'>Here we are looking only at the trees in which
            a detection event occured before the population hit zero.</p>"))
        }
        
    })
    output$ext_text <- renderUI({
        
        x<-y()
        if (length(x$extinction_times)==0){
            HTML(paste("&nbsp; <br/> <p style='color:blue;'>None of the trees ever reached zero population!
                       Try decreasing the geometric parameter or increasing the number of repeats</p>"))
        }else{
            HTML(paste("&nbsp; <br/> <p style='font-size:150%;'>Here we are looking only at the trees for which 
            the population hit zero before a detection event occured.</p>"))
            
        }
        
    })
    output$det_text_2 <- renderUI({
        
        x<-y()
        
        if (length(x$detection_times)!=0){
            HTML(paste("<br/><br/><br/><br/><br/> 
                   The ", color.id(x$colours$col1)[1]," line shows what we would expect to see. 
                   The",color.id(x$colours$col2)[1]," histogram is our simulated results."))
        }
        
    })
    output$ext_text_2 <- renderUI({
        
        x<-y()
        
        if (length(x$extinction_times)!=0){
            HTML(paste("<br/><br/><br/><br/><br/> 
                   The ", color.id(x$colours$col3)[1], " line shows what we would expect to see. 
                   The ", color.id(x$colours$col4)[1], " histogram is our simulated results"))
        }
        
    })
    
    #plots time detected
    output$det1 <- renderPlot({
        x<-y()
        
        if (length(x$detection_times)!=0){
            
            temp = c(0,seq(x$max_time-1))
            
            det_predict = c()
            for(i in temp){
                det_predict = c(det_predict, input$repeats*(gnx(1,i, 1-input$param, input$geom_rv)-gnx(1-input$param,i, 1-input$param, input$geom_rv)))
            }
            
            #plots histograms for detection and non-detection
            plot = histline(x$detection_times,det_predict,x$colours$col2, x$colours$col1, "Time of detection", "Generation detected")
            
            grid.arrange(plot)
            
        }
        
    })
    
    #plots generation detected
    output$det2 <- renderPlot({
        x<-y()
        
        if (length(x$detection_times)!=0){
            
            plot1 = bin(x$populations$detected_pops,  x$colours$col2, "Population on detection", "Population on detection")
            plot2 = bin(x$populations$detected_tots, x$colours$col1, "Total Infected", "")
            
            grid.arrange(plot1, plot2, ncol=2)
        }
        
        
    })
    
    #plots generation/time detected
    output$det3 <- renderPlot({
        
        x<-y()
        
        if (length(x$detection_times)!=0){
            
            #initializes variables
            dots = c()
            times = c()
            
            #creates vectors for times and populations - separated by detected/undetected points
            #excluding the trees that haven't died out yet
            for (i in 1:length(x$masterlist)){
                if (x$status[i] == 2){
                    dots = c(dots, tail(x$masterlist[[i]],1))
                    times = c(times, length(x$masterlist[[i]])-0.9)
                }
            }
            
            df <- data.frame(x = times,y=dots)
            plot = ggplot(df,aes(x=x,y=y))+ theme_minimal() + geom_bin2d(binwidth=1) + 
                scale_fill_gradient(low=x$colours$col2,high=x$colours$col1,trans="log10")+
                xlab("Time on detection") + ylab("Population on detection") + ggtitle("Time/Population at detection")+ theme(
                    plot.title = element_text(size=18, face="bold"))
            
            grid.arrange(plot)
            
        }
        
        
    })
    
    #plots time died out
    output$ext1 <- renderPlot({
        x<-y()
        
        if (length(x$extinction_times)!=0){
            
            temp = c(0,seq(x$max_time-1))
            
            #calculates predicted values
            nodet_predict = c()
            for(i in temp){
                nodet_predict = c(nodet_predict, input$repeats*(gnx(0,i+1, 1-input$param, input$geom_rv)-gnx(0,i, 1-input$param, input$geom_rv)))
            }
            
            #plots histograms for detection and non-detection
            plot = histline(x$extinction_times, nodet_predict,x$colours$col4, x$colours$col3, "Time of death", "Generation died out")
            
            #graphs only show up if they have at least one point to plot
            grid.arrange(plot)
        }
        
    })
    
    #plots generation died out
    output$ext2 <- renderPlot({
        x<-y()
        
        if (length(x$extinction_times)!=0){
            plot3 = bin(x$populations$extinction_pops,  x$colours$col4, "Population on death", "Population on death")
            plot4 = bin(x$populations$extinction_tots, x$colours$col3, "Total Infected", "")
            
            grid.arrange(plot3, plot4, ncol=2) 
        }
        
    })
    
    #plots generation/time died out
    output$ext3 <- renderPlot({
        x<-y()
        
        if (length(x$extinction_times)!=0){
            
            #initializes variables
            dots = c()
            times = c()
            
            #creates vectors for times and populations - separated by detected/undetected points
            #excluding the trees that haven't died out yet
            for (i in 1:length(x$masterlist)){
                if (x$status[i] == 1){
                    dots = c(dots, tail(x$masterlist[[i]],2)[1])
                    times = c(times, length(x$masterlist[[i]])-1.9)
                }
            }
            
            df <- data.frame(x = times,y=dots)
            plot1 = ggplot(df,aes(x=x,y=y))+ theme_minimal() + geom_bin2d(binwidth=1) + 
                scale_fill_gradient(low=x$colours$col4,high=x$colours$col3,trans="log10")+
                xlab("Time on death") + ylab("Population on death") + ggtitle("Time/Population on death")+ theme(
                    plot.title = element_text(size=18, face="bold"))
            
            #only draws plot if there is at least one point
            grid.arrange(plot1)
            
        }
        
        
    })
    
}

#runs the application 
shinyApp(ui = ui, server = server)