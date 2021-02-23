library(shiny)
library(TurtleGraphics)
library(ggplot2)
library(gridExtra)
library(gtable)
library(randomcoloR)
library(stringr)
library(expm)

#draws one node
draw_point <- function(colour){
    turtle_col(colour)
    
    sides = 8
    lps = 0.3
    ang = 360/sides
    l = (lps/2)/(tan((pi*ang/180)/2))
    
    turtle_lwd(5)
    
    turtle_setangle(0)
    turtle_up()
    turtle_backward(l)
    turtle_down()
    turtle_setangle(270)
    
    #turtle_col("purple")
    for (i in 1:sides) {
        turtle_forward(lps)
        turtle_right(360/sides)
    }
    turtle_col("black")
    
    turtle_setangle(0)
    turtle_up()
    turtle_forward(l)
    turtle_down()
    
    turtle_lwd(1)
}

#determines angle for next generation given angle for this generation
angle_func <- function(n, angle){
    if(n==2){
        return(0.8*angle)
    }else{
        return(2*angle/n)   
    }
    
}

#defines user interface
ui <- fluidPage(
    
    #title
    titlePanel("Multi-Type Branching Process"),
    
    #creates sidebar
    sidebarLayout(
        sidebarPanel(
            
            numericInput("seeds",
                         "Input seed (0 for random):",
                         min=0, value = 2),
            
            numericInput("flip",
                         "When to change parameters:",
                         min=1, value = 20),
            
            
            numericInput("par_before",
                        "Mean infection spread before parameter change:",
                        min=1,  value = 1.5),
            
            htmlOutput("text2"),
            
            numericInput("par_after",
                        "Mean infection spread after parameter change:",
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
                        min = 1, max=20,
                        value = 10),
            
            actionButton("regenerate", "Re-generate", icon = icon("sync"), style=
                             paste("color: #fff; background-color: ", randomColor(1, luminosity = "dark"), 
                                   "; border-color: #ffffff;", sep="")),
            
        ),
        
        mainPanel(tags$head(tags$style(HTML("#text {font-family:  'Source Sans Pro','Helvetica Neue',
                  Helvetica,Arial,sans-serif; font-size: 14px;}"))),
            
            verbatimTextOutput("text1"),
            
            #creates tabs
            tabsetPanel(id = "tabs",
                tabPanel("Shows all nodes", plotOutput("plot1")),
                tabPanel("Shows structure", plotOutput("plot2")),
                tabPanel("Population over time", plotOutput("plot3"))
            )
        )
    )
)

#defines server logic
server <- function(input, output) {
    
    #reactive expression
    y <- reactive({
        input$regenerate
        
        
        
        # unfinished!
        #  - first find out the expected population 
        #  - then hide or show the trees depending if we expect to see overprinting
        
        # if (max_pop < 50){
        #     showTab(inputId = "tabs", target = "Shows all nodes")
        #     showTab(inputId = "tabs", target = "Shows structure")
        # }else{
        #     hideTab(inputId = "tabs", target = "Shows all nodes")
        #     hideTab(inputId = "tabs", target = "Shows structure")
        # }
        
        
        
        #sets parameters
        flength = 20
        colA = "turquoise"          #non-infectious colour
        colB = "darksalmon"         #infectious colour
        
        #initializes variables
        iteration = 1
        new_r = input$par_before
        strlist = c("A")
        nums = list(c(1,0,"A"))
        end = 0
        
        #sets seed
        if (input$seeds!=0){
            set.seed(input$seeds)
        }
        
        #repeats for every generation up to maximum as long as population doesn't hit zero
        while (iteration<input$iterations & end!=1){
            
            #changes parameter
            if (iteration == input$flip){
                new_r = input$par_after
            }
            
            newstring = ""
            end = 1
            
            #for every existing node, determine what happens in the next generation
            for (i in strsplit(tail(strlist,1), "")[[1]]){
                
                r = runif(1)
                if(i == "A"){
                    if(r < input$par_p){
                        newstring = paste(newstring, "B", sep = "")  #newstring/strlist used to draw nodes
                        nums = c(nums, list(c(1,iteration,i)))       #nums used to draw connections between nodes
                        end = 0
                    }else{
                        newstring = paste(newstring, "A", sep = "")
                        nums = c(nums, list(c(1,iteration,i)))
                        end=0
                    }
                }else{
                    
                    if (r < input$par_q){
                        nums = c(nums, list(c(0,iteration,i)))
                    }else{
                        var = rgeom(1,1/(1+new_r))
                        x = paste(c("B", replicate(var, "A")), collapse = "")
                        newstring = paste(newstring, x, sep = "")
                        nums = c(nums, list(c(var+1,iteration,i)))
                        end=0
                    }
                }
            }
            strlist = c(strlist, newstring)
            iteration=iteration+1
        } 
        
        #ensures last generation have no offspring
        for (i in 1:nchar(strlist[length(strlist)])){
            nums = c(nums, list(c(0,iteration, substring(strlist[length(strlist)],i,i)))) 
        }
        
        #changes horizontal distance between nodes so it fits on screen
        tot = length(strlist)
        if (strlist[length(strlist)]!=""){
            flength = min((90)/(tot-1), flength)
        }else{
            flength = min((90)/(tot-2), flength)
        }
        
        #returns list
        x = list(nums, tot, flength, strlist, colA, colB)
        names(x) = c("nums", "tot", "flength", "strlist", "colA", "colB")
        return(x)
    })
    
    #outputs first tree
    output$plot1 <- renderPlot({
        x<-y()
        
        #initializes variables and turtle
        locs = list()
        turtle_init(100,100,"clip")
        turtle_hide()
        turtle_up()
        turtle_goto(5,50)
        turtle_down()
        
        #draws nodes and records locations
        #this loop repeats for every new generation
        for (i in 1:x$tot){
            
            #makes the shape triangular
            h = 45
            if (x$tot!=1){
                height = h*(i-1)/(x$tot-1)
            }                          
            
            #if there is one node in this generation
            if (nchar(x$strlist[i])==1){
                turtle_up()
                turtle_goto(5+(i-1)*x$flength,50)
                locs = c(locs, list(c(turtle_getpos(), i,1)))
                turtle_down()
                if(x$strlist[i]=="A"){
                    draw_point(x$colA)
                }else{
                    draw_point(x$colB)
                }
                
            #if there are more than one node in this generation
            }else if (nchar(x$strlist[i])!=0){
                turtle_up()
                for (j in seq(nchar(x$strlist[i]))){
                    turtle_up()
                    turtle_goto(5+(i-1)*x$flength,50 + height - ((2*height*(j-1))/(nchar(x$strlist[i])-1)))
                    locs = c(locs, list(c(turtle_getpos(), i,1)))
                    turtle_down()
                    if (substring(x$strlist[i],j,j) == "A"){
                        draw_point(x$colA)
                    }else{
                        draw_point(x$colB)
                    }
                }
            }
        }
        
        #draws lines between nodes
        #this loop repeats for every node
        for (i in 1:length(locs)){
            level = locs[[i]][3]
            lol = list()
            
            #makes list of all locations in the next level
            for (k in locs){
                if (k[3]==level+1 & k[4]>0){
                    lol = c(lol, list(k))
                }
            }
            
            #goes to parent node
            turtle_up()
            turtle_goto(locs[[i]][1], locs[[i]][2])
            turtle_down()
            
            if (strtoi(x$nums[[i+1]][1]) != 0){
                for (j in 1:strtoi(x$nums[[i+1]][1])){
                    turtle_goto(lol[[1]][1], lol[[1]][2])   #goes to first unused location on the next level
                    for (k in 1:length(locs)){
                        if (all(locs[[k]] == lol[[1]])){
                            ind = k                         #finds the index
                            break
                        }
                    }
                    
                    locs[[ind]][4]= locs[[ind]][4]-1        #marks location as used
                    lol = lol[-1]                           #and deletes it from list
                    turtle_up()
                    turtle_goto(locs[[i]][1], locs[[i]][2]) #goes back to parent node
                    turtle_down()
                }
            }
        }
    })
    
    #outputs second tree
    output$plot2 <- renderPlot({
        x<-y()
        
        #initializes variables
        initial_angle = 50
        nums = x$nums[-1]
        nlist = list()
        
        #initializes turtle
        turtle_init(100,100,"clip")
        turtle_hide()
        turtle_up()
        turtle_goto(5,50)
        turtle_down()
        
        #converts from level order to preorder tree
        topreorder = function(level){
            if (level<=x$tot){
                for(i in 1:length(nums)){
                    if (nums[[i]][2]==level){
                        a = nums[[i]]
                        nums <<- nums[-i]  #makes sure nums/nlist is global so we can call function recursively
                        break
                    }
                }
                
                nlist <<- c(nlist, list(a)) 
                
                if (a[1]!=0){
                    for (i in 1:a[1]){
                        topreorder(level+1)
                    }
                }
                
            }
            
            
        }
        topreorder(1)
        
        #first colour to use
        col = nlist[[1]][3]
        
        #draws tree
        draw_tree2 <- function(angle){
            
            col <<- nlist[[1]][3]
            
            #draws node of correct colour
            if (col == "A"){
                draw_point(x$colA)
            }else {
                draw_point(x$colB)
            }
            
            #n is the number of offspring for the current node
            n = strtoi(nlist[[1]][1])
            nlist <<- nlist[-1]
            
            
            
            if(n>=2){
                u_length = 2*x$flength*tan(pi*angle/180)
                small_length = u_length / (n-1)
                
                #repeats for every offspring
                for (i in 1:n-1){                                #this is (1:n)-1 and not 1:(n-1) 
                    up = u_length/2 - i*small_length
                    small_angle = 180*(atan(up/x$flength)/pi)
                    turtle_setangle(-small_angle+90)
                    turtle_forward((x$flength^2 + up^2)^0.5)
                    draw_tree2(angle_func(n,angle))              #calls function recursively
                    turtle_setangle(-small_angle+90)
                    turtle_backward((x$flength^2 + up^2)^0.5)
                }
                
            }else if(n==1){
                turtle_setangle(90)
                turtle_forward(x$flength)
                draw_tree2(angle)                                #calls function recursively
                turtle_setangle(90)
                turtle_backward(x$flength)
                
            }
            
        }
        draw_tree2(initial_angle)
        
    })
    
    #outputs population over time
    output$plot3 <- renderPlot({
        x<-y()
        
        strlist = x$strlist
        tot = x$tot
        
        notinf = c()
        inf = c()
        
        #deletes last entry if it is empty
        if(strlist[length(strlist)]==""){
            strlist = strlist[-length(strlist)]
            tot = tot-1
        }
        
        # finds the number of infected/non-infected per generation
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
        
        #draws bar graph
        df = data.frame(Time = rep(seq(tot),2), Population = c(notinf, inf), 
                        Type = c(rep("Non-Infectious",length(notinf)), rep("Infectious",length(inf))))
        ggplot(df,aes(x=Time, y=Population))+ theme_minimal() +
            geom_col(aes(fill=Type), width=0.7) + scale_fill_manual(values = c(x$colB, x$colA))
        
        
    })
    
    #text describing node colours
    output$text1 <- renderText({
        x<-y()
        
        paste("We have two types of node - infectious nodes are coloured ", x$colB, " and non infectious nodes are coloured ", x$colA, ".",
              sep="")
        
        
    })
    
    #outputs the geometric parameters given the mean
    output$text2 <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; this is a geometric parameter of ~",
                      round(1/(1+input$par_before),3), sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    output$text3 <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; this is a geometric parameter ~",
                      round(1/(1+input$par_after),3), sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    
}

#runs the app 
shinyApp(ui = ui, server = server)
