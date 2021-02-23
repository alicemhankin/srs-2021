library(shiny)
library(TurtleGraphics)
require(randomcoloR)

#draws point
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

#draws first tree
draw_tree<- function(all_nodes, pop_over_time, tot, flength, margin, plot_width, plot_height) {
    
    count = 1
    locs = list()
    
    #repeats for every generation
    for (i in 1:tot){
        
        #makes tree triangle shaped
        h = (plot_height/2)-margin
        height = h*i/tot
        
        #draws nodes and records locations
        if (pop_over_time[i]==1){
            turtle_up()
            turtle_goto(margin+(i-1)*flength,plot_height/2)
            locs = c(locs, list(c(turtle_getpos(), i,1)))
            turtle_down()
            
            if(all_nodes[[count]][3]==1){
                draw_point("red")
            }else{
                draw_point("black")
            }
            count = count+1
        }else if (pop_over_time[i]!=0){
            turtle_up()
            for (j in seq(pop_over_time[i])){
                turtle_up()
                turtle_goto(margin+(i-1)*flength,plot_height/2 + height - ((2*height*(j-1))/(pop_over_time[i]-1)))
                locs = c(locs, list(c(turtle_getpos(), i,1)))
                turtle_down()
                
                if(all_nodes[[count]][3]==1){
                    draw_point("red")
                }else{
                    draw_point("black")
                }
                count = count+1
                
                
            }
        } 
    }
    
    #draws lines connecting the nodes
    for (i in 1:length(all_nodes)){                            #i is the index of the current node
        level = all_nodes[[i]][2]                              #level of current node
        
        #finds list of all nodes in next level
        lol = list()
        for (k in locs){
            if (k[3]==level+1 & k[4]>0){
                lol = c(lol, list(k))
            }
        }   
        
        turtle_up()
        turtle_goto(locs[[i]][1], locs[[i]][2])           #goes to the current (parent) node
        turtle_down()
        
        if (all_nodes[[i]][1] != 0){
            for (j in 1:all_nodes[[i]][1]){                   
                turtle_goto(lol[[1]][1], lol[[1]][2])     #goes to the child
                
                #finds index of child
                for (k in 1:length(locs)){   
                    if (all(locs[[k]] == lol[[1]])){
                        ind = k
                        break
                    }
                }                
                
                locs[[ind]][4]= locs[[ind]][4]-1           #marks as done          
                lol = lol[-1]                              
                turtle_up()
                turtle_goto(locs[[i]][1], locs[[i]][2])    #goes back to the parent
                turtle_down() 
            }
        }
    }
}

#returns the angle for generation n+1
angle_func <- function(angle, n){
    if (n==2){
        return (angle*0.8)
    }else{
        return (1.2*angle/n)
    }
}

#defines UI
ui <- fluidPage(
    
    #creates title
    titlePanel("Branching Process Simulation"),
    
    #creates sidebar 
    sidebarLayout(
        sidebarPanel(
            
            numericInput("seeds",
                         "Select seed (0 for random):",
                         min = 0,
                         max = 10000,
                         value = 3),
            
            numericInput("when_to_change",
                         "Which generation to change parameters? (0 for when detected):",
                         min = 0,
                         max = 100,
                         value = 0),
            
            sliderInput("det_prob",
                        "Detection probability:",
                        min = 0,
                        max = 1,
                        value = 0.05),
            
            numericInput("geom_rv1",
                         "Mean offspring before parameter change (>1):",
                         min = 1,
                         max = 20,
                         value = 3.5),
            
            htmlOutput("text1"),
            
            numericInput("geom_rv2",
                         "Mean offspring after parameter change (<1):",
                         min = 0.000001,
                         max = 1,
                         value = 0.5),
            
            htmlOutput("text2"),
            
            numericInput("maxiteration",
                         "Maximum number of generations:",
                         min = 1,
                         value = 10),
            
            #regenerate button
            actionButton("regenerate", "Re-generate", icon = icon("sync"), style=paste("color: #fff; 
                        background-color: ", randomColor(1, luminosity = "dark"), "; border-color:
                        #ffffff;", sep="")),
            
        ),
        
        #main section
        mainPanel(  
                        conditionalPanel(
                            condition = "output.show_tree_option",
                            radioButtons("graphs", "Show trees?",
                                         choices = list("Yes" = 1, "No" = 0),selected = 1
                            )
                            
                        ),
                        htmlOutput("text3"),
            
            #creates tabs
            tabsetPanel(id = "tabs",
                        tabPanel("Shows all nodes", plotOutput("plot1")),
                        tabPanel("Shows structure", plotOutput("plot2")),
                        tabPanel("Population over time", plotOutput("plot3"))
            )
            
            
            
            
        )
        
    )
)

# Defines server logic
server <- function(input, output) {
    
    #outputs whether to show tree or not
    output$show_tree_option <- reactive({
        x<-y()
        if (x$allow_tree ==1){
            return(TRUE)
        }else{
            return(FALSE)
        }
    })
    outputOptions(output, 'show_tree_option', suspendWhenHidden = FALSE)
    
    #reactive expression
    y <- reactive({
        input$regenerate
        
        #can change these!
        max_points = 50                 #maximum number of points in any one generation (before tree not allowed)
        margin = 5                      #margins of tree on the page
        plot_width = 100                #tree plot width
        plot_height = 1.5*plot_width    #tree plot height
        flength = 0.4*plot_width        #forwards distance between generations (if it fits on screen)
        
        #determines if user is allowed to view the tree
        allow_tree = 1
        if(input$when_to_change!=0){
            if ((input$geom_rv1 ^(input$when_to_change-1)) > max_points | (input$geom_rv2 ^(input$maxiteration - 
                input$when_to_change))*(input$geom_rv1 ^(input$when_to_change-1)) > max_points){
                allow_tree = 0
            }
        }else{
            if (input$det_prob < 1/max_points){
                allow_tree = 0
            }
        }
        
        #hides or shows the tree
        if (input$graphs==1 & allow_tree == 1){
            showTab(inputId = "tabs", target = "Shows all nodes")
            showTab(inputId = "tabs", target = "Shows structure")
        }else{
            hideTab(inputId = "tabs", target = "Shows all nodes")
            hideTab(inputId = "tabs", target = "Shows structure")
        }
 
        #initializes variables
        pop_over_time = c(1)
        has_died_out = 0
        iteration = 1
        all_nodes = list()
        newgeo = input$geom_rv1
        flipped = 0
        
        #when to change variables
        if (input$when_to_change!=0){
            flip = input$when_to_change
        }else{
            flip = input$maxiteration
        }
        
        #loops every generation until population hits zero or we reach maximum iterations
        while (has_died_out == 0 & iteration<input$maxiteration){
            
            parents = tail(pop_over_time, 1)
            total_kids = 0
            
            if (parents!=0){
                
                #if we haven't changed parameters, determine if we do
                if (flipped == 0){
                    
                    #sets seed
                    if (input$seeds != 0){
                        set.seed(strtoi(paste(input$seeds,iteration,sep="")))
                    }
                    
                    if (input$when_to_change==0){
                        node_colours = rbinom(size = 1, n=parents, prob = input$det_prob)
                    }else if (flip == iteration){
                        node_colours = rep(1,parents)
                    }else{
                        node_colours = rep(0,parents)
                    }
                    
                    if (sum(node_colours) !=0){
                        flipped = 1
                        newgeo = input$geom_rv2
                    }
                }
                
                #loops for each node in current generation
                for (parent in 1:parents){
                    
                    #sets seed
                    if (input$seeds != 0){
                        set.seed(strtoi(paste(input$seeds,iteration,parent,sep="")))
                    }
                    
                    #finds offspring 
                    kids = rgeom(1,1/(newgeo+1))
                    
                    #and records colour
                    if (flipped == 2 | flipped==0){
                        all_nodes = c(all_nodes, list(c(kids, iteration, 0)))
                    }else{
                        all_nodes = c(all_nodes, list(c(kids, iteration, node_colours[parent])))
                    }
                    
                    #running total of offspring in next generation
                    total_kids = total_kids + kids
                }
                
                if (flipped == 1 | iteration == flip){
                    flipped = 2
                }
            }
            
            pop_over_time = c(pop_over_time, total_kids)
            
            #stop if population hits zero
            if (tail(pop_over_time, 1) ==0){
                has_died_out = 1
            }
            iteration = iteration + 1
        }        
        
        #makes sure final generation doesn't have offspring
        if (tail(pop_over_time,1)!=0){
            for(i in 1:tail(pop_over_time,1)){
                all_nodes = c(all_nodes, list(c(0,iteration,0)))
            }
        }
        
        tot = length(pop_over_time)
        
        #changes horizontal distance between points to fit on the page
        if (pop_over_time[length(pop_over_time)]!=0){
            flength = min((plot_width-2*margin)/(tot-1), flength)
        }else{
            flength = min((plot_width-2*margin)/(tot-2), flength)
        }
        
        #returns values
        x = list(all_nodes, pop_over_time, tot, flength,  margin, plot_width, plot_height, max_points, allow_tree)
        names(x) = c("all_nodes", "pop_over_time", "tot", "flength", "margin", "plot_width", "plot_height", "max_points",
                     "allow_tree")
        x
        
    })
    
    #first tree
    output$plot1 <- renderPlot({
        
        x<-y()
        
        if (x$allow_tree == 1){
        
            turtle_init(x$plot_width,x$plot_height,"clip")
            turtle_hide()
            turtle_up()
            turtle_goto(x$margin,x$plot_height/2)
            turtle_down()
            
            #draws the first tree
            draw_tree(x$all_nodes, x$pop_over_time, x$tot, x$flength, x$margin, x$plot_width, x$plot_height)
        
        }
        
        
    }, height=500, width=500)
    
    #second tree
    output$plot2 <- renderPlot({
        x<-y()
        
        if (x$allow_tree == 1){

            #initializes variables
            initial_angle = 50
            all_nodes = x$all_nodes
            nlist = list()
            
            #initializes turtle
            turtle_init(x$plot_width, x$plot_height,"clip")
            turtle_hide()
            turtle_up()
            turtle_goto(x$margin,x$plot_height/2)
            turtle_down()
            
            #converts level order tree to pre-order tree recursively
            topreorder = function(level){
                
                if (level<=x$tot){
                    for(i in 1:length(all_nodes)){
                        if (all_nodes[[i]][2]==level){
                            a = all_nodes[[i]]
                            all_nodes <<- all_nodes[-i]
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
            
            #draws tree recursively
            draw_tree2 <- function(angle){
                
                if(nlist[[1]][3] == 1){
                    draw_point("red")
                }else{
                    draw_point("black")
                }
                
                n = nlist[[1]][1]
                nlist <<- nlist[-1]
                
                if(n>=2){                               
                    u_length = 2*x$flength*tan(pi*angle/180)
                    small_length = u_length / (n-1)
                    for (i in 1:n-1){
                        up = u_length/2 - i*small_length
                        small_angle = 180*(atan(up/x$flength)/pi)
                        turtle_setangle(-small_angle+90)
                        turtle_forward((x$flength^2 + up^2)^0.5)
                        draw_tree2(angle_func(angle,n))
                        turtle_setangle(-small_angle+90)
                        turtle_backward((x$flength^2 + up^2)^0.5)
                    }
                }else if(n==1){
                    turtle_setangle(90)
                    turtle_forward(x$flength)
                    draw_tree2(angle)
                    turtle_setangle(90)
                    turtle_backward(x$flength)
                }
                
            }
            draw_tree2(initial_angle)
            
        }
        
    }, height=500, width=500)
    
    #line graph
    output$plot3 <- renderPlot({
        x<-y()
        
        #plots line graph
        plot(1:(length(x$pop_over_time)),x$pop_over_time, col=rainbow(1, start=0.6), type='l', ylab="Population", xlab="Time")
        
    })
    
    #mean before detection
    output$text1 <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; the geometric parameter is ",
                      round(1/(input$geom_rv1+1),3))
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    
    #mean after detection
    output$text2 <- renderUI({
        
        str2 <- paste("&nbsp;")
        str1 <- paste("&rarr; the geometric parameter is ",
                      round(1/(input$geom_rv2+1),3))
        HTML(paste(str1, str2, sep = '<br/>'))
        
    })
    
    #text for when population is too big and can't show tree
    output$text3 <- renderUI({
        
        x<-y()
    

        if (x$allow_tree == 0){
            
            if(input$when_to_change == 0){
                str2 <- paste("Try increasing the detection probability")
            }else{
                str2 <- paste("Try reducing the mean offspring or the generation 
                          when the parameters change")
            }
            str1 <- paste("Showing trees is an option only when we expect a maximum population of",
                          x$max_points, " or less")
            HTML(paste(str1, str2,"&nbsp", sep = '<br/>'))
        }

    })

}

# Runs the application 
shinyApp(ui = ui, server = server)