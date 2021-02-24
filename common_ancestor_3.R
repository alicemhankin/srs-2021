library(randomcoloR)
library(TurtleGraphics)

draw_point <- function(colour){
  turtle_col(colour)
  
  sides = 10
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

#this needs work
angle_func <- function(angle, n,level){
  return (angle/n)
}

draw_tree <- function(angle){
  
  draw_point("black")
  level = level + 1
  
  n = nlist[1]
  nlist <<- nlist[-1]
  
  if(n>=2){  
    
    
    for (i in 1:n-1){
      
      flength = length(idklist[[1]])*factor
      idklist <<- idklist[-1]
      
      
      u_length = 2*flength*tan(pi*angle/180)
      small_length = u_length / (n-1)
      up = u_length/2 - i*small_length
      small_angle = 180*(atan(up/flength)/pi)
      turtle_setangle(-small_angle+90)
      turtle_forward((flength^2 + up^2)^0.5)
      draw_tree(angle_func(angle,n, level))
      turtle_setangle(-small_angle+90)
      turtle_backward((flength^2 + up^2)^0.5)
    }
  }else if(n==1){
    flength = 3*length(idklist[[1]])
    idklist <<- idklist[-1]
    turtle_setangle(90)
    turtle_forward(flength)
    draw_tree(angle)
    turtle_setangle(90)
    turtle_backward(flength)
  }
  
  
}

repeats <- function(veclist){
  
  if (length(veclist)>1){
    
    stop = 0
    for (i in 1:length(veclist[[1]])){
      for (j in veclist){
        if (j[i] != veclist[[1]][i]){
          point = i
          stop = 1
          break
        }
      }
      if (stop == 1){
        break
      }
    }
    
    
    idklist <<- c(idklist, list(veclist[[1]][1:point-1]))
    
    for (i in 1:length(veclist)){
      veclist[[i]] = veclist[[i]][point:length(veclist[[i]])]
    }
    
    
    
    firsts = c()
    for (i in 1:length(veclist)){
      if (!(veclist[[i]][1] %in% firsts)){
        firsts = c(firsts, veclist[[i]][1])
      }
    }
    
    
    nlist <<- c(nlist, length(firsts))
    
    for (i in firsts){
      newveclist = list()
      for (j in veclist){
        if (j[1]==i){
          newveclist = c(newveclist,list(j))
        }
      }
      repeats(newveclist)
    }
    
  }else{
    nlist <<- c(nlist, 0)
    idklist <<- c(idklist, list(veclist[[1]]))
  }
  
}

###############################################

samples_per_tree = 100          #number of pairs of points we sample from each tree
height_of_each_tree = 100       #which generation we sample from
no_of_points = 100            #minimum number of points required to sample

###############################################

#initializes variables
param = 1/2
done = 0

#repeat until we get tree of desired height and number of points
while (done != 1){
  
  vec = list(c(1))                    
  vec2 = list()                       
  for(i in 1:height_of_each_tree){ 
    if (length(vec)!=0){
      
      #finds next gen given this gen (using ulam-harris labeling)
      for (j in vec){
        ran = rgeom(1,param)
        if (ran!=0){
          for (k in 1:ran){
            vec2 = c(vec2, list(c(j,k)))
          }
        }
      }
      
      #updates vector for this gen
      vec = vec2
      vec2 = list()
      
    }else{
      break
    }
  }
  
  #stop when we hit desired height and desired number of points
  if (length(vec) != 0){
    if (length(vec[[1]]) == height_of_each_tree+1 & length(vec) >= no_of_points){
      done = 1
    }
  }
}

ca_all = c()

#repeatedly takes sample and finds common ancestor
for (i in 1:samples_per_tree){
  
  a = vec[sample(1:length(vec),1)][[1]]
  b = vec[sample(1:length(vec),1)][[1]]
  
  ca = 0
  for (j in 2:length(a)){
    if (a[j]==b[j]){
      ca = ca + 1
    }else{
      break
    }
  }
  
  #updates list of common ancestors
  ca_all = c(ca_all,ca)
  
}


#draw histogram
col = randomColor(1, luminosity = "light",  hue="blue")
hist(ca_all, freq=TRUE, col = col, breaks = 1000,border = col, 
     xlab = "Most recent common ancestor", main = paste(strwrap(paste("simulated vs theoretical frequency
     of most recent common ancestor for two points in generation", height_of_each_tree),width = 50),
                                                        collapse = "\n"))

veclist = vec
nlist  = c()
idklist = list()
level = 0
initial_angle = 40
  
repeats(veclist)

factor = 90/(height_of_each_tree-length(idklist[[1]]))
idklist = idklist[-1]

turtle_init(100, 150,"clip")
turtle_hide()
turtle_up()
turtle_goto(5,75)
turtle_down()

draw_tree(initial_angle)