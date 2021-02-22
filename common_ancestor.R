###############################################

samples_per_tree = 10
no_of_trees = 300
height_of_each_tree = 60

###############################################

#initializes variables
param = 1/2
ca_all = c()

for (t in 1:no_of_trees){
  done = 0
  
  #repeat until we get tree of desired height
  while (done != 1){
    
    vec = list(c(1))                    
    vec2 = list()                       
    for(i in 1:height_of_each_tree){ 
      if (length(vec)!=0){
        
        #finds next gen given this gen
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
    
    #stop when we hit desired height
    if (length(vec) != 0){
      if (length(vec[[1]]) == height_of_each_tree+1){
        done = 1
      }
    }
  }
  
  ca_this_tree = c()
  
  #take sample and find common ancestor
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
    
    ca_this_tree = c(ca_this_tree,ca)
    
  }
  
  ca_all = c(ca_all,ca_this_tree)
}

#draw histogram
col = randomColor(1, luminosity = "light", hue="blue")
hist(ca_all, breaks = seq(0,height_of_each_tree+1)-0.5, freq=TRUE, col = col, border = col, 
     xlab = "Most recent common ancestor", main = paste(strwrap(paste("simulated vs theoretical frequency
     of most recent common ancestor for two points in generation", height_of_each_tree),width = 50),
     collapse = "\n"))
     
#draw line graph
x = seq(0,1,0.001)
y = (-4*x-2*(x-2)*log(1/(1-x)))/(x^3)
lines(x*height_of_each_tree,y*samples_per_tree*(no_of_trees/height_of_each_tree), lwd=2, col = "darkblue")