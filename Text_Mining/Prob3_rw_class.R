#c The Class
#the constructor
rw = function(step=0){
  if((step%%1!=0)|(step<0)) {
    print("Parameter step is not valid") 
    return
  }
  x_path=numeric(step+1)
  y_path=numeric(step+1)
  set.seed(0)
  path = sample(1:4,step,replace=TRUE)
  #1:go up; 2:go down; 3:do right; 4:go left. 
  #Those directions are with equal probability
  #then transfer it to the operation of each steps
  x_path[which(path==1)+1]=1
  x_path[which(path==2)+1]=-1
  y_path[which(path==3)+1]=1
  y_path[which(path==4)+1]=-1
  x_pos = cumsum(x_path)
  y_pos = cumsum(y_path)
  obj = list(length=step,x.path=x_pos,y.path=y_pos,
             x.origin = x_pos[1],y.origin=y_pos[1])
  class(obj)<-'rw'
  return(obj) 
}
#the printer
#print out the origin, step and the final position
print.rw = function(object){
  cat("The walk starts from:(",object$x.origin,",",object$y.origin,").","\n")
  cat("After",object$length, "length of random walk,")
  cat("the final position of random walk is: (",object$x.path[length(object$x.path)]
      ,",",object$y.path[length(object$y.path)],")")
}
#plot method
plot.rw = function(object){
  x_lim = max(abs(object$x.path))
  y_lim = max(abs(object$y.path))
  plot(object$x.origin,object$y.origin,
       xlim=c(-1-x_lim,1+x_lim),
       ylim=c(-1-y_lim,1+y_lim),
       xlab="X",ylab="Y")
  for (i in 1:length(object$x.path)) 
    lines(c(object$x.path[i],object$x.path[i+1]),
          c(object$y.path[i],object$y.path[i+1]),col=i+1)
}
#operator []
'[.rw'=function(object,i){
  x = object$x.path[i+1]
  y = object$y.path[i+1]
  return(c("x"=x,"y"=y))
}
#start replacement
'start<-' <- function(object,...) UseMethod("start<-",object)
'start<-.rw' = function(object,value=c(0,0)){
  object$x.path= object$x.path+value[1]
  object$y.path= object$y.path+value[2]
  object$x.origin = value[1]
  object$y.origin = value[2]
  return(object)
}


