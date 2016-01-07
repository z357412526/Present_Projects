#a
RandomWalk1= function(step,categ){
 if((step%%1!=0)|(step<0)|(step==0)) {
   print("Parameter step is not valid") 
   return
 }
 if((categ!="position")&&(categ!="path")){
   print("Parameter category is not valid")
   return
 }
 x=0
 y=0
 x_path=numeric(step+1)
 y_path=numeric(step+1)
 set.seed(0)
 path = sample(1:4,step,replace=TRUE)
 for (i in 1:step){
   if (path[i]==1){x=x+1}
   else if (path[i]==2){x=x-1}
   else if (path[i]==3){y=y+1}
   else {y=y-1}
   x_path[i+1]=x
   y_path[i+1]=y 
 }
 if (categ=="position"){
   print(paste0("The final position is: (", x, ",",y,")")) 
 }
 else if(categ=="path"){
   print("The path is:")
   #print(x_path)
   #print(y_path)
   for (i in 1:length(x_path)) print(paste0("(", x_path[i], ",",y_path[i],")"))
 } 
}


#b
##vectorize:
RandomWalk2= function(step,categ){
  if((step%%1!=0)|(step<0)|(step==0)) {
    print("Parameter step is not valid") 
    return
  }
  if((categ!="position")&&(categ!="path")){
    print("Parameter category is not valid")
    return
  }
  x_path=numeric(step+1)
  y_path=numeric(step+1)
  set.seed(0)
  path = sample(1:4,step,replace=TRUE)
  x_path[which(path==1)+1]=1
  x_path[which(path==2)+1]=-1
  y_path[which(path==3)+1]=1
  y_path[which(path==4)+1]=-1
  x_pos = cumsum(x_path)
  y_pos = cumsum(y_path)
  if (categ=="position"){
    print(paste0("The final position is: (", x_pos[step+1], ",",y_pos[step+1],")")) 
  }
  else if(categ=="path"){
    x_lim = max(abs(x_pos))
    y_lim = max(abs(y_pos))
    plot(0,0,xlim=c(-x_lim-1,x_lim+1),ylim=c(-y_lim-1,y_lim+1),xlab="X",ylab="Y",col=4)
    print("The path is:")
    for (i in 1:length(x_pos)) {
      print(paste0("(", x_pos[i], ",",y_pos[i],")"))
      lines(c(x_pos[i],x_pos[i+1]),c(y_pos[i],y_pos[i+1]),col=i+1)
    }
  } 
}

#c
