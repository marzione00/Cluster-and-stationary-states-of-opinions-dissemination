library(ggplot2)
library(plot.matrix)
library(ggplotify)
library(grid)


ffn<-as.double(0)
sfn<-as.double(0)
tfn<-as.double(0)
J <- as.double(-1)
energy <- as.double(0)
energy_new <- as.double(0)
dimension <-as.integer(25)
dimension_2 <- dimension -1
dimension_3 <- dimension_2 -1
energy_history<-data.frame(matrix(0, ncol = 2, nrow = 601))
colnames(energy_history)<-c("Step","Energy")


lattice <- data.frame(replicate(dimension,replicate(dimension,0)))
lattice_save <- data.frame(replicate(dimension,replicate(dimension,0)))
lattice[2:dimension_2 ,2:dimension_2] <- data.frame(replicate(dimension_3,sample(c(-1,1),dimension,rep=TRUE)))
correlation_matrix_start <- array(dim=c(dimension ,dimension ,dimension,dimension ))
correlation_matrix <- array(dim=c(dimension ,dimension ,dimension,dimension ))
mean_correlation_matrix <- array(dim=c(dimension ,dimension ))
Ising_lattice<-data.matrix(lattice)

plot(Ising_lattice,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE)



for (iter in 1:200) {
  
  energy <- 0
  energy_new <- 0
  lattice_save <- data.frame(replicate(dimension,replicate(dimension,0)))


for (i in 2:dimension_3) {
  for (j in 2:dimension_3) {
    i_right= (i+1) %% dimension_3+2
    i_left= ((i-3) %% dimension_3)+2
    j_down= ((j-3) %% dimension_3)+2
    j_up=(j+1) %% dimension_3+2
    
    if ( lattice[i,j]==lattice[i_right,j]*J){
      energy = energy -1
    }
    if ( lattice[i,j]==lattice[i_left,j]*J){
      energy = energy -1
    }
    if ( lattice[i,j]==lattice[i,j_up]*J){
      energy = energy -1
    }
    if ( lattice[i,j]==lattice[i,j_down]*J){
      energy = energy -1
    }
    
  }
  
}
  
  energy_history[iter,"Energy"]=energy
  energy_history[iter,"Step"]=iter

print(c(iter,energy))

lattice_save<-lattice

  
  x <- sample(2:dimension_3 , 1)
  y <- sample(2:dimension_3 , 1)
  
  
  if ( lattice[x,y]==1){
    lattice[x,y]=-1
  }
  
  if ( lattice[x,y]==-1){
    lattice[x,y]=+1
  }
  
#  print(x)
#  print(y)

  for (i in 2:dimension_3) {
    for (j in 2:dimension_3) {
      i_right= (i+1) %% dimension_3+2
      i_left= ((i-3) %% dimension_3)+2
      j_down= ((j-3) %% dimension_3)+2
      j_up=(j+1) %% dimension_3+2
      
      
      if ( lattice[i,j]==lattice[i_right,j]*J){
        energy_new = energy_new -1
      }
      if ( lattice[i,j]==lattice[i_left,j]*J){
        energy_new = energy_new -1
      }
      if ( lattice[i,j]==lattice[i,j_up]*J){
        energy_new = energy_new -1
      }
      if ( lattice[i,j]==lattice[i,j_down]*J){
        energy_new = energy_new -1
      }
      
    }
    
  }
  
#  print(c(energy_new,energy))
  
  #print(energy_new/(dimension_3*dimension_3))
  
  if (energy_new < energy ){
   # print("Accepted")
  }
  
  if (energy_new  >= energy ){
   # print("Rejected")
    
    lattice=lattice_save
    
  }
  
  Ising_lattice_FINAL<-data.matrix(lattice)
  

  
} 


Ising_lattice_FINAL<-data.matrix(lattice)

plot(Ising_lattice_FINAL,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE)

#plot(energy_history)

ggplot(data= energy_history,mapping = aes(x = Step, y = Energy))+geom_line()+theme_bw()


ffn<-as.double(0)
sfn<-as.double(0)
tfn<-as.double(0)

for (i in 2:dimension_2) {
  for (j in 2:dimension_2) {

    
    
    i_right= (i+1) %% dimension_3+2
    i_left= ((i-3) %% dimension_3)+2
    j_down= ((j-3) %% dimension_3)+2
    j_up=(j+1) %% dimension_3+2



i_rright=(i+2) %% dimension_3 +2
i_lleft= (i-5) %% dimension_3 +2
j_ddown= (j-5) %% dimension_3 +2
j_uup=(j+2) %% dimension_3 +2

i_rrright=(i+3) %% dimension_3 +2
i_llleft= (i-6) %% dimension_3 +2
j_dddown= (j-6) %% dimension_3 +2
j_uuup=(j+3) %% dimension_3 +2


    
ffn<-ffn+(lattice[i,j]*lattice[i_right,j]+lattice[i,j]*lattice[i_left,j]+lattice[i,j]*lattice[i,j_up]+lattice[i,j]*lattice[i,j_down])/4

sfn<-sfn+(lattice[i,j]*lattice[i_rright,j]+lattice[i,j]*lattice[i_lleft,j]+lattice[i,j]*lattice[i,j_ddown]+lattice[i,j]*lattice[i,j_uup]+lattice[i,j]*lattice[i_right,j_down]+lattice[i,j]*lattice[i_right,j_up]+lattice[i,j]*lattice[i_left,j_down]+lattice[i,j]*lattice[i_left,j_up])/8

tfn<-tfn+(lattice[i,j]*lattice[i_rrright,j]+lattice[i,j]*lattice[i_llleft,j]+lattice[i,j]*lattice[i,j_dddown]+lattice[i,j]*lattice[i,j_uuup]+lattice[i,j]*lattice[i_rright,j_down]+2*lattice[i,j]*lattice[i_right,j_ddown]+lattice[i,j]*lattice[i_rright,j_up]+2*lattice[i,j]*lattice[i_right,j_uup]+lattice[i,j]*lattice[i_lleft,j_down]+2*lattice[i,j]*lattice[i_left,j_ddown]+lattice[i,j]*lattice[i_left,j_uup]+2*lattice[i,j]*lattice[i_lleft,j_up])/16


#sfn<-sfn+(lattice[i,j]*lattice[i_rright,j])/8



  }
}
ffn/(dimension_3*dimension_3)

sfn/(dimension_3*dimension_3)

tfn/(dimension_3*dimension_3)

  