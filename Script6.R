library(ggplot2)
library(plot.matrix)
library(ggplotify)
library(grid)


ffn<-as.double(0)
ffn<-as.double(0)
sfn<-as.double(0)
tfn<-as.double(0)
J <- as.double(-1)
m <- as.double(0)

energy <- as.double(0)
energy_new <- as.double(0)
dimension <-as.integer(60)
dimension_2 <- dimension -1
dimension_3 <- dimension_2 -1
energy_history<-data.frame(matrix(0, ncol = 4, nrow = 10000))
colnames(energy_history)<-c("Step","Energy","Magnetization","Mag_VAR")


lattice <- data.frame(replicate(dimension,replicate(dimension,0)))
lattice_save <- data.frame(replicate(dimension,replicate(dimension,0)))
lattice[2:dimension_2 ,2:dimension_2] <- data.frame(replicate(dimension_3,sample(c(-1,1),dimension,rep=TRUE)))
correlation_matrix_start <- array(dim=c(dimension ,dimension ,dimension,dimension ))
correlation_matrix <- array(dim=c(dimension ,dimension ,dimension,dimension ))
mean_correlation_matrix <- array(dim=c(dimension ,dimension ))
Ising_lattice<-data.matrix(lattice)

plot(Ising_lattice,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE)

for (i in 2:dimension_3) {
  for (j in 2:dimension_3) {
    
    m<-m+lattice[i,j]
    
  }
}



print(m/(dimension_3*dimension_3))

for (iter in 1:2500) {
  m <- 0
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
  
  for (i in 2:dimension_3) {
    for (j in 2:dimension_3) {
      
      m<-m+lattice[i,j]
      
    }
  }
  
  energy_history[iter,"Energy"]=energy
  energy_history[iter,"Step"]=iter
  energy_history[iter,"Magnetization"]=mean(as.matrix(lattice[2:dimension_2,2:dimension_2]))
  energy_history[iter,"Mag_VAR"]=var(as.matrix(lattice[2:dimension_2,2:dimension_2]))
  
  print(c(iter,energy,mean(as.matrix(lattice[2:dimension_2,2:dimension_2]))))
  
  lattice_save<-lattice
  
  for (h in 1:1)  {
    
    x <- sample(2:dimension_3 , 1)
    y <- sample(2:dimension_3 , 1)
    
    
    
    if ( lattice[x,y]==1){
      lattice[x,y]=-1
    }
    
    if ( lattice[x,y]==-1){
      lattice[x,y]=+1
    }
    
    
    
  }
  #  print(x)
  #  print(y)
  
  #for (k in 0:3) {
  #  for (j in 0:3) {
  #    lattice[k+2+3,j+2+3]=+1
  #    lattice[dimension_2-k-3,dimension_2-j-3]=-1
  #  }
  #}
  
  
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
    #   delta<-as.double(0)
    #   q<-as.double(0)
    #   p<-sample(1:10,1)/10
    #    delta<-abs(energy_new-energy)
    #print(delta)
    #    q<-exp(-delta/0.0000025)
    #print(c(p,q))
    #    if(p > q){
    
    lattice=lattice_save
    #   }
  }
  
  Ising_lattice_FINAL<-data.matrix(lattice)
  
  
  
  
  
  
} 


Ising_lattice_FINAL<-data.matrix(lattice)

plot(Ising_lattice_FINAL,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE)

#plot(energy_history)
energy_history<-energy_history[-2,]
ggplot(data= energy_history,mapping = aes(x = Step, y = Energy))+geom_line()+theme_bw()
ggplot(data= energy_history,mapping = aes(x = Step, y = Magnetization))+geom_line()+theme_bw()


ffn<-as.double(0)
sfn<-as.double(0)
tfn<-as.double(0)
fofn<-as.double(0)

lattice_red<-data.frame(lattice[-c(1,15),-c(1,15)])
dimension_4<-dimension_3-1
colnames(lattice_red)<-(0:dimension_4)
rownames(lattice_red)<-(0:dimension_4)

for (i in 1:dimension_3) {
  for (j in 1:dimension_3) {
    
    
    
    i_right= ((i) %% dimension_3)+1
    i_left= ((i-2) %% dimension_3)+1
    j_down= ((j-2) %% dimension_3)+1
    j_up= ((j) %% dimension_3)+1
    
    
    i_rright= ((i+1) %% dimension_3)+1
    i_lleft= ((i-3) %% dimension_3)+1
    j_ddown=  ((j-3) %% dimension_3)+1
    j_uup= ((j+1) %% dimension_3)+1
    
    i_rrright= ((i+2) %% dimension_3)+1
    i_llleft= ((i-4) %% dimension_3)+1
    j_dddown= ((j-4) %% dimension_3)+1
    j_uuup= ((j+2) %% dimension_3)+1
    
    i_rrrright= ((i+3) %% dimension_3)+1
    i_lllleft= ((i-5) %% dimension_3)+1
    j_ddddown= ((j-5) %% dimension_3)+1
    j_uuuup= ((j+3) %% dimension_3)+1
    
    
    ffn<-ffn+(lattice_red[i,j]*lattice_red[i_right,j]+lattice_red[i,j]*lattice_red[i_left,j]+lattice_red[i,j]*lattice_red[i,j_up]+lattice_red[i,j]*lattice_red[i,j_down])/4
    
    sfn<-sfn+(lattice_red[i,j]*lattice_red[i_rright,j]+lattice_red[i,j]*lattice_red[i_lleft,j]+lattice_red[i,j]*lattice_red[i,j_ddown]+lattice_red[i,j]*lattice_red[i,j_uup]
              +lattice_red[i,j]*lattice_red[i_right,j_down] + lattice_red[i,j]*lattice_red[i_right,j_up]+lattice_red[i,j]*lattice_red[i_left,j_down]+lattice_red[i,j]*lattice_red[i_left,j_up])/8
    
    tfn<-tfn+(lattice_red[i,j]*lattice_red[i_rrright,j]+lattice_red[i,j]*lattice_red[i_llleft,j]+lattice_red[i,j]*lattice_red[i,j_dddown]+lattice_red[i,j]*lattice_red[i,j_uuup]+lattice_red[i,j]*lattice_red[i_rright,j_down]+lattice_red[i,j]*lattice_red[i_right,j_ddown]+lattice_red[i,j]*lattice_red[i_rright,j_up]+lattice_red[i,j]*lattice_red[i_right,j_uup]+lattice_red[i,j]*lattice_red[i_lleft,j_down]+lattice_red[i,j]*lattice_red[i_left,j_ddown]+lattice_red[i,j]*lattice_red[i_left,j_uup]+lattice_red[i,j]*lattice_red[i_lleft,j_up])/12
    
    fofn<-fofn+(lattice_red[i,j]*lattice_red[i_rrrright,j]+lattice_red[i,j]*lattice_red[i_lllleft,j]+lattice_red[i,j]*lattice_red[i,j_ddddown]+lattice_red[i,j]*lattice_red[i,j_uuuup]
                +lattice_red[i,j]*lattice_red[i_right,j_uuup]+lattice_red[i,j]*lattice_red[i_rright,j_uup]+lattice_red[i,j]*lattice_red[i_rrright,j_up]
                +lattice_red[i,j]*lattice_red[i_rrright,j_down]+lattice_red[i,j]*lattice_red[i_rright,j_ddown]+lattice_red[i,j]*lattice_red[i_right,j_dddown]
                +lattice_red[i,j]*lattice_red[i_llleft,j_down]+lattice_red[i,j]*lattice_red[i_lleft,j_ddown]+lattice_red[i,j]*lattice_red[i_left,j_dddown]
                +lattice_red[i,j]*lattice_red[i_llleft,j_up]+lattice_red[i,j]*lattice_red[i_lleft,j_uup]+lattice_red[i,j]*lattice_red[i_left,j_uuup]
    )/16
    
    
    
  }
}
ffn

sfn

tfn

fofn

save(Ising_lattice_FINAL,file="J-1_60_2500_T=0_lattice.rda")
save(energy_history,file="J-1_60_2500_T=0_energy.rda")

