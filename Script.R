library(ggplot2)
library(plot.matrix)
library(ggplotify)
library(grid)


energy <- as.double(0)
dimension <-as.integer(100)
dimension_2 <- dimension -1
dimension_3 <- dimension_2 -1




lattice <- data.frame(replicate(dimension,replicate(dimension,0)))
lattice[2:dimension_2 ,2:dimension_2] <- data.frame(replicate(dimension_3,sample(c(-1,1),dimension,rep=TRUE)))
Ising_lattice<-data.matrix(lattice)

plot(Ising_lattice,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE)

p<-as.grob( ~plot(Ising_lattice,breaks=c(-1,1),xaxt = "n",ylab='',xlab='',tick = FALSE))
grid.draw(p)

for (i in 2:3) {
  for (j in 2:3) {
    if ( lattice[i,j]==lattice[i+1,j]){
      energy = energy +1
    }
    if ( lattice[i,j]==lattice[i-1,j]){
      energy = energy +1
    }
    if ( lattice[i,j]==lattice[i,j+1]){
      energy = energy +1
    }
    if ( lattice[i,j]==lattice[i,j-1]){
      energy = energy +1
    }
    
  }

  }
  print(energy/(dimension_3*dimension_3))

  
  
  
  
  


  lattice <- data.frame(replicate(4,replicate(4,0)))
  lattice[2:3,2:3] <- data.frame(replicate(2,sample(c(-1,1),4,rep=TRUE)))
  Ising_lattice<-data.matrix(lattice)
  