library(ggplot2)
library(hexbin)

lattice <- data.frame(replicate(10,sample(c(-1,1),10,rep=TRUE)))
plotdf<-data.matrix(lattice)


plot(plotdf)