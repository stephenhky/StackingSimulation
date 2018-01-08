
library(dplyr)
library(ggplot2)

nbdata<- 20000
nbclasses<- 6
nbprifeatures<- 5
binfeatures<- c(5, 6)

data.frame(x=seq(0, 1, 0.01), y=dexp(seq(0, 1, 0.01), rate=5)) %>% ggplot(aes(x, y)) + geom_line()
