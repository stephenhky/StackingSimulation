
library(dplyr)
library(ggplot2)

# parameters
nbdata<-10000     # <------ change this number of data
nbclasses<- 840
regions<- seq(0.05, 0.95, 0.1)

# sample function
sample.probs<- function(nbprobs, topscore, max) {
  diff(c(0, topscore, sort(runif(nbprobs-2, topscore, max)), max))
}

# clip function
toclip <- function(x, a, b) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}

# sigmoid function
sigmoid<- function(z) exp(z)/(1+exp(z))

# simulation
bigtbl<- data.frame()
for (id in 1:nbdata) {
  midpt<- sample(regions, 1)
  topscore<- toclip(rnorm(1, mean=midpt, sd=0.025), 0, 1)
  probs<- sample.probs(nbclasses, topscore, 1)
  selected<- rep(0, nbclasses);   selected[sample(seq(1, nbclasses), 1, replace = TRUE, prob = probs)]<- 1
  bigtbl<- bigtbl %>% bind_rows(data.frame(Id=id, label=seq(1, nbclasses), selected=selected, prob=probs))
}


big.tbl<- bigtbl %>% mutate(odds=prob/(1-prob)) %>% group_by(Id) %>%
  mutate(normodds=odds/sum(odds)) %>% ungroup()

# prediction
pred.tbl<- big.tbl %>% group_by(Id) %>% slice(which.max(normodds)) %>% ungroup() %>% rename(score=prob)

# plot
pred.tbl %>% 
  mutate(bin=(as.numeric(cut(normodds, breaks=seq(0, 1, 0.1)))-0.5)*0.1) %>% 
  group_by(bin) %>%
  summarize(accuracy=mean(selected)) %>% 
  ggplot(aes(x=bin, y=accuracy)) + geom_point() + ggtitle("Accuracy as a function of normalized odds") + xlim(c(0, 1)) + ylim(c(0, 1)) +
  geom_abline(slope=1, intercept=0)






