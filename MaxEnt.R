
library(dplyr)
library(ggplot2)

# hyperparameters
nbdata<- 200
nbclasses<- 8
nbprifeatures<- 128

# defining probability
probdist<- t(mapply(function(i) runif(nbprifeatures), 1:nbclasses))
saveRDS(probdist, 'data/maxent_probdist.rds')

# making big table
labels<- sample(1:nbclasses, replace = TRUE, size = nbdata)
bigtbl<- data.frame()
for (dataidx in 1:nbdata) {
  sampled.nums<- t(mapply(function(i) runif(nbprifeatures), 1:nbclasses))
  sampled.nums[labels[dataidx],]<- 1- sampled.nums[labels[dataidx],]
  featuretbl<- data.frame(jdid=dataidx, 
                          classlabel=1:nbclasses,
                          selected=(labels[dataidx]==1:nbclasses)) %>%
    bind_cols(data.frame(sampled.nums>probdist))
                        
  names(featuretbl)[4:(3+nbprifeatures)]<- paste('c', 1:nbprifeatures, sep='_')
  
  bigtbl<- bigtbl %>% bind_rows(featuretbl)
}
for (labelid in 1:nbprifeatures) {
  bigtbl[, paste('c', labelid, sep='_')]<- as.numeric(bigtbl[, paste('c', labelid, sep='_')])
}
saveRDS(bigtbl, 'data/bigtbl.rds')

# train maxent
maxent.model<- glm(as.formula(paste('selected~', paste(paste('c', 1:nbprifeatures, sep='_'), collapse = '+'))),
                   family=binomial,
                   data=bigtbl)
saveRDS(maxent.model, 'data/maxent_model.rds')

# predict data
newdata<- bigtbl
logodds<- predict(maxent.model, newdata = newdata)
new.pred.tbl<- newdata %>% 
  select(jdid, classlabel, selected) %>%
  mutate(explogodds=exp(logodds)) 
sum.logodds<- new.pred.tbl %>% group_by(jdid) %>% 
  summarise(sumexplo=sum(explogodds)) %>% pull(sumexplo)
new.pred.tbl<- new.pred.tbl %>% 
  group_by(jdid) %>% 
  filter(explogodds==max(explogodds)) %>% 
  ungroup()
new.pred.tbl$maxprob<- new.pred.tbl$explogodds/sum.logodds
new.pred.tbl<- new.pred.tbl %>% select(jdid, classlabel, selected, explogodds, maxprob)

saveRDS(new.pred.tbl, 'data/new_pred_tbl.rds')

# plot accuracy
new.pred.tbl %>% 
  mutate(bin=floor(new.pred.tbl$maxprob/0.1)*0.1+0.05) %>% 
  group_by(bin) %>%
  summarize(accuracy=sum(selected)/length(selected)) %>%
  ggplot(aes(x=bin, y=accuracy)) + 
  geom_line()
