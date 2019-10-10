### 8 October 2019 - Cat
## Helping Morgan with some stats


### Main Question:
# concentration ~ season * stemwood increment
# see how concentrations vary across the seasons in a given stemwood increment  
# (i.e. sugar concentration in 0-1 stemwood increment ~ season). 
# This is really what I want to show - 
# that concentrations vary seasonally A LOT in shallower increments, and only a little in deeper increments.

## Main Issues with data:
# Right skew of data for total and total sugar concentration
## zero-inflated starch concentration


## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)



#### So let's make some fake data based on the histograms...
set.seed(12345)
n <- 100
ntot <- n*5
incr <- rep(c("0-1","1-2","2-3","3-4","4-8"), each=n)

total <- c(sample(0:150, size = n, replace = TRUE, prob = 150:0),
               sample(0:100, size = n, replace = TRUE, prob = 100:0),
               sample(0:60, size = n, replace = TRUE, prob = 60:0),
               sample(0:50, size = n, replace = TRUE, prob = 50:0),
               sample(0:35, size = n, replace = TRUE, prob = 35:0))

sugar <- c(sample(0:80, size = n, replace = TRUE, prob = 80:0),
           sample(0:50, size = n, replace = TRUE, prob = 50:0),
           sample(0:40, size = n, replace = TRUE, prob = 40:0),
           sample(0:35, size = n, replace = TRUE, prob = 35:0),
           sample(0:35, size = n, replace = TRUE, prob = 35:0))

p <- round(rnorm(n, 0.5, 0.1), digits=1)
starch <- c(ifelse(rbinom(n, size = 1, prob = p) >= 0.5, 0, sample(0:140, size=n, replace=TRUE, prob=140:0)),
            ifelse(rbinom(n, size = 1, prob = p) >= 0.5, 0, sample(0:80, size=n, replace=TRUE, prob=80:0)),
            ifelse(rbinom(n, size = 1, prob = p) >= 0.5, 0, sample(0:35, size=n, replace=TRUE, prob=35:0)),
            ifelse(rbinom(n, size = 1, prob = p) >= 0.5, 0, sample(0:40, size=n, replace=TRUE, prob=40:0)),
            ifelse(rbinom(n, size = 1, prob = p) >= 0.5, 0, sample(0:15, size=n, replace=TRUE, prob=15:0)))

season <- sample(c("apring","bsummer","cfall","winter"), ntot, replace=TRUE)

df <- data.frame(incr, season, total, sugar, starch)


total.allhist <- ggplot(df, aes(x=incr, y=total, fill=season)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymin=ymin, ymax=ymax),width = 0.2, position=position_dodge(0.9)) + 
  theme_classic()

total.allhist               



#### Let's start with total 
sug.mod.rand <- brm(sugar ~ season + (season|incr), data=df )
sug.mod.rand



######################################################################
#### Now for mu plots based of bb_analysis/models_stan_plotting.R ###
######################################################################

figpath <- "~/Desktop"
figpathmore <- "sugar_brms" ### change based on model

source("~/Documents/git/classes/Stats/exp_muplot_brms.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 5)

alphahere = 0.4
xlab <- "Model estimate of change in sugar concentration"

incr <- unique(df$incr)

modelhere <- tot.mod.rand


intercept <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 1] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("intercept", "[", i, "]", sep="")
}
intercept$parameter<-new.names

summer <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("summer", "[", i, "]", sep="")
}
summer$parameter<-new.names
mod.ranef <- full_join(intercept, summer)

fall <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("fall", "[", i, "]", sep="")
}
fall$parameter<-new.names
mod.ranef<-full_join(mod.ranef, fall)

winter <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 4] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("winter", "[", i, "]", sep="")
}
winter$parameter<-new.names
mod.ranef <- full_join(mod.ranef, winter)

ints <- c("intercept[1]", "intercept[2]", "intercept[3]", "intercept[4]", "intercept[5]")
mod.ranef$int.mean <- rep(mod.ranef$mean[(mod.ranef$parameter%in%ints)])
mod.ranef$int.25 <- rep(mod.ranef$`25%`[mod.ranef$parameter%in%ints])
mod.ranef$int.75 <- rep(mod.ranef$`75%`[mod.ranef$parameter%in%ints])

mod.ranef$mean <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$mean + mod.ranef$int.mean, mod.ranef$mean)
mod.ranef$`25%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`25%` + mod.ranef$int.25, mod.ranef$`25%`)
mod.ranef$`75%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`75%` + mod.ranef$int.75, mod.ranef$`75%`)

modoutput <- tidy(modelhere, prob=c(0.5))

muplotfx(modelhere, "", 8, 8, c(0,4), c(-5, 60) , 62, 3.5)


