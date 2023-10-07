install.packages('sjPlot')
library(sjPlot)
colnames(tun_r9_data)

xtabs(~tun_r9_data$Q22A)




library(tidyverse)  # general use ----
library(here)       # file paths  ----
library(haven)      # import .sav files ----  
library(labelled)   # tools for labelled data ----
library(sjlabelled) # more tools for labelled data ----


dictionary <- labelled::generate_dictionary(tun_r9_data)


## data potitics

data_pol_r9<-tun_r9_data%>%select()