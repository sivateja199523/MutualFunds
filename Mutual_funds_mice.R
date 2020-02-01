library(readr)
library(data.table)
library(nnet)
library(caret)
library(dplyr)
#install.packages('randomForest')
library(randomForest)
#install.packages('ROCR')
library(ROCR)
library(ggplot2)
library(cluster)
library(dbscan)
library(clustertend)
library(mice)
library(tidyverse)

mutualfunds_bl=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_blend_large.csv"
                    , sep=",", header=T, 
                    strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_bm=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_blend_medium.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_bs=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_blend_small.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_gl=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_growth_large.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_gm=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_growth_medium.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_gs=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_growth_small.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_vl=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_value_large.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_vm=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_value_medium.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
mutualfunds_vs=fread("Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_value_small.csv"
                     , sep=",", header=T, 
                     strip.white = T, na.strings = c("NA","NaN","","?"))
View(mutualfunds_bl)

mice_data_bl <- mice(mutualfunds_bl, defaultMethod = "pmm")
mice_data_bm <- mice(mutualfunds_bm, defaultMethod = "pmm")
mice_data_bs <- mice(mutualfunds_bs, defaultMethod = "pmm")
mice_data_gl <- mice(mutualfunds_gl, defaultMethod = "pmm")
mice_data_gm <- mice(mutualfunds_gm, defaultMethod = "pmm")
mice_data_gs <- mice(mutualfunds_gs, defaultMethod = "pmm")
mice_data_vl <- mice(mutualfunds_vl, defaultMethod = "pmm")
mice_data_vm <- mice(mutualfunds_vm, defaultMethod = "pmm")
mice_data_vs <- mice(mutualfunds_vs, defaultMethod = "pmm")
mice_data_bl<- mice::complete(mice_data_bl)
mice_data_bm<- mice::complete(mice_data_bm)
mice_data_bs<- mice::complete(mice_data_bs)
mice_data_gl<- mice::complete(mice_data_gl)
mice_data_gm<- mice::complete(mice_data_gm)
mice_data_gs<- mice::complete(mice_data_gs)
mice_data_vl<- mice::complete(mice_data_vl)
mice_data_vm<- mice::complete(mice_data_vm)
mice_data_vs<- mice::complete(mice_data_vs)
write.table(final_data, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutual_funds_mice.csv", sep=",", row.names=F)
write.table(mice_data_bl, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_bl_mice.csv", sep=",", row.names=F)
write.table(mice_data_bm, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_bm_mice.csv", sep=",", row.names=F)
write.table(mice_data_bs, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_bs_mice.csv", sep=",", row.names=F)
write.table(mice_data_gl, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_gl_mice.csv", sep=",", row.names=F)
write.table(mice_data_gm, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_gm_mice.csv", sep=",", row.names=F)
write.table(mice_data_gs, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_gs_mice.csv", sep=",", row.names=F)
write.table(mice_data_vl, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_vl_mice.csv", sep=",", row.names=F)
write.table(mice_data_vm, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_vm_mice.csv", sep=",", row.names=F)
write.table(mice_data_vs, file="Desktop/Fall 2019/6211-Advanced BA/Group Project/mutual-funds-and-etfs/Mutualfunds/mf_vs_mice.csv", sep=",", row.names=F)
nrow(mutualfunds_bl)
ncol(mutualfunds_bl)

nrow(mutualfunds_bm)
nrow(mutualfunds_bs)
nrow(mutualfunds_gl)
nrow(mutualfunds_gm)
nrow(mutualfunds_gs)
nrow(mutualfunds_vl)
nrow(mutualfunds_vm)
nrow(mutualfunds_vs)