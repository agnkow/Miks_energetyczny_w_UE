
### Loading required R packages ###
library(shiny)
library(shinythemes)
library(DT)
#library(htmltools)

library(cluster)
library(clusterSim)
library(factoextra)
library(magrittr)

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
require(gridExtra)
library(viridis) 

library(circlize)
library(dendextend)
require(ape)     
require(igraph)  
library(grDevices)

library(dplyr)
library(reshape2)
require(gdata)

library(scales)


options(scipen=999)
set.seed(123)


### Load scripts ###
path_2 <- '.' 
script_1 <- paste0(path_2, '/', 'palette.R')
script_2 <- paste0(path_2, '/', 'graphics.R')
script_data <- paste0(path_2, '/', 'prepare_data.R')

source(script_1, encoding = 'windows-1252')
source(script_2, encoding = 'windows-1252')
source(script_data, encoding = 'windows-1252')



### Data preparation ###
path = './data' 
file = paste0(path, '/', 'Net_electricity_generation_UE_2008-2018.xls')
file_2 = paste0(path, '/', 'Net_electricity_generation_UE_2008-2018_GWh_1.csv')
file_geo = paste0(path, '/', 'country_en_pl.csv')

df <- read.table(file, sep = ',', dec = '.', header=TRUE) 
df_total <- read.table(file_2, sep = ';', dec = '.', header=TRUE) 
dict_geo <- read.table(file_geo, sep = ';', dec = '.', header=TRUE)

df_total <- rename_geo_my(df_total)
df_total <- df_total[complete.cases(df_total[ , 2]),]
df_total <- rename_vars_my(df_total)
df_total[df_total == 0] <- NA


df <- rename_geo_my(df)
df <- replace_na_df(df)
df <- create_vars_NA(df)



### Vars in SelectInput ###
drops_2_pl <- c('Fale morskie',
                'Wegiel',
                'Ropa naftowa',
                'Gaz ziemny',
                'Biomasa',
                'Inne nieodnawialne',
                'Konwencjonalne NA')

years <- sort(unique(df$year), decreasing = TRUE)
countries <- sort(unique(df$GEO.TIME))
countries <- as.character(countries)

df_1 <- rename_vars_my(df)
vars <- names(df_1)[names(df_1) %!in% c(drops_2_pl,'year','GEO.TIME','Inne')] 
vars <- sort(vars)
vars <- c(vars, 'Inne')

vars_ind <- names(df_1)[names(df_1) %!in% c('year','GEO.TIME', 
                                            'Inne','Konwencjonalne')] 
vars_ind <- sort(vars_ind)
vars_ind <- c(vars_ind, 'Inne')
remove(df_1)


max_clusters <- 7






