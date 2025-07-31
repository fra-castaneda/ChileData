
source('R/ChileData.R')





SLM_CL_1d <-SeaLevel(Time=1)
SLM_CL_7d <-SeaLevel(Time=7)$df

SLM_CL_Boyeruca <- SeaLevel(ID= 'boye2')$df
SLM_CL_Boyeruca_plot <- SeaLevel(ID= 'boye2', plot=TRUE)$plot
SLM_CL_Boyeruca_plot







