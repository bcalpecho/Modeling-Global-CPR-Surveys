# ---
# title: Plot Global CPR predictions 
# author: Bryan Alpecho
# date: 2026 Feb 12 - 
# output: csv and plot for predictions
# ---

#Aims
#01 to plot for delta of trophic groups between 2015 and 2100

date <- "14022026"

#load libraries
  library(tidyverse)
  library(tmap)
  
#load zooplankton GLMs
load("Output/previousModels/revision/merged_chla_mdls_28112025.RData") #need to revise the directory
mdl_list <- list(Carni_mdl_zib, Omni_mdl_zib, Filter_mdl_zib_ver2)
names(mdl_list) <- c("Carni","Omni","Filter")

plot_projections_delta <- function(mdls){
  
  for(i in 1:length(mdls)){
    TG <- names(mdls)[i]
    
    #for plot label
    TG_label <- if(TG == "Carni"){ "carnivorous zooplankton"
    }else if(TG == "Omni"){"omnivorous zooplankton"
    }else{"gelatinous filter-feeders"}
    
    projection_baseline <- readRDS(file=paste("Output/data/projections/",TG,"_baseline_",date,".RData",sep=""))
    
    projection_future <- readRDS(file=paste("Output/data/projections/",TG,"_future_",date,".RData",sep=""))
    
    #to compute for delta pixel-by-pixel
    projection_delta <- projection_baseline %>% 
      left_join(projection_future %>% 
                  rename("future_estimate" = "estimate") %>% 
                  select(c("future_estimate","x","y")), by = c("x","y")) %>% 
      mutate(delta = (future_estimate - estimate) * 100)
    
    st_projections <- st_as_stars(projection_delta, dims = c("x","y"))
    
    #to plot
    TG_projection <- tm_shape(st_projections) +
      tm_raster(
        col = "delta",      # The name of your data column
        col.scale = tm_scale_continuous(values = c("blue", "white","red")),
        col.legend = tm_legend(paste("Mean change (%) of proportion from ",TG_label," by 2100 relative to 2015",sep=""),
                               orientation = "landscape", 
                               frame = FALSE)) +
      tm_layout(legend.outside = TRUE, bg.color = "black", text.size = 36) 
    
    tmap_save(TG_projection, filename=paste("Output/map/projections/",TG,"_delta_",date,".png",sep=""))
    # width = 400,
    # height = 200,
    # units = "mm",
    # dpi = 400)
    #}
  }
}

plot_projections_delta(mdl_list)
