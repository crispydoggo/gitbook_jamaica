
# Clear Lac
# CARICOM RBM System Draft
# Spring 2022

# ========================
# Libraries
# ========================


library(tidyverse)
library(readxl)
library(XLConnect)
library(fmsb) # Radar Chart
library(scales) # Scales as percentage
library(cowplot) # Grid of Plots (not used)
library(extrafont) # Used to import the Lora font to R

# ========================
# Data
# ========================


radar <- read_excel("radar.xlsx")


# ========================
# Headers
# ========================

header <- c( " RBM System Diagnosis" ,  
           " RBM Institutionalization Diagnosis", 
           " RBM Execution Diagnosis", 
           " RBM Technical Diagnosis",
           " RBM Use Diagnosis")

# ========================
# Font Lora - Run Only Once
# ========================

# install.packages("extrafont")

# library(extrafont)

## Importa todos los archivos .ttf de tu sistema
## Tendrás que ejecutar esto una vez, pero tardará unos minutos en acabar

# font_import()

# https://r-coder.com/personalizar-fuentes-r/


# ========================
# Colors
# ========================

# Colors of the border of the radar chart

colors_border <- matrix( 
    # Jamaica
    c(rgb(0,91/255,0,alpha = 0.9), rgb(0,91/255,0,alpha = 0.5), 
    # Saint Lucia
    rgb(8/255,59/255,202/255,alpha = 0.9), rgb(8/255,59/255,202/255,alpha = 0.5),
    # Dominica
    rgb(171/255,5/255,69/255,alpha = 0.9), rgb(171/255,5/255,69/255,alpha = 0.5),
    # CXC
    rgb(0,169/255,235/255,alpha = 0.9), rgb(0,169/255,235/255,alpha = 0.5),
    # IMPACS
    rgb(0,169/255,235/255,alpha = 0.9), rgb(0,169/255,235/255,alpha = 0.5)),
  nrow = 5, ncol = 2,byrow = TRUE   )


# Colors of the fill of the radar chart

colors_in <- matrix( 
  # Jamaica
  c(rgb(32/255,77/255,51/255,alpha = 0.4), rgb(32/255,77/255,51/255,alpha = 0.2), 
    # Saint Lucia
    rgb(32/255,77/255,203/255,alpha = 0.4), rgb(32/255,77/255,203/255 ,alpha = 0.2),
    # Dominica
    rgb(175/255,52/255,99/255,alpha = 0.4), rgb(175/255,52/255,99/255,alpha = 0.2) , 
    # CXC
    rgb(17/255,178/255,242/255,alpha = 0.4), rgb(0,169/255,235/255,alpha = 0.2),
  # IMPACS
  rgb(17/255,178/255,242/255,alpha = 0.4), rgb(0,169/255,235/255,alpha = 0.2)), 
  nrow = 5, ncol = 2,byrow = TRUE   )



# ========================
# Function Radar Graph 
# ========================


radar_rbm <- function( data, colors_border, colors_in, whole = c(FALSE,TRUE) ){
  
  # This function develops the radar chart for an specific dimension of the RBM system
  
  # ----------------------------------------------------------------------------------
  
  par(family = "Lora", mar = c(0.01, 0.01, 0.01, 0.01))
  
  if(whole){
    radarchart( data  , axistype=1 , 
                
                #custom polygon
                pcol=colors_border[1] , pfcol = colors_in[1] , plwd=2 , 
                
                #custom the grid
                cglcol="grey", cglty=2, axislabcol="grey", caxislabels=percent(seq(0,1,0.25)), cglwd=0.8,
                
                #custom labels
                vlcex=1.2, vlabels = c( "Institutionalization",	"Execution \n Framework"	, "Technical Capabilities",	"Use of \n Evidence")
                
    )
  }
  else{
    
    radarchart( data, axistype=1 , 
                
                #custom polygon
                pcol=colors_border , pfcol = colors_in , plwd=2 , 
                
                #custom the grid
                cglcol="grey", cglty=2, axislabcol="grey", caxislabels=percent(seq(0,1,0.25)), cglwd=0.8,
                
                #custom labels
                vlcex=1.2, vlabels = c( "Institutionalization",	"Execution \n Framework"	, "Technical Capabilities",	"Use of \n Evidence")
    )
    
    
  }
  
}


# ========================
# Exportation Function
# ========================


export_png <- function( filename, data, colors_border, colors_in, whole = c(FALSE,TRUE)){
  
  # The method creates a png image of a radar chart and exports to the specified directory
  # ---------------------------------------------------------------------------------------
  
  dir.create(dirname(filename), showWarnings = FALSE)
  
  # 1. Open png file
  png(filename = filename,
      width = 1900, height = 1600, units = "px", res = 300, pointsize = 11)
  
  # 2. Create the plot
  
  radar_rbm(data,colors_border, colors_in,whole)
  
  # 3. Close the file
  dev.off()
  
}


# ==================================
# Loop For Exporting for each country
# ==================================

radar_graph <- function(){
  
  # The methods makes an iterative creation of the countries' radar charts
  # ---------------------------------------------------------------------------------------
  
  # 1) For each country/institution i
  
    for( i in 1:max(radar$order)){
      
      # Filter by country/institution
      
      data <- filter( radar, order == i)
      
      # 2) For each dimension j of the RBM system
      
      for( j in 0:(nrow(data) - 1)  ){
        
        # Create the file's title 
        
        title <- paste0( data$country[1], header[j + 1] , sep = "") 
        
        # Create the file's directory
        filename <- paste("./Radar", str_replace(data$country[1], " ", ""), 
                           paste0(j, ". ", title, ".png", sep = "" ), sep = "/"  )
        
        
        # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot
        
        data_1 <- rbind(rep(100,4) , rep(0,4) , select(data, -country, -order))
        
        # If is the radar chart of all the dimensions
        
        if( j == 0){
          
          export_png( filename, data_1[1:3,], colors_border[i,], colors_in[i,],  whole = TRUE)
          
        }
        # If is not the radar chart of all the dimensions
        else{
          
          export_png( filename, data_1[c(1,2,3 + j, 3),], colors_border[i,], colors_in[i,],  whole = FALSE)
          
        }
        
        
      }
      
      
    }
}

# Create the radar charts








