library(imager)
library(raster)
library(magick)
library(tidyverse)
library(sf)
library(terra)

# Open code for calculating the simulated LAI  
# Article doi: to be updated 

# This code is designed for one specific simulation scenario
# To change to differnt scenarios, modify Line 105 and 134 accordingly 

# Scienario 18:	
# 4 images at hinge angle (57° from zenith) 
# with azimuths at 90 degree difference, at 5 locations

# Find x, y and r of the DHPs
centrex <- 465
centrey <- 465
rad <- 465

# Boundary BOX for the selected region
# E.g. AOV = 60 degrees horizontally, 40 degrees vertically
hor_aov <- 60
#ver_aov <- 40

# Input the number of images to be generated 
no_simu_dcp <- 100

# Calculate zenith angles 
angle <- matrix(0, nrow=centrex*2, ncol=centrey*2)

for (x in 1:(rad*2)) {
  for (y in 1:(rad*2)) {
    dy <- y - centrey;
    dx <- x - centrex;
    dist <- sqrt(dx^2 + dy^2);
    angle[x, y] <- dist/rad*90;
  }
}

angle <- raster(angle)
#plot(angle)


# Calculate azimuth angles
azim <- matrix(0, nrow = centrey*2, ncol = centrex*2)

for (x in 1:(centrex*2)) {
  for (y in 1:(centrey*2)) {
    if (x < centrex & y < centrey) {
      azim[x, y] <- ((atan((x - centrex)/(y - centrey))) * 180 / pi) + 270;} # 4th quadrant
    if (x < centrex & y > centrey) {
      azim[x, y] <- ((atan((x - centrex)/(y - centrey))) * 180 / pi) + 90;} # 1st quadrant
    if (x > centrex & y > centrey) {
      azim[x, y] <- ((atan((x - centrex)/(y - centrey))) * 180 / pi) + 90;} # 2nd quadrant
    if (x > centrex & y < centrey) {
      azim[x, y] <- ((atan((x - centrex)/(y - centrey))) * 180 / pi) + 270;} # 3rd quadrant
    if (x == centrex & y > centrey) {azim[x, y] <- 90;}
    if (x == centrex & y < centrey) {azim[x, y] <- 270;}
    if (x > centrex & y == centrey) {azim[x, y] <- 180;}
  }
}

azim <- raster(azim)
#plot(azim)


# Import DHPs by plot
Hyytiala_plt <- list.files("./Open data/DHPs/") %>% str_subset("Hyytiala")
Suonenjoki_plt <- list.files("./Open data/DHPs/") %>% str_subset("Suonenjoki")
Liperi_plt <- list.files("./Open data/DHPs/") %>% str_subset("Liperi")

plot_list <- c(Hyytiala_plt, Suonenjoki_plt, Liperi_plt)

# Create a data frame for saving the final simulation results of all plots
DF_write <- data.frame() 


# Loop through all plots 
# Simulate hinge angle images 
for (a in 1:length(plot_list)) { 
  plot_name <- plot_list[a]
  print(plot_name)
  
  # Simulation results at plot level
  Results <- data.frame() 
  PlotID <- c()
  SimulationNo <- c()
  
  # Simulate 100 times per plot
  for (c in 1:no_simu_dcp) { 
    PlotID <- rep(plot_name, 5)
    SimulationNo <- rep(c, 5)
    
    # Import DHP from the selected plot
    # Select one random image (which represents a random location) for simulation
    img_list <- list.files(paste0("./Open data/DHPs/", plot_name, "/results/"),
                           pattern = "\\.bmp$", full.names = TRUE) # Ending with .bmp only
    
    # Index and read images
    img_index <- sample(1:length(img_list), 5) # Generate 5 random index number 1-12
    img_dir <- img_list[img_index] # Random image's directory
    img <- image_read(img_dir)
    
    # Simulation results at image level
    ImageID <- c()
    GF_perlocation <- c()  
    Selected_image <- c()

    #b=1
    for (b in 1:length(img)) {
      ImageID[b] <- b
      Selected_image[b] <- str_sub(img_dir[b], start = -22)
      #print(paste("Location", b))
      
      # Asssign binary values, vegetation (1), gaps (0)     
      ras <- as.raster(img[b])
      ras[ras == "#000000ff"] <- 1
      ras[ras == "#ffffffff"] <- 0
      ras <- as.numeric(ras)
      
      # Create an empty raster and assign image values
      r <- raster(nrows = 930, ncols = 930,
                  xmn=0, xmx=1,
                  ymn=0, ymx=1, crs = NULL)
      
      values(r) <- ras
      #plot(r)
      
      # Stack three raster layers 
      ref <- stack(r, angle, azim)
      #plot(ref)
      
      # Simulate 4 images at one location
      simu_angles <- data.frame(simu_azim1 = runif(1, min = 7.5, max = 82.5),
                                simu_zeni = rep(57)) %>% 
        
                          mutate(simu_azim2 = simu_azim1 + 90,
                                 simu_azim3 = simu_azim1 + 180,
                                 simu_azim4 = simu_azim1 + 270) 
      

      # Boundary BOX for the selected region
      df_angles <- simu_angles %>%
        mutate(min_azim1 = simu_azim1 - hor_aov/2, max_azim1 = simu_azim1 + hor_aov/2,
               
               min_azim2 = simu_azim2 - hor_aov/2, max_azim2 = simu_azim2 + hor_aov/2,
               
               min_azim3 = simu_azim3 - hor_aov/2, max_azim3 = simu_azim3 + hor_aov/2,
               
               min_azim4 = simu_azim4 - hor_aov/2, max_azim4 = simu_azim4 + hor_aov/2,
               
               
               min_zeni = simu_zeni - 15/2, max_zeni = simu_zeni + 15/2)
      
      # Find the bbox of the four images
      bbox1 <- ref$layer.2 > df_angles$min_zeni &
                ref$layer.2 < df_angles$max_zeni &
                ref$layer.3 > df_angles$min_azim1 &
                ref$layer.3 < df_angles$max_azim1
      
      bbox2 <- ref$layer.2 > df_angles$min_zeni &
                ref$layer.2 < df_angles$max_zeni &
                ref$layer.3 > df_angles$min_azim2 &
                ref$layer.3 < df_angles$max_azim2

      bbox3 <- ref$layer.2 > df_angles$min_zeni &
                ref$layer.2 < df_angles$max_zeni &
                ref$layer.3 > df_angles$min_azim3 &
                ref$layer.3 < df_angles$max_azim3

      bbox4 <- ref$layer.2 > df_angles$min_zeni &
                ref$layer.2 < df_angles$max_zeni &
                ref$layer.3 > df_angles$min_azim4 &
                ref$layer.3 < df_angles$max_azim4

      # Set the bbox value to NA 
      values(bbox1)[values(bbox1) == 0] <- NA
      values(bbox2)[values(bbox2) == 0] <- NA
      values(bbox3)[values(bbox3) == 0] <- NA
      values(bbox4)[values(bbox4) == 0] <- NA
      
      bboxs <- merge(bbox1, bbox2, bbox3, bbox4)
      simu_region <- mask(ref$layer.1, bboxs)
      #plot(simu_region)
    
      # Calculate the GF of the simulated region
      gf <- sum(table(as.matrix(simu_region))[1]) /
        sum(table(as.matrix(simu_region)))
      
      GF_perlocation[b] <- gf
      GF_persimulation <- mean(GF_perlocation)

      df_simu_image <- as.data.frame(cbind(ImageID, Selected_image, 
                             GF_perlocation, GF_persimulation))
    }
     # Combine results for 
     df_simulation <- as.data.frame(cbind(PlotID, SimulationNo, df_simu_image))
     Results <- bind_rows(Results, df_simulation) 
  }   
    # Combine results from all plots
  DF_write <- bind_rows(DF_write, Results)
}


write.csv(DF_write, dir)








