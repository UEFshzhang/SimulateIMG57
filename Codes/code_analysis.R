library(dplyr)
library(tidyverse)
library(forcats)
library(ggpubr)


# Open code for the analysis of simulation results  
# Article doi: to be updated 


# Define functions for computing RMSE and bias. Call: rmse(observed_values, predicted_values)
# Observed value y must be input first

rmse<-function(y,yhat){ sqrt(sum((y-yhat)^2)/length(y))} # RMSE

relrmse<-function(y,yhat){ sqrt(sum((y-yhat)^2)/length(y))/mean(y)} # RMSE-%

bias<-function(y,yhat){ sum(y-yhat)/length(y)} # bias

relbias<-function(y,yhat){ sum(y-yhat)/length(y)/mean(y)} # bias-%


# Import field reference LAI data measured by Digital hemispherical photographs (DHPs)
dhp <- read.csv("./Open data/field.csv") 


# Define LAI classes based on field reference LAI data by their quantile 
dhp <- dhp %>%
  mutate(LAI_class = case_when(quantile(dhp$LAIe)[1] <= LAIe & LAIe < quantile(dhp$LAIe)[2] ~ "Q1",
                               quantile(dhp$LAIe)[2] <= LAIe & LAIe < quantile(dhp$LAIe)[3] ~ "Q2",
                               quantile(dhp$LAIe)[3] <= LAIe & LAIe < quantile(dhp$LAIe)[4] ~ "Q3",
                               quantile(dhp$LAIe)[4] <= LAIe & LAIe <= quantile(dhp$LAIe)[5] ~ "Q4" ),
         PlotNo = c(1:127)) # Assign a number to each plot


# Merge data frames: simulated LAI and reference LAI
df <- read.csv("./Open data/scienario_example.csv") %>% 
          left_join(dhp, by = "PlotID") %>%
          mutate(Simu_LAI = -1.089 * log(SimuGF)) %>% # Compute LAI at the hinge angle
          group_by(PlotID) %>% 
          mutate(relRMSE = relrmse(LAIe, Simu_LAI), # compute RMSE%
                 sdLAI = sd(Simu_LAI)) # Compute SD of the simulated LAI

# Add lengend category and notes in preparation for making plots
df <- df %>% 
  mutate(Simu_LAI_min = min(Simu_LAI), Simu_LAI_q1 = quantile(Simu_LAI)[2],
         Simu_LAI_median = median(Simu_LAI),
         Simu_LAI_q3 = quantile(Simu_LAI)[4], Simu_LAI_max = max(Simu_LAI),
         
         Simu_LAI_col = case_when(LAIe < Simu_LAI_min ~ "NA",
                                  Simu_LAI_min <= LAIe & LAIe < Simu_LAI_q1 ~ "Q1", 
                                  Simu_LAI_q1 <= LAIe & LAIe <= Simu_LAI_q3 ~ "Q2-3",
                                  Simu_LAI_q3 < LAIe & LAIe <= Simu_LAI_max ~ "Q4", 
                                  Simu_LAI_max < Simu_LAI_max ~ "NA"),
         
         legend_col = case_when(Simu_LAI_col == "Q1" ~ "Lower whisker", 
                                Simu_LAI_col == "Q2-3" ~ "Inside the box", 
                                Simu_LAI_col == "Q4" ~ "Upper whisker" )) 

# Explore Q1
q1 <- df %>% subset(LAI_class == "Q1", )

# Q1 plot
p_q1 <- ggplot(q1, aes(x=reorder(PlotNo, LAIe), y=Simu_LAI, fill=legend_col)) + 
          geom_boxplot(outlier.shape = NA) +
          scale_fill_manual(values=c("aquamarine2", "darksalmon", "burlywood2")) +
          stat_boxplot(geom = "errorbar", width = 0.15, lwd = 0.5) +  # Line width
          geom_point(aes(x=reorder(PlotNo, LAIe), y=LAIe), color = 'red') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y = "Simulated LAI", x = "Plot ID", fill = "Legend") +
          ggtitle("a")


# Explore Q2
q2 <- df %>% subset(LAI_class == "Q2", ) 

# Q2 plot
p_q2 <- ggplot(q2, aes(x=reorder(PlotNo, LAIe), y=Simu_LAI, fill=legend_col)) + 
          geom_boxplot(outlier.shape = NA) +
          scale_fill_manual(values=c("aquamarine2", "darksalmon", "burlywood2")) +
          stat_boxplot(geom = "errorbar", width = 0.15, lwd = 0.5) +  # Line width
          geom_point(aes(x=reorder(PlotNo, LAIe), y=LAIe), color = 'red') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y = "Simulated LAI", x = "Plot ID", fill = "Legend")+
          ggtitle("b")


# Explore Q3
q3 <- df %>% subset(LAI_class == "Q3", )

p_q3 <- ggplot(q3, aes(x=reorder(PlotNo, LAIe), y=Simu_LAI, fill=legend_col)) + 
          geom_boxplot(outlier.shape = NA) +
          scale_fill_manual(values=c("aquamarine2", "darksalmon", "burlywood2")) +
          stat_boxplot(geom = "errorbar", width = 0.15, lwd = 0.5) +  # Line width
          geom_point(aes(x=reorder(PlotNo, LAIe), y=LAIe), color = 'red') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y = "Simulated LAI", x = "Plot ID", fill = "Legend")+
          ggtitle("c")


# Explore Q4
q4 <- df %>% subset(LAI_class == "Q4", )

p_q4 <- ggplot(q4, aes(x=reorder(PlotNo, LAIe), y=Simu_LAI, fill=legend_col)) + 
          geom_boxplot(outlier.shape = NA) + 
          scale_fill_manual(values=c("aquamarine2", "darksalmon", "burlywood2")) +
          stat_boxplot(geom = "errorbar", width = 0.15, lwd = 0.5) +  # Line width
          geom_point(aes(x=reorder(PlotNo, LAIe), y=LAIe), color = 'red') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
          labs(y = "Simulated LAI", x = "Plot ID", fill = "Legend")+
          ggtitle("d")


ggarrange(p_q1, p_q2, p_q3, p_q4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

# # Save figure
# png("./Fig.png", units="cm", width=17, height=17, res=600)
# 
# dev.off()


#########################
## Regression analysis ##
#########################

# Regression analysis on the residual against plot attributes

# Import simulation results 

regression <- df %>% 
        filter(!PlotID %in% c("Hyytiala_U28", "Hyytiala_U1")) %>%  # No field data
        mutate(resid = LAIe - Simu_LAI) %>% 
        group_by(PlotID) %>%
        mutate(mean_resid = mean(resid),
               abs_mean_resid = abs(mean_resid),
               sum_abs_resid = sum(abs(resid)),
               rel_abs_mean_resid = abs_mean_resid / LAIe) %>% ungroup()


# Add stand measurements from field 
field <- read_excel("./Open data/plot_attributes.xlsx", sheet = 1)[,3:8] %>% transform(Ch = as.numeric(Ch))


join_df <- left_join(regression, field, by = c("PlotID"="Plot")) %>%
  mutate(legend_col = case_when(Simu_LAI_col == "Q1" ~ "Lower whisker", 
                                Simu_LAI_col == "Q2-3" ~ "Inside the box", 
                                Simu_LAI_col == "Q4" ~ "Upper whisker" ))


# Linear regression models 
m1 <- lm(abs_mean_resid ~ BA+DBH, data = join_df) 
m3 <- lm(rel_abs_mean_resid ~ BA+DBH, data = join_df)  

summary(m1)
summary(m3)


par(mfrow = c(1,2))

plot(fitted(m1), residuals(m1), abline(0,0, col = "red"), 
     xlim = c(0, 0.2),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Non-normalised residual")

plot(fitted(m3), residuals(m3), abline(0,0, col = "red"), 
     xlim = c(0, 0.1),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Normalised residual")


dev.off()




