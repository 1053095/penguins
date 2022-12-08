## Script name: penguin_assignment.r
##
## Purpose of script: 
##      Loads penguin data, cleans it, and plots a linear model of flipper length as a function of body mass, 
##      and save the plot to a file.

# Install the packages
install.packages("palmerpenguins")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ragg")
install.packages("svglite")

# Load the packages
library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ragg)
library(svglite)

#Loading functions
source("functions/cleaning.r")
source("functions/plotting.r")

# Set working directory 
setwd("~/Desktop/PenguinProject ")

#Load the data
write.csv(penguins_raw, paste0("data_raw/data_raw.csv"))

# ---- Cleaning --------------
#Cleaning the data using cleaning() function
penguins_clean <- cleaning(penguins_raw)
head(penguins_clean) #Viewing the clean data set

# Save the cleaned data
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

# ---- Statistical Test -------
linear_model <- lm(body_mass_g ~ flipper_length_mm, data = penguins_clean)#Fitting a linear model to examine the body mass as a function of flipper length in the penguins_clean data. 
anova(linear_model) #Performing an ANOVA test on the model 

# ---- Plot linear model --------------
#Rename facet labels:
species_labels <- c('Adelie', 'Chinstrap', 'Gentoo')
names(species_labels) <- c('Adelie Penguin (Pygoscelis adeliae)','Chinstrap penguin (Pygoscelis antarctica)','Gentoo penguin (Pygoscelis papua)')

#Plot linear regression plot for each species by calling plotting function
linear_regression_plot <- plot_linear_model(penguins_clean)

#Viewing the plot
linear_regression_plot

#---- Saving the plot --------------
setwd("~/Desktop/PenguinProject /figures")
png(file = "save_linear_regression_plot.png", 
    width = 500,
    height = 500, 
    res = 600)
dev.off()

