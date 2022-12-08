## Script name: run_penguin_analysis.r
##
## Purpose of script: 
##      Loads penguin data, cleans it, and plots a linear model of flipper length as a function of body mass, 
##      and saves the plot to a file.

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

# Set working directory 
setwd("~/Desktop/PenguinProject ")

#Load the data
write.csv(penguins_raw, paste0("data_raw/data_raw.csv"))

# ---- Cleaning --------------
# Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(penguins_raw){
  penguins_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

#Cleaning the data using cleaning() function and remove_empty_flipper_length() function
penguins_clean <- cleaning(penguins_raw)
head(penguins_raw)

# Save the cleaned data
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

# ---- Statistical Test -----
linear_model <- lm(body_mass_g ~ flipper_length_mm, data = penguins_clean)#Fitting a linear model to examine the body mass as a function of flipper length in the penguins_clean data. 
anova(linear_model) #Performing an statistical test on the model 

# ---- Plot linear model --------------
#Rename facet labels:
species_labels <- c('Adelie', 'Chinstrap', 'Gentoo')
names(species_labels) <- c('Adelie Penguin (Pygoscelis adeliae)','Chinstrap penguin (Pygoscelis antarctica)','Gentoo penguin (Pygoscelis papua)')

#Plot linear regression plot for each species.
linear_regression_plot <- ggplot(penguins_clean, aes(x = flipper_length_mm, y= body_mass_g)) +
  facet_grid(~species, labeller = labeller(species = species_labels)) +
  geom_point(size = 0.5 ) +
  stat_smooth(method = "lm", col= "blue", formula = y~x)+
  labs(x = "Flipper length (mm)",
  y = "Body mass (g)") +
  theme_grey()

#Viewing the plot
linear_regression_plot

#---- Saving the plot --------------
setwd("~/Desktop/PenguinProject /figures")
png(file = "save_linear_regression_plot.png", 
    width = 500,
    height = 500, 
    res = 600)
dev.off()

