##Plotting function 
plot_linear_model <- function(penguins_clean){
  penguins_clean %>%
  ggplot(penguins_clean, aes(x = flipper_length_mm, y= body_mass_g)) +
  facet_grid(~species, labeller = labeller(species = species_labels)) +
  geom_point(size = 0.5 ) +
  stat_smooth(method = "lm", col= "blue", formula = y~x)+
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme_grey()
}