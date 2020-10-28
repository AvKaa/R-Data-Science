heights

# Create separate smooth density plots for males and females by defining group by sex. 
# Use the existing aes function inside of the ggplot function.
heights %>% 
  ggplot(aes(height,group = sex))
geom_density()

# alpha blending (transparency)
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha = 0.2) 