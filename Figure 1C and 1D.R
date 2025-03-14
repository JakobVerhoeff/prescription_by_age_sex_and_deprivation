### Code for Figure 1b and c

library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(leaflet)
library(stringr)
library(here)

setwd(here())

# read in combined data 
data <- read_csv("data/cleaned_combined_data.csv")

# Summarize data
###############################################
foidata_summary <- foidata_combined %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband = ave(total_items, AGE_BAND, FUN = sum),
         percentage = (total_items / total_items_ageband) * 100)
#############################################
foidata_summary <- foidata_combined %>%
  group_by(GENDER, ANTIBIOTIC_GROUP, AGE_BAND) %>%
  summarise(total_items = sum(ITEMS, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(total_items_ageband_gender = ave(total_items, AGE_BAND, GENDER, FUN = sum),
         percentage = (total_items / total_items_ageband_gender) * 100)

#############################################
# Ensure AGE_BAND is ordered correctly
foidata_summary <- foidata_summary %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = c("0-5", "6-10", "11-20", 
                                                "21-30", "31-40", "41-50", 
                                                "51-60", "61-70", "71-80", "81-90", 
                                                "91-100", "over 100")))

# Create plot with different linetypes for gender
foidata_summary<-foidata_summary%>%
  filter(!is.na(AGE_BAND))
# Create last_points to extract the last data point for each group
last_points <- foidata_summary %>%
  group_by(ANTIBIOTIC_GROUP, GENDER) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Now plot the graph with geom_text() for labeling
g1c <- 
  ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                            color = ANTIBIOTIC_GROUP, 
                            group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                            linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #           size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #           show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female",
       x = "Age Band",
       y = "Percentage (%)",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  theme(legend.position="bottom")

ggsave("plots/figure1c.pdf")

ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                            color = ANTIBIOTIC_GROUP, 
                            group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                            linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
 # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
#            size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
#            show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female zoomed",
       x = "Age Band",
       y = "Percentage (%)",
       color = "Antibiotic Group",
       linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo") +  # Apply viridis color palette
  ylim(0, 25)  # Set y-axis limits to match percentage range
 
ggsave("plots/figure1d_ALL.pdf")

g1d <- ggplot(foidata_summary, aes(x = AGE_BAND, y = percentage, 
                                   color = ANTIBIOTIC_GROUP, 
                                   group = interaction(ANTIBIOTIC_GROUP, GENDER), 
                                   linetype = GENDER)) +
  geom_line(size = 1) +  # Line plot with a consistent width
  geom_point(size = 2) +  # Points for each data point
  # geom_text(data = last_points, aes(label = ANTIBIOTIC_GROUP), 
  #            size = 3, vjust = -0.5, hjust = -0.1, # Adjust label position
  #            show.legend = FALSE) +  # Hide label legend
  labs(
    #title = "Percentage of Antibiotic Use by Age Band - Male vs Female zoomed",
    x = "Age Band",
    y = "Percentage (%)",
    color = "Antibiotic Group",
    linetype = "Gender") +
  theme_minimal() +  # Clean, minimal theme
  scale_y_continuous(lim = c(1, 25)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for clarity
  scale_color_viridis_d(option = "turbo",drop = FALSE) +  # Apply viridis color palette
  theme(legend.position="bottom")