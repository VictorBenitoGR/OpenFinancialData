# Load the ggplot2 package
library(ggplot2)

# Create a data frame
df <- data.frame(
  Company = c(
    "AAPL", "MSFT", "NVDA", "FICO", "ARES", "ODFL",
    "KLAC", "LLY", "TSLA", "AVGO", "AZO", "AJG", "AMD", "CELH",
    "COR", "KKR", "COST", "ABBV", "REGN", "DECK"
  ),
  Sector = c(
    "Information technology", "Information technology",
    "Information technology", "Information technology", "Financials",
    "Industrials", "Information technology", "Health care",
    "Consumer discretionary", "Information technology",
    "Consumer discretionary", "Financials", "Information technology",
    "Consumer staples", "Health care", "Financials", "Consumer staples",
    "Health care", "Health care", "Consumer discretionary"
  )
)

# Count the frequency of each sector
sector_counts <- table(df$Sector)

# Convert the table to a data frame for ggplot2
sector_df <- as.data.frame(sector_counts)
names(sector_df) <- c("Sector", "Count")

# Sort the sector_df data frame by the Count column
sector_df <- sector_df[order(sector_df$Count), ]

# Reorder the factor levels of the Sector variable based on the new order
sector_df$Sector <- factor(sector_df$Sector, levels = sector_df$Sector)

# Add a small value to the Count column for each sector
sector_df$Count <- sector_df$Count + 0.1

library(dplyr)

# Calculate the percentage of each sector
sector_percentages <- expanded_sector_df %>%
  group_by(Sector) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the plot
plot <- ggplot() +
  # Add rectangles to represent each quantity
  geom_tile(
    data = expanded_sector_df,
    aes(
      x = reorder(Sector, Count),
      y = Quantity,
      fill = Sector
    ),
    width = 0.8, # Reduce the width to create a gap between sectors
    color = "black",
    alpha = .9
  ) +
  # Add a line to each sector
  geom_segment(
    data = max_count_df,
    aes(
      x = as.numeric(reorder(Sector, Count)),
      xend = as.numeric(reorder(Sector, Count)),
      y = 0,
      yend = Count + 0.5 # Adjust for polar transformation
    ),
    color = "gray12",
    linetype = "dashed"
  ) +
  # Add a point at the end of each line
  geom_point(
    data = max_count_df,
    aes(
      x = as.numeric(reorder(Sector, Count)),
      y = Count + 0.5 # Adjust for polar transformation
    ),
    size = 3,
    color = "gray12"
  ) +
  # Add the percentages to the top of each column
  # Add the percentages to the top of each column
  geom_text(
    data = sector_percentages,
    aes(
      x = reorder(Sector, Count),
      # Increase this value to move the labels further from the center
      y = Count + 2.0,
      label = paste0(round(Percentage, 1), "%")
    ),
    hjust = 0.5,
    fontface = "bold" # Make the text bold
  ) +
  # Make it circular!
  coord_polar() +
  # Change the color palette
  scale_fill_brewer(palette = "Accent") +
  # Make the guide for the fill discrete
  guides(
    fill = guide_legend(
      title.position = "top", title.hjust = .5
    )
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(face = "bold"), # Make the legend text bold
    legend.title = element_text(face = "bold") # Make the legend title bold
  )

# Now, you can save it to a file
ggsave("my_plot.png", plot, width = 8, height = 6, dpi = 600)

# * WHAT IF...

# df2 <- data.frame(
#   Time = c("0", "1y", "3y", "5y"),
#   NVDA = as.numeric(gsub("\\$", "", c("5000", "$15400",  "$32100", "$95500"))),
#   ARES = as.numeric(gsub("\\$", "", c("5000", "$7900", "$11800", "$28500"))),
#   CELH = as.numeric(gsub("\\$", "", c("5000", "$13700", "$26600", "$300300")))
# )

library(openxlsx)

# Import data
df2 <- read.xlsx("./data/what_if.xlsx")
View(df2)

# 15k, 37k, 70k, 424k
95500 + 28500 + 300300 <- 424300
# Load the necessary library
library(tidyverse)

# Reshape the data
df2_long <- df2 %>%
  pivot_longer(
    cols = -Time,
    names_to = "Ticker",
    values_to = "Value"
  )
# Create the bubble plot
# Load the stringr package
library(stringr)

# Replace " " with "\n" in the Time column
df2_long$Time <- str_replace_all(df2_long$Time, " ", "\n")

# Load the forcats package
library(forcats)

# Make NA an explicit level and place it at the beginning
df2_long$Time <- fct_explicit_na(df2_long$Time, na_level = "(Missing)")
df2_long$Time <- fct_relevel(df2_long$Time, "(Missing)")

# Then plot your data
# Then plot your data
plot2 <- ggplot(df2_long, aes(
  x = Time, y = Ticker, size = Value, color = Ticker
)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 20), guide = FALSE) + # Remove the size scale legend
  scale_color_manual(values = c(
    "NVDA" = "#004700", "ARES" = "#000051", "CELH" = "darkorange"
  ), guide = FALSE) +
  theme_minimal() +
  xlab(NULL) + # Remove the x axis label
  ylab(NULL) + # Remove the y axis label
  theme(
    # Make xy axis labels bold and black
    axis.text.x = element_text(face = "bold", color = "black", vjust = 2),
    axis.text.y = element_text(face = "bold", color = "black"),
    # Change grid line color to black
    panel.grid.major.x = element_blank(), # Remove vertical major grid lines
    panel.grid.minor.x = element_blank(), # Remove vertical minor grid lines
    panel.grid.major.y = element_line(color = "#58585889"),
    panel.grid.minor.y = element_line(color = "#58585889")
  ) +
  geom_label(
    aes(x = 1, y = Inf, label = "$3k"),
    vjust = 1, hjust = 0.5,
    fontface = "bold", size = 2, fill = "#bbbbbb", colour = "black"
  ) +
  geom_label(
    aes(x = 2, y = Inf, label = "$7.4k"),
    vjust = 1, hjust = 0.5,
    fontface = "bold", size = 2, fill = "#bbbbbb", colour = "black"
  ) +
  geom_label(
    aes(x = 3, y = Inf, label = "$14.1k"),
    vjust = 1, hjust = 0.5,
    fontface = "bold", size = 2, fill = "#bbbbbb", colour = "black"
  ) +
  geom_label(
    aes(x = 4, y = Inf, label = "$84.8k"),
    vjust = 1, hjust = 0.5,
    fontface = "bold", size = 2, fill = "#bbbbbb", colour = "black"
  ) +
  scale_x_discrete(labels = c("(Missing)" = "")) # Remove the "(Missing)" label

ggsave("my_plot2.jpg", plot2, width = 4, height = 2.5, dpi = 600)

# * Differences
# Create a data frame with the values
df_v <- data.frame(
  Value = c(15000, 424300),
  Label = c("$15k", "$424k")
)

# Create the plot
my_plot3 <- ggplot(df_v, aes(x = 1, y = Label, size = Value)) +
  geom_point(color = "black", fill = "#008a3c", shape = 21) +
  # Add labels with fixed size
  geom_text(aes(x = 1, y = Label), label = df_v$Label, hjust = -1, size = 3) +
  scale_size_continuous(range = c(1, 20)) +
  theme_void() + # Remove all non-data ink
  theme(legend.position = "none") # Remove legend

ggsave("my_plot3.jpg", my_plot3, width = 2, height = 2, dpi = 600)

# * Macro Data

# Define the data frame
df3 <- data.frame(
  Year = c(2024, 2028),
  Inflation = c(2.3, 2.1),
  GDP = c(83062, 95301)
)

# Melt the data frame to long format for plotting
df3_long <- reshape2::melt(df3, id.vars = "Year")

# Load the necessary packages
library(ggplot2)
library(gridExtra)

# Create the plot for Inflation
<<<<<<< HEAD
# Create the plot for Inflation
=======
>>>>>>> 61c20ca (Add new files and update code)
plot_inflation <- ggplot(df3, aes(x = Year, y = Inflation)) +
  geom_line(size = 1, color = "red", linetype = 2) +
  geom_point(size = 3, color = "#820000") +
  ggtitle("% Anual Inflation") + # Add title
  xlab("") + # Remove x label
  ylab("") + # Remove y label
  ylim(min(df3$Inflation - .01), max(df3$Inflation + .01)) + # Set y-axis limits
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "black"), # Make title bold and black
    axis.text = element_text(face = "bold", color = "black"), # Make axis text bold and black
    axis.title = element_text(color = "black")
  ) + # Make axis title black
  geom_label(
    aes(x = min(df3$Year), y = min(df3$Inflation), label = "8.6% DECREASE"),
    hjust = 0, vjust = 0,
    size = 5, fontface = "bold", # Make label text bigger and bolder
    fill = "#4b1227", # Fill label with yellow color
    color = "#ffffff" # Set label text color to black
  )

ggsave("my_plotinflation.png", plot_inflation, width = 4, height = 7, dpi = 600)

# Create the plot for GDP
plot_gdp <- ggplot(df3, aes(x = Year, y = GDP)) +
  geom_line(size = 1, color = "#00ff0d", linetype = 2) +
  geom_point(size = 3, color = "#00670a") +
  ggtitle("GDP per capita (USD)") + # Add title
  xlab("") + # Remove x label
  ylab("") + # Remove y label
  ylim(min(df3$GDP - 500), max(df3$GDP + 500)) + # Set y-axis limits
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", color = "black"), # Make title bold and black
    axis.text = element_text(face = "bold", color = "black"), # Make axis text bold and black
    axis.title = element_text(color = "black")
  ) + # Make axis title black
  geom_label(
    aes(x = min(df3$Year), y = min(df3$GDP), label = "12.84% INCREASE"),
    hjust = -0.4, vjust = 0,
    size = 5, fontface = "bold", # Make label text bigger and bolder
    fill = "#124b19", # Fill label with dark red color
    color = "#ffffff" # Set label text color to white
  )

# Save the combined plot
ggsave("my_plotgdp.jpg", plot_gdp, width = 4, height = 7, dpi = 600)
<<<<<<< HEAD
=======

# * Optimized Vs MSCI USA

# comparison <- data.frame(
#   metric = c(
#     "Annu. Return", "Annu. Risk", "Beta", "Sharpe",
#     "Treynor", "Alpha", "VaR"
#   ),
#   portfolio = c(
#     "48.28%", "26.72%", 0.89, 1.611,
#     0.483, 0.124, "4.195%"
#   ),
#   msci_usa = c(
#     "39.64%", "22.4%", 0.97, 1.536,
#     0.355, 0.010, "2.676%"
#   )
# )

comparison <- data.frame(
  metric = c(
    "Annu. Return", "Annu. Risk", "Beta", "Sharpe",
    "Treynor", "Alpha", "VaR"
  ),
  portfolio = c(
    "45.64%", "27.16%", 1.04, 1.488,
    0.387, 0.206, "4.195%"
  ),
  msci_usa = c(
    "24.20%", "22.35%", 0.97, 0.849,
    0.196, 0.006, "2.676%"
  )
)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert percentages to numeric
comparison$portfolio <- ifelse(grepl("%", comparison$portfolio), as.numeric(sub("%", "", comparison$portfolio)) / 100, as.numeric(comparison$portfolio))
comparison$msci_usa <- ifelse(grepl("%", comparison$msci_usa), as.numeric(sub("%", "", comparison$msci_usa)) / 100, as.numeric(comparison$msci_usa))

# Reshape data from wide to long format
comparison_long <- comparison %>%
  gather(key = "group", value = "value", -metric)

# Define metric groups
group1 <- c("Annu. Return", "Annu. Risk")
group2 <- c("Beta", "Sharpe", "Treynor", "Alpha")
group3 <- c("VaR")

plot_group1 <- ggplot(comparison_long %>% filter(metric %in% group1), aes(x = metric, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label = scales::percent(value), group = group), position = position_dodge(width = 1), vjust = -0.5, color = "white", fill = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("msci_usa" = "#054496", "portfolio" = "#96052e")) +
  xlab(NULL) + ylab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", colour = "black", size = 14),
        axis.text.y = element_text(face = "bold", colour = "black"),
        axis.title = element_text(face = "bold", colour = "black"),
        panel.grid.major = element_line(color = "black", size = 0.2),
        panel.grid.minor = element_line(color = "black", size = 0.2)) +
  guides(fill=FALSE)

# Define a function to format the labels
format_label <- function(metric, value) {
  if (metric %in% c("Beta", "Sharpe", "Treynor", "Alpha")) {
    return(as.character(round(value, 3)))
  } else {
    return(scales::percent(value))
  }
}

# Load necessary library
library(forcats)

# Reorder the factor levels of the metric variable
comparison_long$metric <- fct_relevel(comparison_long$metric, "Alpha", after = Inf)

# Create the plot
plot_group2 <- ggplot(comparison_long %>% filter(metric %in% group2), aes(x = metric, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label = format_label(metric, value), group = group), position = position_dodge(width = 1), vjust = -0.5, color = "white", fill = "black", fontface = "bold") +
  scale_fill_manual(values = c("msci_usa" = "#054496", "portfolio" = "#96052e")) +
  xlab(NULL) + ylab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", colour = "black", size = 14),
    axis.text.y = element_text(face = "bold", colour = "black"),
    axis.title = element_text(face = "bold", colour = "black"),
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_line(color = "black", size = 0.2)) +
  guides(fill=FALSE)

plot_group3 <- ggplot(comparison_long %>% filter(metric %in% group3), aes(x = metric, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label = scales::percent(value), group = group), position = position_dodge(width = 1), vjust = -0.5, color = "white", fill = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("msci_usa" = "#054496", "portfolio" = "#96052e")) +
  xlab(NULL) + ylab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", colour = "black", size = 14),
        axis.text.y = element_text(face = "bold", colour = "black"),
        axis.title = element_text(face = "bold", colour = "black"),
        panel.grid.major = element_line(color = "black", size = 0.2),
        panel.grid.minor = element_line(color = "black", size = 0.2)) +
  guides(fill=FALSE)

# Save the plots
ggsave("my_plotgroup1.jpg", plot_group1, width = 5, height = 7, dpi = 600)
ggsave("my_plotgroup2.jpg", plot_group2, width = 7, height = 7, dpi = 600)
ggsave("my_plotgroup3.jpg", plot_group3, width = 3, height = 7, dpi = 600)

library(gridExtra)

# Combine the plots
combined_plot <- grid.arrange(plot_group1, plot_group2, ncol = 2, widths = c(0.57, 1.08))

# Save the combined plot
ggsave("combined_plot.png", combined_plot, width = 15, height = 8, dpi = 600, bg = "transparent")

# * PLOT WEIGHTS ----------------------------

# Import xlsx df
df_weights <- read.xlsx("./data/weights.xlsx")

head(df_weights)
>>>>>>> 61c20ca (Add new files and update code)
