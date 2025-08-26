library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Load Excel
ExportDependency_data <- read_excel("C:/Users/User/OneDrive/Documents/Chow_research/ExportDependency2018/ExportDependency2018.xlsx", sheet = 1)

# Extract values for all countries
export_values <- data.frame(
  Country = c(
    "Japan", "South Korea", "Brunei Darussalam", "Fiji", 
    "India", "Indonesia", "Cambodia","Laos PDR", "Philippines", 
    "Singapore", "Thailand", "Vietnam", "Malaysia", "Taiwan", "Myanmar"
  ),
  China = c(
    ExportDependency_data[[2]][9],
    ExportDependency_data[[2]][18],
    ExportDependency_data[[2]][27],
    ExportDependency_data[[2]][37],
    ExportDependency_data[[2]][47],
    ExportDependency_data[[2]][57],
    ExportDependency_data[[2]][67],
    ExportDependency_data[[2]][77],
    ExportDependency_data[[2]][87],
    ExportDependency_data[[2]][97],
    ExportDependency_data[[2]][107],
    ExportDependency_data[[2]][117],
    ExportDependency_data[[2]][127],
    ExportDependency_data[[2]][137],
    ExportDependency_data[[2]][147]
  ),
  USA = c(
    ExportDependency_data[[2]][10],
    ExportDependency_data[[2]][19],
    ExportDependency_data[[2]][28],
    ExportDependency_data[[2]][38],
    ExportDependency_data[[2]][48],
    ExportDependency_data[[2]][58],
    ExportDependency_data[[2]][68],
    ExportDependency_data[[2]][78],
    ExportDependency_data[[2]][88],
    ExportDependency_data[[2]][98],
    ExportDependency_data[[2]][108],
    ExportDependency_data[[2]][118],
    ExportDependency_data[[2]][128],
    ExportDependency_data[[2]][138],
    ExportDependency_data[[2]][148]
  )
)

# Convert to numeric (important)
export_values <- export_values %>%
  mutate(across(c(China, USA), ~ round(as.numeric(.), 3)))

# Convert to long format for ggplot
export_long <- export_values %>%
  pivot_longer(cols = c("China", "USA"), names_to = "Market", values_to = "Dependency")

ggplot(export_long, aes(x = Country, y = Dependency, fill = Market)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("China" = "firebrick", "USA" = "green")) +
  labs(
    caption = "Data Source: World Trade Map*",
    title = "Export Dependency on the US and China in 2018",
    y = "Percentage of Export",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  geom_text(
    aes(label = percent(Dependency)),
    position = position_dodge(width = 0.8),
    hjust = -0.1,
    vjust = 0.3,
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.5)) +
  coord_flip()









