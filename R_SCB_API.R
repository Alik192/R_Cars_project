library(pxweb)
library(dplyr)
library(ggplot2)
library(scales)

url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel"

# Get metadata (to explore valid options)
meta <- pxweb_get(url)

# Extract available values
region_values <- meta$variables[[1]]$values 
drivmedel_values <- meta$variables[[2]]$values 
contents_code <- meta$variables[[3]]$values[1]  
tid_values <- meta$variables[[4]]$values        
# Filter monthly values from 2021 to 2024 only
tid_filtered <- tid_values[grepl("^202[1-4]M", tid_values)]




# Build the query
query <- list(
  Region = region_values,        # All regions
  Drivmedel = drivmedel_values,  # All fuel types
  ContentsCode = contents_code,  # One contents code (required)
  Tid = tid_filtered             # Months from 2021 to 2025
)

# Run the API query
data <- pxweb_get(url = url, query = query)

# Convert to a data frame
df_api <- as.data.frame(data)

# View the first few rows
head(df_api,500)

head(df_api$månad)

# Ensure 'year' column is correctly created
df_api <- df_api %>%
  mutate(year = substr(månad, 1, 4))

# Check the unique values in the 'year' column again
unique(df_api$year)



# Group by year and fuel type (Drivmedel)
trend_by_year <- df_api %>%
  filter(year %in% c("2021", "2022", "2023", "2024", "2025")) %>%
  group_by(year, drivmedel) %>%
  summarise(Total = sum(Antal, na.rm = TRUE)) %>%
  ungroup()

# Plot the data
ggplot(trend_by_year, aes(x = year, y = Total, color = drivmedel, group = drivmedel)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Yearly Trend of Cars by Fuel Type in Sweden (2021–2025)",
    x = "Year",
    y = "Number of Cars",
    color = "Fuel Type"
  ) +
  scale_y_continuous(labels = comma) + 
  theme_minimal()





# Group by fuel type and summarize
fuel_summary <- df_api %>%
  group_by(drivmedel) %>%
  summarise(Total = sum(Antal, na.rm = TRUE)) %>%
  arrange(desc(Total))

# View it (optional)
print(fuel_summary)

# Plot
png("my_plot.png", width = 800, height = 600)
ggplot(fuel_summary, aes(x = reorder(drivmedel, Total), y = Total, fill = drivmedel)) +
  geom_col() +
  coord_flip() +  # Flip for readability if many fuel types
  labs(
    title = "Total Number of Cars by Fuel Type (2021–2025)",
    x = "Fuel Type",
    y = "Total Number of Cars"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()
dev.off()






# Summarize total cars per region
top_regions <- df_api %>%
  group_by(region) %>%
  summarise(Total = sum(Antal, na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10)  # Top 10 regions

# Plot
ggplot(top_regions, aes(x = reorder(region, Total), y = Total, fill = region)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 Regions by Number of Cars (2021–2025)",
    x = "Region",
    y = "Total Number of Cars"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


yearly_total <- df_api %>%
  filter(region == "Riket") %>%
  mutate(year = substr(månad, 1, 4)) %>%  # Extract year from 'månad' like "2021M01" → "2021"
  group_by(year) %>%
  summarise(Total = sum(Antal, na.rm = TRUE))

# Plot
ggplot(yearly_total, aes(x = year, y = Total)) +
  geom_col(fill = "#2E86AB") +
  labs(
    title = "Total Number of Registered Cars per Year (Sweden)",
    x = "Year",
    y = "Number of Cars"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


