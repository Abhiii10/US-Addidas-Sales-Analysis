# Load necessary libraries
library(plotly)
library(readxl)
library(dplyr)
library(usmap) # For creating map visualizations
library(ggplot2) # For creating visualizations
library(lubridate) # For working with dates
library(tidyr) # For data tidying (includes unite function)

# Load the dataset
Adidas_data <- read_excel("C:/Users/Zenbook/Desktop/New folder/Abhishek Paudel-23140737/Adidas US Sales Datasets.xlsx")

# Data Cleaning and Preparation
Adidas_data$Year <- year(Adidas_data$`Invoice Date`)
Adidas_data$`Invoice Date` <- as.Date(Adidas_data$`Invoice Date`)
Adidas_data <- Adidas_data %>% filter(`Total Sales` >= 0) 
Adidas_data$Region <- as.factor(Adidas_data$Region)
Adidas_data$`Sales Method` <- as.factor(Adidas_data$`Sales Method`)
Adidas_data$Product <- as.factor(Adidas_data$Product)
Adidas_data$Retailer <- as.factor(Adidas_data$Retailer)
Adidas_data$`Total Sales` <- as.numeric(gsub("[^0-9.-]", "", Adidas_data$`Total Sales`))
Adidas_data$`Operating Profit` <- as.numeric(gsub("[^0-9.-]", "", Adidas_data$`Operating Profit`))

# Create a new variable for product category
Adidas_data <- Adidas_data %>%
  mutate(
    `Product Category` = case_when(
      grepl("Street Footwear", Product) ~ "Street Footwear",
      grepl("Athletic Footwear", Product) ~ "Athletic Footwear",
      grepl("Apparel", Product) ~ "Apparel",
      TRUE ~ "Other"
    )
  )

# Ensure state names match those expected by usmap
state_mapping <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
            "Connecticut", "Delaware", "District of Columbia", "Florida", 
            "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
            "Massachusetts", "Michigan", "Minnesota", "Mississippi",
            "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", 
            "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
            "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
            "Wisconsin", "Wyoming"),
  stringsAsFactors = FALSE
)

Adidas_data <- Adidas_data %>%
  left_join(state_mapping, by = c("State" = "State"))

# --- Sales Analysis ---

# 1. Total Sales by Region (Bar Chart)
total_sales_region <- Adidas_data %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(total_sales_region, aes(x = Region, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("total_sales_region.png", plot = plot_object, width = 10, height = 6)

# 2. Sales Trend Over Time (Line Chart)
sales_trend <- Adidas_data %>%
  group_by(`Invoice Date`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_trend, aes(x = `Invoice Date`, y = Total_Sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trend Over Time", x = "Date", y = "Total Sales (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("sales_trend_over_time.png", plot = plot_object, width = 10, height = 6)

# 3. Sales by Product Category (Stacked Bar Chart)
sales_by_category <- Adidas_data %>%
  group_by(Region, `Product Category`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_by_category, aes(x = Region, y = Total_Sales, fill = `Product Category`)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Product Category and Region", 
       x = "Region", 
       y = "Total Sales (Million)", 
       fill = "Product Category") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16)
  ) -> plot_object

ggsave("sales_by_category.png", plot = plot_object, width = 10, height = 6)

# 4. Sales by Retailer (Stacked Bar Chart)
sales_by_retailer <- Adidas_data %>%
  group_by(Retailer) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_by_retailer, aes(x = Retailer, y = Total_Sales, fill = Retailer)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Retailer", x = "Retailer", y = "Total Sales (Million)", fill = "Retailer") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("sales_by_retailer.png", plot = plot_object, width = 10, height = 6)

# 5. Sales by Sales Method (Stacked Bar Chart)
sales_by_sales_method <- Adidas_data %>%
  group_by(`Sales Method`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_by_sales_method, aes(x = `Sales Method`, y = Total_Sales, fill = `Sales Method`)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Sales Method", x = "Sales Method", y = "Total Sales (Million)", fill = "Sales Method") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("sales_by_sales_method.png", plot = plot_object, width = 10, height = 6)

# 6. Sales Breakdown by Product Category (Pie Chart)
sales_by_category <- Adidas_data %>%
  group_by(`Product Category`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6)  # Convert to Millions

plot_object <- ggplot(sales_by_category, aes(x = "", y = Total_Sales, fill = `Product Category`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Sales Breakdown by Product Category", x = "", y = "Total Sales (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16)
  )

ggsave("sales_by_category.png", plot = plot_object, width = 10, height = 6)

# 7. Quarterly Sales Trend by Region (Line Chart)
sales_trend_by_quarter_region <- Adidas_data %>%
  mutate(Quarter = quarter(`Invoice Date`), Year = year(`Invoice Date`)) %>%
  group_by(Region, Year, Quarter) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6)  # Convert to Millions

sales_trend_by_quarter_region <- sales_trend_by_quarter_region %>%
  unite("Year_Quarter", Year, Quarter, sep = " ")

plot_object <- ggplot(sales_trend_by_quarter_region, aes(x = Year_Quarter, y = Total_Sales, color = Region)) +
  geom_line() +
  labs(title = "Quarterly Sales Trend by Region", 
       x = "Year-Quarter", 
       y = "Total Sales (Million)", 
       color = "Region") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  )

ggsave("quarterly_sales_trend_by_region.png", plot = plot_object, width = 10, height = 6)

# --- Profitability Analysis ---

# 1. Operating Profit by Region (Bar Chart)
profit_by_region <- Adidas_data %>%
  group_by(Region) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_region, aes(x = Region, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Total Operating Profit by Region", x = "Region", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("profit_by_region.png", plot = plot_object, width = 10, height = 6)

# 2. Operating Profit by Sales Method (Bar Chart)
profit_by_sales_method <- Adidas_data %>%
  group_by(`Sales Method`) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_sales_method, aes(x = `Sales Method`, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Total Operating Profit by Sales Method", x = "Sales Method", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("profit_by_sales_method.png", plot = plot_object, width = 10, height = 6)

# 3. Operating Profit by Product (Bar Chart)
profit_by_product <- Adidas_data %>%
  group_by(Product) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_product, aes(x = Product, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Operating Profit by Product", x = "Product", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("profit_by_product.png", plot = plot_object, width = 10, height = 6)

# 4. Operating Profit by Retailer (Bar Chart)
profit_by_retailer <- Adidas_data %>%
  group_by(Retailer) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_retailer, aes(x = Retailer, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Total Operating Profit by Retailer", x = "Retailer", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("profit_by_retailer.png", plot = plot_object, width = 10, height = 6)

# 5. Operating Margin by Sales Method (Box Plot)
ggplot(Adidas_data, aes(x = `Sales Method`, y = `Operating Margin`, fill = `Sales Method`)) +
  geom_boxplot() +
  labs(title = "Operating Margin by Sales Method", x = "Sales Method", y = "Operating Margin") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("operating_margin_by_sales_method.png", plot = plot_object, width = 10, height = 6)

# 6. Operating Margin by Region (Box Plot)
ggplot(Adidas_data, aes(x = Region, y = `Operating Margin`, fill = Region)) +
  geom_boxplot() +
  labs(title = "Operating Margin by Region", x = "Region", y = "Operating Margin") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("operating_margin_by_region.png", plot = plot_object, width = 10, height = 6)

# 7. Operating Margin by Product (Box Plot)
ggplot(Adidas_data, aes(x = Product, y = `Operating Margin`, fill = Product)) +
  geom_boxplot() +
  labs(title = "Operating Margin by Product", x = "Product", y = "Operating Margin") +
  theme(
    axis.text = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16) 
  ) -> plot_object

ggsave("operating_margin_by_product.png", plot = plot_object, width = 12, height = 9)

# --- Interactive Visualizations (Plotly) ---

# Example: Interactive Sales Trend by Region
sales_trend_by_quarter_region <- Adidas_data %>%
  mutate(Quarter = quarter(`Invoice Date`), Year = year(`Invoice Date`)) %>%
  group_by(Region, Year, Quarter) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6)  # Convert to Millions

sales_trend_by_quarter_region <- sales_trend_by_quarter_region %>%
  unite("Year_Quarter", Year, Quarter, sep = " ")

plot_object <- plot_ly(sales_trend_by_quarter_region, x = ~Year_Quarter, y = ~Total_Sales, color = ~Region, type = 'scatter', mode = 'lines') %>%
  layout(title = "Quarterly Sales Trend by Region (Interactive)",
         xaxis = list(title = "Year-Quarter"),
         yaxis = list(title = "Total Sales (Million)"),
         legend = list(title = list(text = "Region")))

htmlwidgets::saveWidget(as_widget(plot_object), "sales_trend_by_quarter_region_interactive.html")
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Data manipulation
quarterly_sales <- Adidas_data %>%
  mutate(Quarter = quarters(`Invoice Date`), Year = as.numeric(format(`Invoice Date`, "%Y"))) %>%
  group_by(Region, Year, Quarter) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) %>%
  arrange(Year, Quarter)

# Generate all combinations of Region, Year, and Quarter
all_combinations <- expand.grid(
  Region = unique(Adidas_data$Region),
  Year = unique(as.numeric(format(Adidas_data$`Invoice Date`, "%Y"))),
  Quarter = unique(quarters(Adidas_data$`Invoice Date`))
)

# Merge with the actual sales data to ensure all combinations are included
quarterly_sales <- merge(all_combinations, quarterly_sales, by = c("Region", "Year", "Quarter"), all.x = TRUE)
quarterly_sales[is.na(quarterly_sales)] <- 0  # Replace NAs with 0

# Facet plot
ggplot(quarterly_sales, aes(x = interaction(Year, Quarter), y = Total_Sales, fill = Region)) +
  geom_col() +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Quarterly Sales Trend by Region", x = "Quarter-Year", y = "Total Sales (Million)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.text.y = element_text(size = 14),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 16),
    legend.position = "none"
  ) -> plot_object

ggsave("quarterly_sales_trend_by_region_facet.png", plot = plot_object, width = 10, height = 6)



# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)

# Data manipulation
category_sales <- Adidas_data %>%
  group_by(`Product Category`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) %>%
  arrange(desc(Total_Sales)) %>%
  mutate(Percentage = Total_Sales / sum(Total_Sales) * 100) %>%
  mutate(Label = paste0(" (", round(Percentage, 1), "%)"))

# Create a pie chart
ggplot(category_sales, aes(x = "", y = Total_Sales, fill = `Product Category`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Breakdown of Sales by Product Category", x = "", y = "Total Sales (Million)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5) -> plot_object

# Save the pie chart
ggsave("breakdown_by_product_category_pie_chart.png", plot = plot_object, width = 10, height = 6)
install.packages("bbc_style")
