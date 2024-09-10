library(shiny)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot2)

# Load the dataset
Adidas_data <- read_excel("Adidas US Sales Datasets.xlsx")

# Data Cleaning and Preparation
Adidas_data$Year <- format(as.Date(Adidas_data$`Invoice Date`), "%Y")
Adidas_data$`Invoice Date` <- as.Date(Adidas_data$`Invoice Date`)
Adidas_data <- Adidas_data %>% filter(`Total Sales` >= 0)
Adidas_data$Region <- as.factor(Adidas_data$Region)
Adidas_data$`Sales Method` <- as.factor(Adidas_data$`Sales Method`)
Adidas_data$Product <- as.factor(Adidas_data$Product)
Adidas_data$Retailer <- as.factor(Adidas_data$Retailer)
Adidas_data$`Total Sales` <- as.numeric(gsub("[^0-9.-]", "", Adidas_data$`Total Sales`))
Adidas_data$`Operating Profit` <- as.numeric(gsub("[^0-9.-]", "", Adidas_data$`Operating Profit`))

# Define UI for application
ui <- navbarPage(
  "Adidas US Sales Dashboard",
  
  tabPanel("Bar Charts",
           fluidPage(
             titlePanel("Bar Charts"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("bar_chart", "Select Bar Chart", 
                             choices = list(
                               "Total Sales by Region" = "sales_region",
                               "Top Retailers by Sales" = "top_retailers",
                               "Sales by Product Category" = "sales_by_product",
                               "Sales by Region and Product Category" = "sales_by_region_product",
                               "Units Sold by Product Category" = "units_sold_by_product",
                               "Operating Profit by Region" = "profit_by_region",
                               "Units Sold by Region" = "units_sold_by_region",
                               "Operating Margin by Region" = "margin_by_region"
                             ))
               ),
               mainPanel(
                 plotlyOutput("bar_output")
               )
             )
           )
  ),
  
  tabPanel("Pie Charts",
           fluidPage(
             titlePanel("Pie Charts"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("pie_chart", "Select Pie Chart", 
                             choices = list(
                               "Region-wise Sales Distribution" = "region_sales",
                               "Sales Method Comparison" = "sales_method"
                             ))
               ),
               mainPanel(
                 plotlyOutput("pie_output")
               )
             )
           )
  ),
  
  tabPanel("Line Charts",
           fluidPage(
             titlePanel("Line Charts"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("line_chart", "Select Line Chart",
                             choices = list(
                               "Total Sales Trend" = "sales_trend",
                               "Sales Trend Over Time (ggplot2)" = "gg_sales_trend"
                             ))
               ),
               mainPanel(
                 plotlyOutput("line_output")
               )
             )
           )
  ),
  
  tabPanel("Box Plots",
           fluidPage(
             titlePanel("Box Plots"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("box_chart", "Select Box Plot",
                             choices = list(
                               "Sales Distribution by Region" = "sales_distribution_region",
                               "Sales Distribution by Product" = "sales_distribution_product",
                               "Operating Margin by Region (ggplot2)" = "gg_operating_margin_by_region"
                             ))
               ),
               mainPanel(
                 plotlyOutput("box_output")
               )
             )
           )
  ),
  
  tabPanel("Heatmaps",
           fluidPage(
             titlePanel("Heatmaps"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("heatmap", "Select Heatmap", 
                             choices = list(
                               "Sales by Product and Region" = "sales_by_product_region",
                               "Sales by Retailer and Region" = "sales_by_retailer_region"
                             ))
               ),
               mainPanel(
                 plotlyOutput("heatmap_output")
               )
             )
           )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  
  # Bar Chart Outputs
  output$bar_output <- renderPlotly({
    switch(input$bar_chart,
           "sales_region" = {
             total_sales_region <- Adidas_data %>%
               group_by(Region) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = total_sales_region,
               x = ~Region,
               y = ~Total_Sales,
               type = 'bar',
               name = 'Total Sales by Region',
               marker = list(color = 'skyblue')
             ) %>%
               layout(
                 title = 'Total Sales by Region',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Total Sales'),
                 showlegend = FALSE
               )
           },
           "top_retailers" = {
             top_retailers <- Adidas_data %>%
               group_by(Retailer) %>%
               summarise(Total_Sales = sum(`Total Sales`)) %>%
               arrange(desc(Total_Sales)) %>%
               head(10)
             
             plot_ly(
               data = top_retailers,
               x = ~reorder(Retailer, -Total_Sales),
               y = ~Total_Sales,
               type = 'bar',
               name = 'Total Sales',
               marker = list(color = 'purple')
             ) %>%
               layout(
                 title = 'Top Retailers by Sales',
                 xaxis = list(title = 'Retailer'),
                 yaxis = list(title = 'Total Sales'),
                 showlegend = FALSE
               )
           },
           "sales_by_product" = {
             sales_by_product <- Adidas_data %>%
               group_by(Product) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_by_product,
               x = ~Product,
               y = ~Total_Sales,
               type = 'bar',
               name = 'Sales by Product Category',
               marker = list(color = 'orange')
             ) %>%
               layout(
                 title = 'Sales by Product Category',
                 xaxis = list(title = 'Product Category'),
                 yaxis = list(title = 'Total Sales'),
                 showlegend = FALSE
               )
           },
           "sales_by_region_product" = {
             sales_by_region_product <- Adidas_data %>%
               group_by(Region, Product) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_by_region_product,
               x = ~Region,
               y = ~Total_Sales,
               color = ~Product,
               type = 'bar',
               barmode = 'stack'
             ) %>%
               layout(
                 title = 'Sales by Region and Product Category',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Total Sales'),
                 showlegend = TRUE
               )
           },
           "units_sold_by_product" = {
             units_sold_by_product <- Adidas_data %>%
               group_by(Product) %>%
               summarise(Units_Sold = sum(`Units Sold`))
             
             plot_ly(
               data = units_sold_by_product,
               x = ~Product,
               y = ~Units_Sold,
               type = 'bar',
               name = 'Units Sold by Product Category',
               marker = list(color = 'teal')
             ) %>%
               layout(
                 title = 'Units Sold by Product Category',
                 xaxis = list(title = 'Product Category'),
                 yaxis = list(title = 'Units Sold'),
                 showlegend = FALSE
               )
           },
           "profit_by_region" = {
             profit_by_region <- Adidas_data %>%
               group_by(Region) %>%
               summarise(Total_Profit = sum(`Operating Profit`))
             
             plot_ly(
               data = profit_by_region,
               x = ~Region,
               y = ~Total_Profit,
               type = 'bar',
               name = 'Operating Profit by Region',
               marker = list(color = 'firebrick')
             ) %>%
               layout(
                 title = 'Operating Profit by Region',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Operating Profit'),
                 showlegend = FALSE
               )
           },
           "units_sold_by_region" = {
             units_sold_by_region <- Adidas_data %>%
               group_by(Region) %>%
               summarise(Units_Sold = sum(`Units Sold`))
             
             plot_ly(
               data = units_sold_by_region,
               x = ~Region,
               y = ~Units_Sold,
               type = 'bar',
               name = 'Units Sold by Region',
               marker = list(color = 'darkorchid')
             ) %>%
               layout(
                 title = 'Units Sold by Region',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Units Sold'),
                 showlegend = FALSE
               )
           },
           "margin_by_region" = {
             margin_by_region <- Adidas_data %>%
               group_by(Region) %>%
               summarise(Operating_Margin = mean(`Operating Margin`))
             
             plot_ly(
               data = margin_by_region,
               x = ~Region,
               y = ~Operating_Margin,
               type = 'bar',
               name = 'Operating Margin by Region',
               marker = list(color = 'gold')
             ) %>%
               layout(
                 title = 'Operating Margin by Region',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Operating Margin'),
                 showlegend = FALSE
               )
           }
    )
  })
  
  # Pie Chart Outputs
  output$pie_output <- renderPlotly({
    switch(input$pie_chart,
           "region_sales" = {
             region_sales <- Adidas_data %>%
               group_by(Region) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = region_sales,
               labels = ~Region,
               values = ~Total_Sales,
               type = 'pie',
               textinfo = 'label+percent',
               insidetextorientation = 'radial'
             ) %>%
               layout(
                 title = 'Region-wise Sales Distribution'
               )
           },
           "sales_method" = {
             sales_method <- Adidas_data %>%
               group_by(`Sales Method`) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_method,
               labels = ~`Sales Method`,
               values = ~Total_Sales,
               type = 'pie',
               textinfo = 'label+percent',
               insidetextorientation = 'radial'
             ) %>%
               layout(
                 title = 'Sales Method Comparison'
               )
           }
    )
  })
  
  # Line Chart Outputs
  output$line_output <- renderPlotly({
    switch(input$line_chart,
           "sales_trend" = {
             sales_trend <- Adidas_data %>%
               group_by(`Invoice Date`) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_trend,
               x = ~`Invoice Date`,
               y = ~Total_Sales,
               type = 'scatter',
               mode = 'lines',
               line = list(color = 'blue')
             ) %>%
               layout(
                 title = 'Total Sales Trend',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Total Sales')
               )
           },
           "gg_sales_trend" = {
             gg_sales_trend <- Adidas_data %>%
               group_by(`Invoice Date`) %>%
               summarise(Total_Sales = sum(`Total Sales`)/1e6)  # Convert to millions
             
             p <- ggplot(data = gg_sales_trend, aes(x = `Invoice Date`, y = Total_Sales)) +
               geom_line(color = 'darkgreen') +
               labs(title = "Sales Trend Over Time", x = "Date", y = "Total Sales (Million)") +  # Adjust y-axis label
               theme_minimal()
             
             ggplotly(p)
             
           }
    )
  })
  
  # Box Plot Outputs
  output$box_output <- renderPlotly({
    switch(input$box_chart,
           "sales_distribution_region" = {
             plot_ly(
               data = Adidas_data,
               x = ~Region,
               y = ~`Total Sales`,
               type = 'box',
               name = 'Sales Distribution by Region',
               marker = list(color = 'lightblue')
             ) %>%
               layout(
                 title = 'Sales Distribution by Region',
                 xaxis = list(title = 'Region'),
                 yaxis = list(title = 'Total Sales')
               )
           },
           "sales_distribution_product" = {
             plot_ly(
               data = Adidas_data,
               x = ~Product,
               y = ~`Total Sales`,
               type = 'box',
               name = 'Sales Distribution by Product',
               marker = list(color = 'lightgreen')
             ) %>%
               layout(
                 title = 'Sales Distribution by Product',
                 xaxis = list(title = 'Product Category'),
                 yaxis = list(title = 'Total Sales')
               )
           },
           "gg_operating_margin_by_region" = {
             p <- ggplot(data = Adidas_data, aes(x = Region, y = `Operating Margin`)) +
               geom_boxplot(fill = 'lightcoral') +
               labs(title = "Operating Margin by Region", x = "Region", y = "Operating Margin") +
               theme_minimal()
             
             ggplotly(p)
           }
    )
  })
  
  # Heatmap Outputs
  output$heatmap_output <- renderPlotly({
    switch(input$heatmap,
           "sales_by_product_region" = {
             sales_by_product_region <- Adidas_data %>%
               group_by(Product, Region) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_by_product_region,
               x = ~Product,
               y = ~Region,
               z = ~Total_Sales,
               type = 'heatmap',
               colorscale = 'Viridis'
             ) %>%
               layout(
                 title = 'Sales by Product and Region',
                 xaxis = list(title = 'Product'),
                 yaxis = list(title = 'Region')
               )
           },
           "sales_by_retailer_region" = {
             sales_by_retailer_region <- Adidas_data %>%
               group_by(Retailer, Region) %>%
               summarise(Total_Sales = sum(`Total Sales`))
             
             plot_ly(
               data = sales_by_retailer_region,
               x = ~Retailer,
               y = ~Region,
               z = ~Total_Sales,
               type = 'heatmap',
               colorscale = 'Cividis'
             ) %>%
               layout(
                 title = 'Sales by Retailer and Region',
                 xaxis = list(title = 'Retailer'),
                 yaxis = list(title = 'Region')
               )
           }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
