library(tidyverse)
library(lubridate)
library(readxl)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(plotly)


# Read the data from a CSV file
Data <- read.csv("Supermarket_Sales.csv")
str(Data) # structure of dataset
summary(Data) # Summary stats

# Data Processing and Visualization code...
#Checking for NAs within each column/variable

unique_value <- unique(Data$Invoice.ID)
print(unique_value)

uni <- length(unique(Data$Invoice.ID))
print(uni)

#Drop unique column
Data <- subset(Data, select = -Invoice.ID)


#remove nulls
sum_na <- colSums(is.na(Data))

# Display the results
print(sum_na)


# Check for missing values
missing_value <- colSums(is.na(Data))
print(missing_value)

Data <- Data[complete.cases(Data), ]

print(missing_value)


#Checking for duplicated rows
n_duplicates <- sum(duplicated(Data))
print(n_duplicates)

# Define UI
ui <- fluidPage(
  # Dark/Light Mode Toggle
  useShinyjs(),
  includeCSS("styles.css"),
  titlePanel("Supermarket Sales Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("branchSelector", "Select Branch", choices = unique(Data$Branch)),
      checkboxGroupInput("visualizationsSelector", "Select Visualizations",
                         choices = c("Customer Types", "Gender Distribution", "Product Line Distribution",
                                     "Payment Method Distribution", "Ratings Distribution",
                                     "Total Bill Histogram", "Daily Growth Rate", "Rating by Payment Method","Scatter Plot of Total vs. Rating","City Chart"),
                         selected = c("Customer Types", "Gender Distribution")),
      dateRangeInput("dateRange", "Select Date Range", start = min(Data$Date), end = max(Data$Date)),
      sliderInput("minRating", "Minimum Rating", min = 1, max = 5, value = 1, step = 0.1),
      textInput("productNameFilter", "Product Name Contains"),
      selectInput("paymentMethodSelector", "Select Payment Method", choices = unique(Data$Payment)),
      radioButtons("value_type", label = "Select Value Type:",
                   choices = c("Absolute", "Percentage"),
                   selected="Absolute"),
      actionButton("toggleMode", "Toggle Light/Dark Mode")
    ),
    mainPanel(
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Customer Types") > -1',
        plotlyOutput("customerTypesPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Gender Distribution") > -1',
        plotOutput("genderDistributionPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("City Chart") > -1',
        plotlyOutput("pie_chart")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Product Line Distribution") > -1',
        plotlyOutput("productLineDistributionPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Payment Method Distribution") > -1',
        plotlyOutput("paymentMethodDistributionPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Ratings Distribution") > -1',
        plotOutput("ratingsDistributionPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Total Bill Histogram") > -1',
        plotlyOutput("totalBillHistogram")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Daily Growth Rate") > -1',
        plotlyOutput("dailyGrowthRatePlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Rating by Payment Method") > -1',
        plotlyOutput("ratingByPaymentMethodPlot")
      ),
      conditionalPanel(
        condition = 'input.visualizationsSelector.indexOf("Scatter Plot of Total vs. Rating") > -1',
        plotlyOutput("totalRatingScatterPlot")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive expression to filter data based on the selected branch, gender, and date range
  filteredData <- reactive({
    req(input$branchSelector)  # Make sure a branch is selected before filtering
    
    data <- Data[Data$Branch == input$branchSelector, ]
    if (!is.null(input$genderSelector) && length(input$genderSelector) > 0) {
      data <- data[data$Gender %in% input$genderSelector, ]
    }
    data <- data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2], ]
    data <- data[data$Rating >= input$minRating, ]
    if (!is.null(input$productNameFilter) && input$productNameFilter != "") {
      data <- data[str_detect(data$Product.line, input$productNameFilter), ]
    }
    if (!is.null(input$paymentMethodSelector) && length(input$paymentMethodSelector) > 0) {
      data <- data[data$Payment %in% input$paymentMethodSelector, ]
    }
    data
  })
  
  # Reactive expression to calculate the unit conversion factor based on the selected unit type
  unitConversionFactor <- reactive({
    if (input$unitTypeSelector == "Percentage Change") {
      ratings_diff <- c(NA, diff(filteredData()$Rating))
      ratings_diff[is.na(filteredData()$Rating) | is.na(lag(filteredData()$Rating))] <- NA
      percentage_change <- ratings_diff / lag(filteredData()$Rating) * 100
      return(percentage_change)
    } else {
      return(rep(1, nrow(filteredData())))
    }
  })
  
  # Customer Types plot
  output$customerTypesPlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Customer.type, fill = Customer.type)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, color = 'black', size = 4) +  
      ggtitle("Distribution of Customer Types")
  })
  
  output$pie_chart <- renderPlotly({
    if ("City Chart" %in% input$visualizationsSelector) {
      data <- table(Data$City)  
      
      if (input$value_type == "Absolute") {
        labels <- names(data)
        values <- as.numeric(data)
        text <- paste(labels, "<br>", "Count: ", values)
      } else {
        data_percentage <- prop.table(data) * 100
        labels <- names(data_percentage)
        values <- as.numeric(data_percentage)
        text <- paste(labels, "<br>", "Percentage: ", round(values, 2), "%")
      }
      
      p <- plot_ly(labels = labels, values = values, type = 'pie', text = text,
                   hoverinfo = "text") %>%
        layout(title = "City Chart")
      
      p  # Return Plotly object
    }
  })
  
  # Gender Distribution plot
  output$genderDistributionPlot <- renderPlot({
    gender_counts <- table(filteredData()$Gender)
    total <- sum(gender_counts)
    gender_percentages <- round((gender_counts / total) * 100, 1)
    pie(gender_counts, labels = paste(names(gender_counts), "\n", gender_percentages, "%"), col = c("blue", "pink"))
  })
  
  # Product Line Distribution plot
  output$productLineDistributionPlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Product.line, fill = Product.line)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, color = 'black', size = 4) +  
      ggtitle("Product Line Distribution") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Payment Method Distribution plot
  output$paymentMethodDistributionPlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Payment, fill = Payment)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, color = 'black', size = 4) +  
      ggtitle("Payment Method Distribution")
  })
  
  # Distribution of Ratings plot
  output$ratingsDistributionPlot <- renderPlot({
    ggplot(filteredData(), aes(x = Rating , fill = factor(Rating))) +
      geom_bar(width = 0.07) + 
      ggtitle("Distribution of Ratings")
  })
  
  # Total Bill Histogram plot
  output$totalBillHistogram <- renderPlotly({
    ggplot(data = filteredData(), aes(x = Total)) +
      geom_histogram(binwidth = 15, fill = "#5B6A87", colour = "Black") +
      xlab("Total bill prices") + ylab("Count of bills") +
      theme(axis.title.x = element_text(colour = "red", size = 10), axis.title.y = element_text(colour = "Red", size = 10)) +
      ggtitle("Total bill")
  })
  
  # Daily Growth Rate plot
  output$dailyGrowthRatePlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Date, y = Total)) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Daily Growth Rate", x = "Date", y = "Growth Rate (%)")
  })
  
  # Rating by Payment Method plot
  output$ratingByPaymentMethodPlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Payment, y = Rating)) +
      geom_boxplot() +
      labs(title = "Distribution of Rating by Payment Method", x = "Payment Method", y = "Rating")
  })
  
  # Scatter plot of Total vs. Rating
  output$totalRatingScatterPlot <- renderPlotly({
    ggplot(filteredData(), aes(x = Rating, y = Total, color = Rating)) +
      geom_point() +
      labs(title = "Scatter Plot of Total vs. Rating", x = "Rating", y = "Total") +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()
  })
  
  observeEvent(input$toggleMode, {
    shinyjs::runjs('
      var mode = localStorage.getItem("mode");
      if (mode === "light") {
        document.body.classList.add("dark-mode");
        localStorage.setItem("mode", "dark");
      } else {
        document.body.classList.remove("dark-mode");
        localStorage.setItem("mode", "light");
      }
    ')
  })
  
}

shinyApp(ui, server)