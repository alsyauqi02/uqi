# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(reshape2)
library(DT)
library(plotly)

# Data penjualan Samsung Seri Flagship
data_flagship <- data.frame(
  Tahun = c(2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2016,
            2017, 2017, 2017, 2018, 2018, 2018, 2019, 2019, 2019,
            2020, 2020, 2020, 2021, 2021, 2021, 2022, 2022, 2022,
            2023, 2023, 2023, 2024, 2024, 2024),
  Negara = c("Indonesia", "USA", "Jerman", "Indonesia", "USA", "Inggris",
             "Indonesia", "USA", "China", "Indonesia", "USA", "Korea",
             "Indonesia", "USA", "India", "Indonesia", "USA", "Brazil",
             "Indonesia", "USA", "Rusia", "Indonesia", "USA", "Kanada",
             "Indonesia", "USA", "Jerman", "Indonesia", "USA", "Korea",
             "Indonesia", "USA", "Inggris"),
  Seri = c("Galaxy S5", "Galaxy S5", "Galaxy S5", "Galaxy S6", "Galaxy S6", "Galaxy S6",
           "Galaxy S7", "Galaxy S7", "Galaxy S7", "Galaxy S8", "Galaxy S8", "Galaxy S8",
           "Galaxy S9", "Galaxy S9", "Galaxy S9", "Galaxy S10", "Galaxy S10", "Galaxy S10",
           "Galaxy S20", "Galaxy S20", "Galaxy S20", "Galaxy S21", "Galaxy S21", "Galaxy S21",
           "Galaxy Z Fold", "Galaxy Z Fold", "Galaxy Z Fold", "Galaxy Z Fold 2", "Galaxy Z Fold 2", "Galaxy Z Fold 2",
           "Galaxy Z Fold 3", "Galaxy Z Fold 3", "Galaxy Z Fold 3"),
  Kuartil1 = c(500000, 1000000, 300000, 550000, 1050000, 400000,
               600000, 1200000, 1500000, 650000, 1250000, 1100000,
               700000, 1300000, 1000000, 750000, 1350000, 1100000,
               800000, 1400000, 1200000, 850000, 1450000, 1100000,
               900000, 1500000, 1200000, 950000, 1600000, 1300000,
               1000000, 1700000, 1400000),
  Kuartil2 = c(450000, 950000, 350000, 500000, 1000000, 380000,
               550000, 1150000, 1400000, 600000, 1200000, 1050000,
               650000, 1250000, 950000, 700000, 1300000, 1050000,
               750000, 1350000, 1150000, 800000, 1400000, 1050000,
               850000, 1450000, 1150000, 900000, 1550000, 1250000,
               950000, 1650000, 1350000),
  Kuartil3 = c(600000, 900000, 400000, 600000, 1100000, 450000,
               700000, 1100000, 1300000, 750000, 1150000, 1200000,
               800000, 1200000, 1100000, 850000, 1250000, 1200000,
               900000, 1300000, 1300000, 950000, 1350000, 1200000,
               1000000, 1400000, 1300000, 1050000, 1450000, 1350000,
               1100000, 1550000, 1400000),
  Kuartil4 = c(700000, 1000000, 500000, 750000, 1200000, 550000,
               800000, 1250000, 1500000, 850000, 1300000, 1250000,
               900000, 1350000, 1200000, 950000, 1400000, 1250000,
               1000000, 1450000, 1350000, 1050000, 1500000, 1300000,
               1100000, 1550000, 1400000, 1150000, 1600000, 1450000,
               1200000, 1700000, 1500000)
)

# Data penjualan Samsung Seri Midrange
data_midrange <- data.frame(
  Tahun = c(2014, 2014, 2014, 2015, 2015, 2015, 2016, 2016, 2016,
            2017, 2017, 2017, 2018, 2018, 2018, 2019, 2019, 2019,
            2020, 2020, 2020, 2021, 2021, 2021, 2022, 2022, 2022,
            2023, 2023, 2023, 2024, 2024, 2024),
  Negara = c("Indonesia", "USA", "Jerman", "Indonesia", "USA", "Inggris",
             "Indonesia", "USA", "China", "Indonesia", "USA", "Korea",
             "Indonesia", "USA", "India", "Indonesia", "USA", "Brazil",
             "Indonesia", "USA", "Rusia", "Indonesia", "USA", "Kanada",
             "Indonesia", "USA", "Jerman", "Indonesia", "USA", "Korea",
             "Indonesia", "USA", "Inggris"),
  Seri = c("Galaxy A3", "Galaxy A3", "Galaxy A3", "Galaxy A5", "Galaxy A5", "Galaxy A5",
           "Galaxy A7", "Galaxy A7", "Galaxy A7", "Galaxy A8", "Galaxy A8", "Galaxy A8",
           "Galaxy A9", "Galaxy A9", "Galaxy A9", "Galaxy A10", "Galaxy A10", "Galaxy A10",
           "Galaxy A20", "Galaxy A20", "Galaxy A20", "Galaxy A21", "Galaxy A21", "Galaxy A21",
           "Galaxy A30", "Galaxy A30", "Galaxy A30", "Galaxy A40", "Galaxy A40", "Galaxy A40",
           "Galaxy A50", "Galaxy A50", "Galaxy A50"),
  Kuartil1 = c(200000, 400000, 150000, 250000, 450000, 200000,
               300000, 500000, 600000, 350000, 550000, 400000,
               400000, 600000, 500000, 450000, 650000, 500000,
               500000, 700000, 600000, 550000, 750000, 600000,
               600000, 800000, 700000, 650000, 850000, 750000,
               700000, 900000, 800000),
  Kuartil2 = c(180000, 380000, 130000, 230000, 420000, 180000,
               280000, 480000, 580000, 330000, 530000, 380000,
               380000, 580000, 480000, 420000, 630000, 480000,
               480000, 680000, 580000, 520000, 730000, 580000,
               580000, 780000, 680000, 620000, 830000, 730000,
               680000, 880000, 780000),
  Kuartil3 = c(250000, 450000, 200000, 300000, 500000, 250000,
               350000, 550000, 650000, 400000, 600000, 500000,
               500000, 650000, 600000, 550000, 700000, 600000,
               600000, 750000, 700000, 650000, 800000, 700000,
               700000, 850000, 800000, 750000, 900000, 850000,
               800000, 950000, 900000),
  Kuartil4 = c(300000, 500000, 250000, 350000, 550000, 300000,
               400000, 600000, 700000, 450000, 650000, 550000,
               550000, 700000, 650000, 600000, 750000, 650000,
               650000, 800000, 750000, 700000, 850000, 750000,
               750000, 900000, 850000, 800000, 950000, 900000,
               900000, 1000000, 950000)
)

# Mengubah data ke format long untuk ggplot
data_flagship_long <- melt(data_flagship, id = c("Tahun", "Negara", "Seri"))
data_midrange_long <- melt(data_midrange, id = c("Tahun", "Negara", "Seri"))

# UI bagian dashboard dengan menu dan tabel
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/2/24/Samsung_Logo.svg", height = "40px"),
      "Dashboard Penjualan Samsung",
      style = "display: inline; color: #FFFFFF; font-size: 20px; font-weight: bold; padding-left: 10px;"
    ),
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Sales Dept",
                   message = "Sales are steady this month."
                 ),
                 messageItem(
                   from = "New User",
                   message = "How do I register?",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 messageItem(
                   from = "Support",
                   message = "The new server is ready.",
                   icon = icon("life-ring"),
                   time = "2014-12-01"
                 )
    ),
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "5 new users today",
                   icon = icon("users")
                 ),
                 notificationItem(
                   text = "12 items delivered",
                   icon = icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
  ),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Detail Grafik", tabName = "detail", icon = icon("chart-line")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Rangkuman", tabName = "Rangkuman", icon = icon("dashboard")),
      menuItem("Peta Penjualan", tabName = "sales_map", icon = icon("map")),
      
      
      # Tambahkan menu Seri dengan 2 kategori sub-menu
      menuItem("Seri", tabName = "seri", icon = icon("phone"), startExpanded = TRUE,
               menuSubItem("Flagship", tabName = "flagship"),
               menuSubItem("Midrange", tabName = "midrange")),
      menuItem("Tentang", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
    .skin-blue .main-header .logo {
      background-color: #000000;
    }
    .skin-blue .main-header .navbar {
      background-color: #000000;
    }
    .skin-blue .main-sidebar {
      background-color: #000000;
    }
    .skin-blue .content-wrapper, .right-side {
      background-color: #000000;
    }
    .box {
      background-color: #333333;
      color: #FFFFFF;
    }
    .dataTables_wrapper .dataTables_filter input,
    .dataTables_wrapper .dataTables_length select,
    .dataTables_wrapper .dataTables_info {
      color: #FFFFFF; /* Warna font putih untuk input, select, dan info tabel */
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: #FFFFFF; /* Warna font putih untuk tombol navigasi */
    }
    .value-box, .info-box {
      color: #FFFFFF; /* Warna font putih untuk value dan info box */
    }
    #data_table table {
      background-color: #1E90FF !important;
      color: #FFFFFF;
    }
    #data_table .dataTables_wrapper .dataTables_info, 
    #data_table .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: #FFFFFF !important;
    }
    #data_table .dataTables_filter input,
    #data_table .dataTables_length select {
      color: #000000;
      background-color: #FFFFFF;
    }
  ")),
    tabItems(
      # Tab Rangkuman
      tabItem(tabName = "Rangkuman",
              fluidRow(
                valueBoxOutput("total_penjualan"),
                valueBoxOutput("pertumbuhan"),
                valueBoxOutput("produk_terlaris")
              ),
              fluidRow(
                box(title = "Tren Penjualan Tahunan", status = "primary", solidHeader = TRUE,
                    plotlyOutput("trend_plot"), width = 12)
              )
      ),
      
      
      # Tab Detail Grafik
      tabItem(tabName = "detail",
              fluidRow(
                box(
                  title = "Pengaturan Grafik", status = "warning", solidHeader = TRUE,
                  selectInput("kategori", "Pilih Kategori Produk:", 
                              choices = c("Flagship", "Midrange"), 
                              selected = "Flagship"),
                  selectInput("plot_type", "Pilih Jenis Plot:", 
                              choices = c("Line Plot", "Scatter Plot", "Pie Chart", "Bar Chart"), 
                              selected = "Line Plot"),
                  conditionalPanel(
                    condition = "input.plot_type == 'Pie Chart'",
                    sliderInput("tahun", "Pilih Tahun (untuk Pie Chart):", 
                                min = 2014, max = 2024, value = 2014, step = 1)
                  ),
                  width = 3
                ),
                box(
                  title = "Grafik Detail", status = "primary", solidHeader = TRUE,
                  plotlyOutput("detail_plot"), width = 9
                )
              )
      ),
      
      # Tab Data
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Data Penjualan", status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("data_table"), width = 12)
              )
      ),
      
      # Tab Tentang
      tabItem(tabName = "about",
              fluidRow(
                box(title = "Tentang Aplikasi", status = "info", solidHeader = TRUE, width = 12,
                    p("Dashboard ini menampilkan data penjualan Samsung dari 2014 hingga 2024, dibagi menjadi kuartil."),
                    p("Fitur-fitur:"),
                    tags$ul(
                      tags$li("Rangkuman: Menampilkan ringkasan penjualan dan tren tahunan."),
                      tags$li("Detail Grafik: Memungkinkan eksplorasi data dengan berbagai jenis grafik."),
                      tags$li("Data: Menampilkan data penjualan dalam bentuk tabel yang dapat dicari dan diurutkan.")
                    ),
                    p("Aplikasi ini dibuat menggunakan Shiny, shinydashboard, dan berbagai paket R lainnya.")
                )
              )
      ),
      
      # Tab untuk Flagship
      tabItem(tabName = "flagship",
              fluidRow(
                box(
                  title = "Samsung Galaxy Z Flip3 5G", width = 12, height = 600, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://cdnpro.eraspace.com/media/wysiwyg/artikel/Tahun_2022/Oktober/hpflagshipsamsung-_1.jpg", height = "540px", width = "1062px")
                )
              ),
              fluidRow(
                box(
                  title = "Samsung Galaxy Z Flip3 5G", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://cdnpro.eraspace.com/media/wysiwyg/artikel/Tahun_2022/Oktober/hpflagshipsamsung-_1.jpg", height = "340px", width = "505px")
                ),
                box(
                  title = "Samsung Galaxy S22+ 5G", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://cdnpro.eraspace.com/media/wysiwyg/artikel/Tahun_2022/Oktober/hpflagshipsamsung-_2.jpg", height = "340px", width = "505px")
                )
              ),
              fluidRow(
                box(
                  title = "Samsung Galaxy S22 Ultra", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://cdnpro.eraspace.com/media/wysiwyg/artikel/Tahun_2022/Oktober/hpflagshipsamsung-_3.jpg", height = "340px", width = "505px")
                ),
                box(
                  title = "Samsung Galaxy Z Fold 4", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://cdnpro.eraspace.com/media/wysiwyg/artikel/Tahun_2022/Agustus/zfold4-1.jpg", height = "340px", width = "505px")
                )
              )
      ),
      
      # Tab untuk Midrange
      tabItem(tabName = "midrange",
              fluidRow(
                box(
                  title = "Samsung Galaxy A24 8/128GB", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://asset-2.tstatic.net/manado/foto/bank/images/Samsung-Galaxy-A54-5G-d156z.jpg", height = "340px", width = "505px")
                ),
                box(
                  title = "Samsung Galaxy A14 5G", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://asset-2.tstatic.net/manado/foto/bank/images/Samsung-Galaxy-A14-5G-dirilis-di-Indonesia.jpg", height = "340px", width = "505px")
                )
              ),
              fluidRow(
                box(
                  title = "Samsung Galaxy S21 FE", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://jateng.disway.id/upload/ed1930139becf1970e7d4b7ee1329cb3.jpg", height = "340px", width = "505px")
                ),
                box(
                  title = "Samsung Galaxy M55s", width = 6, height = 400, solidHeader = TRUE, status = "primary",
                  tags$img(src = "https://media.suara.com/pictures/653x366/2024/03/29/35828-spesifikasi-samsung-galaxy-m55.webp", height = "340px", width = "505px")
                )
              )
      )
    )
  )
)

# Server bagian dashboard
server <- function(input, output) {
  
  # Fungsi untuk menghitung total penjualan
  calculate_total_sales <- reactive({
    if (input$kategori == "Flagship") {
      sum(data_flagship[, -c(1,2,3)])
    } else {
      sum(data_midrange[, -c(1,2,3)])
    }
  })
  
  # Fungsi untuk menghitung pertumbuhan
  calculate_growth <- reactive({
    if (input$kategori == "Flagship") {
      last_year <- sum(data_flagship[data_flagship$Tahun == max(data_flagship$Tahun), -c(1,2,3)])
      prev_year <- sum(data_flagship[data_flagship$Tahun == max(data_flagship$Tahun) - 1, -c(1,2,3)])
    } else {
      last_year <- sum(data_midrange[data_midrange$Tahun == max(data_midrange$Tahun), -c(1,2,3)])
      prev_year <- sum(data_midrange[data_midrange$Tahun == max(data_midrange$Tahun) - 1, -c(1,2,3)])
    }
    growth <- (last_year - prev_year) / prev_year * 100
    return(growth)
  })
  
  # Fungsi untuk menentukan produk terlaris
  find_best_selling <- reactive({
    if (input$kategori == "Flagship") {
      total_sales <- colSums(data_flagship[, -c(1,2,3)])
      best_product <- names(total_sales)[which.max(total_sales)]
      return(best_product)
    } else {
      total_sales <- colSums(data_midrange[, -c(1,2,3)])
      best_product <- names(total_sales)[which.max(total_sales)]
      return(best_product)
    } })
  
  # Output untuk value boxes
  output$total_penjualan <- renderValueBox({
    valueBox(
      paste0("$", round(calculate_total_sales(), 2), "B"),
      "Total Penjualan",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$pertumbuhan <- renderValueBox({
    valueBox(
      paste0(round(calculate_growth(), 2), "%"),
      "Pertumbuhan YoY",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$produk_terlaris <- renderValueBox({
    valueBox(
      find_best_selling(),
      "Produk Terlaris",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  # Output untuk trend plot
  output$trend_plot <- renderPlotly({
    if (input$kategori == "Flagship") {
      yearly_data <- aggregate(. ~ Tahun, data = data_flagship[, -c(2,3)], FUN = sum)
    } else {
      yearly_data <- aggregate(. ~ Tahun, data = data_midrange[, -c(2,3)], FUN = sum)
    }
    plot_ly(yearly_data, x = ~Tahun) %>%
      add_trace(y = ~Kuartil1, name = "Kuartil 1", type = "scatter", mode = "lines+markers") %>%
      add_trace(y = ~Kuartil2, name = "Kuartil 2", type = "scatter", mode = "lines+markers") %>%
      add_trace(y = ~Kuartil3, name = "Kuartil 3", type = "scatter", mode = "lines+markers") %>%
      add_trace(y = ~Kuartil4, name = "Kuartil 4", type = "scatter", mode = "lines+markers") %>%
      layout(title = "Tren Penjualan Tahunan", xaxis = list(title = "Tahun"), yaxis = list(title = "Penjualan (Miliar USD)"))
  })
  
  # Output untuk detail plot
  output$detail_plot <- renderPlotly({
    if (input$kategori == "Flagship") {
      selected_data <- data_flagship_long
    } else {
      selected_data <- data_midrange_long
    }
    
    if (input$plot_type == "Line Plot") {
      p <- ggplot(selected_data, aes(x = interaction(Tahun, Negara), y = value, group = 1)) +
        geom_line() +
        geom_point() +
        labs(title = paste("Line Plot untuk", input$kategori),
             x = "Tahun-Negara", y = "Penjualan (Miliar USD)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
      
    } else if (input$plot_type == "Scatter Plot") {
      p <- ggplot(selected_data, aes(x = interaction(Tahun, Negara), y = value)) +
        geom_point(size = 4, color = "red") +
        labs(title = paste("Scatter Plot untuk", input$kategori),
             x = "Tahun-Negara", y = "Penjualan (Miliar USD)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
      
    } else if (input$plot_type == "Pie Chart") {
      # Create pie data based on selected year
      pie_data <- data.frame(Kuartil = c("Kuartil1", "Kuartil2", "Kuartil3", "Kuartil4"),
                             Values = numeric(4))
      
      if (input$kategori == "Flagship") {
        for (i in 1:4) {
          pie_data$Values[i] <- data_flagship[data_flagship$Tahun == input$tahun, paste0("Kuartil", i)]
        }
      } else {
        for (i in 1:4) {
          pie_data$Values[i] <- data_midrange[data_midrange$Tahun == input$tahun, paste0("Kuartil", i)]
        }
      }
      
      # Remove rows with zero values to avoid empty slices
      pie_data <- pie_data[pie_data$Values > 0, ]
      
      plot_ly(pie_data, labels = ~Kuartil, values = ~Values, type = 'pie') %>%
        layout(title = paste("Pie Chart untuk Tahun", input$tahun))
      
    } else if (input$plot_type == "Bar Chart") {
      p <- ggplot(selected_data, aes(x = interaction(Tahun, Negara), y = value)) +
        geom_bar(stat = "identity", fill = "green") +
        labs(title = paste("Bar Chart untuk", input$kategori),
             x = "Tahun-Negara", y = "Penjualan (Miliar USD)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    }
  })
  
  # Output untuk data table
  output$data_table <- DT::renderDataTable({
    if (input$kategori == "Flagship") {
      DT::datatable(data_flagship, options = list(pageLength = 15, autoWidth = TRUE))
    } else {
      DT::datatable(data_midrange, options = list(pageLength = 15, autoWidth = TRUE))
    }
  })
}

# Jalankan aplikasi Shiny
shinyApp(ui = ui, server = server)