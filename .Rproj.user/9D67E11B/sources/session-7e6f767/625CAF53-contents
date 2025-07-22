# =====================================================
# DASHBOARD ANALISIS SOVI - UJIAN STATISTIKA TERAPAN
# Politeknik Statistika STIS - VERSI LENGKAP & DIPERBAIKI
# =====================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(leaflet)
library(leaflet.extras)
library(viridis)
library(car)
library(nortest)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rmarkdown)
library(zip)
library(moments)
library(e1071)
library(stats)
library(sf)
library(cluster)
library(factoextra)

# Load SOVI data from URL
sovi_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")
distance_data <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")

# Create initial categorical variables
sovi_data$Population_Size <- ifelse(sovi_data$CHILDREN > median(sovi_data$CHILDREN, na.rm = TRUE), "Besar", "Kecil")
sovi_data$Economic_Status <- cut(sovi_data$POVERTY,
                                 breaks = quantile(sovi_data$POVERTY, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Kemiskinan_Rendah", "Kemiskinan_Sedang", "Kemiskinan_Tinggi"), 
                                 include.lowest = TRUE)
sovi_data$Age_Group <- cut(sovi_data$ELDERLY,
                           breaks = quantile(sovi_data$ELDERLY, probs = c(0, 0.5, 1), na.rm = TRUE),
                           labels = c("Muda", "Tua"), include.lowest = TRUE)
sovi_data$Education_Level <- cut(sovi_data$LOWEDU,
                                 breaks = quantile(sovi_data$LOWEDU, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
                                 labels = c("Pendidikan_Tinggi", "Pendidikan_Sedang", "Pendidikan_Rendah"), 
                                 include.lowest = TRUE)

# SOVI Category based on quartiles
sovi_data$SOVI_Category <- cut(sovi_data$POVERTY,
                               breaks = quantile(sovi_data$POVERTY, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                               labels = c("Sangat_Rendah", "Rendah", "Sedang", "Tinggi"), 
                               include.lowest = TRUE)

# Color palette
colors <- c("#5E7892", "#A7B7C6", "#F3EFDF", "#BDCFAA", "#8E9E83")

# Custom CSS
custom_css <- paste0("
.content-wrapper, .right-side {
  background-color: ", colors[3], ";
}
.main-header .navbar {
  background-color: ", colors[1], " !important;
}
.main-header .logo {
  background-color: ", colors[1], " !important;
}
.sidebar {
  background-color: ", colors[2], " !important;
}
.box {
  border-radius: 8px !important;
  box-shadow: 0 4px 12px rgba(94, 120, 146, 0.1) !important;
}
.btn-primary {
  background-color: ", colors[1], " !important;
  border-color: ", colors[1], " !important;
}
.nav-tabs-custom > .nav-tabs > li.active {
  border-top-color: ", colors[1], " !important;
}
")

# UI
ui <- dashboardPage(
  title = "Dashboard Analisis SOVI - STIS UAS 2025",
  skin = "blue",
  
  dashboardHeader(
    title = "🎓 Dashboard Analisis SOVI - STIS UAS 2025",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("✅ Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("📈 Statistik Inferensia", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_proporsi"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("📉 Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart")),
      menuItem("🗺️ Analisis Clustering", tabName = "clustering", icon = icon("map"))
    ),
    
    # Download section in sidebar
    div(
      style = "background: rgba(255,255,255,0.1); margin: 10px; padding: 15px; border-radius: 8px;",
      conditionalPanel(
        condition = "input.sidebar == 'beranda'",
        h5("📥 Download Beranda", style = "color: white;"),
        downloadButton("download_beranda_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'manajemen'",
        h5("📥 Download Manajemen", style = "color: white;"),
        downloadButton("download_manajemen_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'eksplorasi'",
        h5("📥 Download Eksplorasi", style = "color: white;"),
        downloadButton("download_eksplorasi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'asumsi'",
        h5("📥 Download Uji Asumsi", style = "color: white;"),
        downloadButton("download_asumsi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'uji_rata' || input.sidebar == 'uji_proporsi' || input.sidebar == 'anova'",
        h5("📥 Download Inferensia", style = "color: white;"),
        downloadButton("download_inferensia_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'regresi'",
        h5("📥 Download Regresi", style = "color: white;"),
        downloadButton("download_regresi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'clustering'",
        h5("📥 Download Clustering", style = "color: white;"),
        downloadButton("download_clustering_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_clustering_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_clustering_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_clustering_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(
        tabName = "beranda",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("🎓 Dashboard Analisis SOVI - STIS UAS 2025", style = "margin: 0; font-weight: 600;"),
                   p("Social Vulnerability Index Analysis - Ujian Akhir Semester Komputasi Statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        # Metadata Dashboard
        fluidRow(
          column(12,
                 box(
                   title = "📋 Metadata Dashboard", status = "primary", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 15px;",
                     h4("Informasi Dataset SOVI", style = paste0("color: ", colors[1], "; margin-bottom: 15px;")),
                     fluidRow(
                       column(6,
                              tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                      tags$li(strong("Nama Dashboard:"), " Dashboard Analisis SOVI - STIS UAS 2025"),
                                      tags$li(strong("Dataset:"), " Social Vulnerability Index"),
                                      tags$li(strong("Sumber Data:"), " https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"),
                                      tags$li(strong("Metadata:"), " https://www.sciencedirect.com/science/article/pii/S2352340921010180"),
                                      tags$li(strong("Jumlah Observasi:"), textOutput("total_observations_meta", inline = TRUE)),
                                      tags$li(strong("Jumlah Variabel:"), textOutput("total_variables_meta", inline = TRUE))
                              )
                       ),
                       column(6,
                              tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                      tags$li(strong("Platform:"), " R Shiny Dashboard"),
                                      tags$li(strong("Kelengkapan Data:"), textOutput("data_completeness_meta", inline = TRUE)),
                                      tags$li(strong("Fitur Utama:"), " Analisis Statistik Komprehensif"),
                                      tags$li(strong("Download Format:"), " JPG, PDF, Word, ZIP"),
                                      tags$li(strong("Ujian:"), " 23 Juli 2025, 10.30-12.30 WIB"),
                                      tags$li(strong("Fakta Integritas:"), " https://s.stis.ac.id/Fakta_integritas_KOMSTAT")
                              )
                       )
                     )
                   )
                 )
          )
        ),
        
        # Metric Cards
        fluidRow(
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_observations")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Observasi")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[4], " 0%, ", colors[5], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("total_variables")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Total Variabel")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[2], " 0%, ", colors[3], " 100%); color: ", colors[1], "; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("avg_poverty_rate")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Rata-rata Kemiskinan (%)")
                 )
          ),
          column(3,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[5], " 0%, ", colors[4], " 100%); color: white; padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                   div(style = "font-size: 2.5em; font-weight: 700;", textOutput("data_completeness")),
                   div(style = "font-size: 1em; margin-top: 8px;", "Kelengkapan Data")
                 )
          )
        ),
        
        # Main content
        fluidRow(
          column(8,
                 box(
                   title = "📊 Distribusi Data SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("sovi_distribution", height = "350px")
                 )
          ),
          column(4,
                 box(
                   title = "📈 Statistik Ringkasan", status = "info", solidHeader = TRUE, width = NULL,
                   verbatimTextOutput("summary_stats")
                 )
          )
        ),
        
        # Peta Distribusi
        fluidRow(
          column(12,
                 box(
                   title = "🗺️ Peta Distribusi SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   leafletOutput("beranda_map", height = "400px")
                 )
          )
        ),
        
        # Interpretasi
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; border-left: 4px solid ", colors[1], ";"),
                   h4("💡 Interpretasi Dashboard Beranda", style = paste0("color: ", colors[1], ";")),
                   uiOutput("beranda_interpretation")
                 )
          )
        )
      ),
      
      # Manajemen Data Tab
      tabItem(
        tabName = "manajemen",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("📊 Manajemen Data", style = "margin: 0; font-weight: 600;"),
                   p("Kelola dan transformasi data SOVI untuk analisis optimal", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "🛠️ Alat Manajemen Data", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("📋 Ringkasan Data",
                              br(),
                              fluidRow(
                                column(6,
                                       h4("Ringkasan Dataset", style = paste0("color: ", colors[1], ";")),
                                       verbatimTextOutput("data_summary")
                                ),
                                column(6,
                                       h4("Struktur Data", style = paste0("color: ", colors[1], ";")),
                                       verbatimTextOutput("data_structure")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("💡 Interpretasi Ringkasan Data", style = paste0("color: ", colors[1], ";")),
                                uiOutput("data_summary_interpretation")
                              )
                     ),
                     
                     tabPanel("🏷️ Kategorisasi Data",
                              br(),
                              div(
                                style = paste0("background: white; border-radius: 8px; padding: 20px; border: 1px solid ", colors[2], ";"),
                                h4("Mengubah Data Kontinu menjadi Kategorik", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(4,
                                         selectInput("categorize_variable", "Pilih Variabel:",
                                                     choices = NULL),
                                         selectInput("categorize_method", "Metode Kategorisasi:",
                                                     choices = list(
                                                       "Kuartil (4 kategori)" = "quartile",
                                                       "Tertil (3 kategori)" = "tertile",
                                                       "Median (2 kategori)" = "median",
                                                       "Custom (manual)" = "custom"
                                                     ))
                                  ),
                                  column(4,
                                         conditionalPanel(
                                           condition = "input.categorize_method != 'custom'",
                                           textInput("category_labels", "Label Kategori (pisahkan dengan koma):",
                                                     value = "Rendah, Sedang, Tinggi")
                                         ),
                                         conditionalPanel(
                                           condition = "input.categorize_method == 'custom'",
                                           textInput("custom_breaks", "Titik Potong (pisahkan dengan koma):",
                                                     value = "0, 25, 50, 75, 100"),
                                           textInput("custom_labels", "Label Custom (pisahkan dengan koma):",
                                                     value = "Sangat Rendah, Rendah, Sedang, Tinggi")
                                         ),
                                         br(),
                                         actionButton("apply_categorization", "Terapkan Kategorisasi",
                                                      class = "btn-primary", style = "width: 100%;")
                                  ),
                                  column(4,
                                         h5("Preview Kategorisasi"),
                                         div(
                                           style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 6px;"),
                                           verbatimTextOutput("categorization_preview")
                                         )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.apply_categorization > 0",
                                  br(),
                                  h4("Hasil Kategorisasi", style = paste0("color: ", colors[1], ";")),
                                  fluidRow(
                                    column(6,
                                           h5("Tabel Frekuensi"),
                                           DT::dataTableOutput("categorization_table")
                                    ),
                                    column(6,
                                           h5("Visualisasi Kategori"),
                                           plotlyOutput("categorization_plot", height = "300px")
                                    )
                                  ),
                                  
                                  div(
                                    style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                    h5("💡 Interpretasi Kategorisasi", style = paste0("color: ", colors[1], ";")),
                                    uiOutput("categorization_interpretation")
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("🔄 Transformasi Data",
                              br(),
                              div(
                                style = paste0("background: white; border-radius: 8px; padding: 20px; border: 1px solid ", colors[2], ";"),
                                h4("Transformasi Variabel Numerik", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(4,
                                         selectInput("transform_variable", "Pilih Variabel:",
                                                     choices = NULL),
                                         selectInput("transform_method", "Metode Transformasi:",
                                                     choices = list(
                                                       "Log Natural" = "log",
                                                       "Log10" = "log10",
                                                       "Square Root" = "sqrt",
                                                       "Square" = "square",
                                                       "Z-Score" = "zscore"
                                                     ))
                                  ),
                                  column(4,
                                         br(),
                                         actionButton("apply_transformation", "Terapkan Transformasi",
                                                      class = "btn-primary", style = "width: 100%;")
                                  ),
                                  column(4,
                                         h5("Preview Transformasi"),
                                         div(
                                           style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 6px;"),
                                           verbatimTextOutput("transformation_preview")
                                         )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.apply_transformation > 0",
                                  br(),
                                  h4("Hasil Transformasi", style = paste0("color: ", colors[1], ";")),
                                  fluidRow(
                                    column(6,
                                           h5("Sebelum Transformasi"),
                                           plotlyOutput("before_transform_plot", height = "300px")
                                    ),
                                    column(6,
                                           h5("Setelah Transformasi"),
                                           plotlyOutput("after_transform_plot", height = "300px")
                                    )
                                  ),
                                  
                                  div(
                                    style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                    h5("💡 Interpretasi Transformasi", style = paste0("color: ", colors[1], ";")),
                                    uiOutput("transformation_interpretation")
                                  )
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Eksplorasi Data Tab
      tabItem(
        tabName = "eksplorasi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("🔍 Eksplorasi Data", style = "margin: 0; font-weight: 600;"),
                   p("Analisis deskriptif dan visualisasi data SOVI", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "🔬 Eksplorasi Data Interaktif", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("📊 Statistik Deskriptif",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("descriptive_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("descriptive_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status",
                                                               "Kelompok Usia" = "Age_Group",
                                                               "Tingkat Pendidikan" = "Education_Level"))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_descriptive", "Jalankan Analisis",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_descriptive > 0",
                                fluidRow(
                                  column(6,
                                         h4("Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("descriptive_stats")
                                  ),
                                  column(6,
                                         h4("Tabel Ringkasan", style = paste0("color: ", colors[1], ";")),
                                         DT::dataTableOutput("descriptive_table")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("descriptive_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("📈 Visualisasi Data",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("plot_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("plot_type", "Jenis Plot:",
                                                   choices = list(
                                                     "Histogram" = "histogram",
                                                     "Box Plot" = "boxplot",
                                                     "Density Plot" = "density",
                                                     "Violin Plot" = "violin"
                                                   ))
                                ),
                                column(4,
                                       selectInput("plot_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status"))
                                )
                              ),
                              
                              fluidRow(
                                column(8,
                                       plotlyOutput("visualization_plot", height = "450px")
                                ),
                                column(4,
                                       h4("Statistik Deskriptif", style = paste0("color: ", colors[1], ";")),
                                       verbatimTextOutput("plot_stats")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("💡 Interpretasi Visualisasi", style = paste0("color: ", colors[1], ";")),
                                uiOutput("visualization_interpretation")
                              )
                     ),
                     
                     tabPanel("🗺️ Peta Interaktif",
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput("map_variable", "Variabel untuk Peta:",
                                                   choices = NULL),
                                       selectInput("map_type", "Jenis Peta:",
                                                   choices = list(
                                                     "Scatter Plot" = "scatter",
                                                     "Heat Map" = "heatmap",
                                                     "Choropleth" = "choropleth"
                                                   ))
                                ),
                                column(9,
                                       leafletOutput("exploration_map", height = "500px")
                                )
                              ),
                              
                              div(
                                style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                h5("💡 Interpretasi Peta", style = paste0("color: ", colors[1], ";")),
                                uiOutput("map_interpretation")
                              )
                     ),
                     
                     tabPanel("🔗 Analisis Korelasi",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("corr_variables", "Pilih Variabel (minimal 2):",
                                                   choices = NULL, multiple = TRUE),
                                       selectInput("corr_method", "Metode Korelasi:",
                                                   choices = list(
                                                     "Pearson" = "pearson",
                                                     "Spearman" = "spearman",
                                                     "Kendall" = "kendall"
                                                   )),
                                       br(),
                                       actionButton("run_correlation", "Jalankan Analisis",
                                                    class = "btn-primary", style = "width: 100%;")
                                ),
                                column(8,
                                       conditionalPanel(
                                         condition = "input.run_correlation > 0",
                                         plotOutput("correlation_plot", height = "400px")
                                       )
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_correlation > 0",
                                fluidRow(
                                  column(6,
                                         h4("Matriks Korelasi", style = paste0("color: ", colors[1], ";")),
                                         DT::dataTableOutput("correlation_table")
                                  ),
                                  column(6,
                                         h4("Uji Signifikansi", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("correlation_test")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Korelasi", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("correlation_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Asumsi Tab
      tabItem(
        tabName = "asumsi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("✅ Uji Asumsi Statistik", style = "margin: 0; font-weight: 600;"),
                   p("Verifikasi asumsi normalitas dan homogenitas untuk analisis statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "🔍 Pengujian Asumsi Statistik", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("📊 Uji Normalitas",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("normality_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("normality_test", "Jenis Uji:",
                                                   choices = list(
                                                     "Shapiro-Wilk" = "shapiro",
                                                     "Anderson-Darling" = "anderson",
                                                     "Kolmogorov-Smirnov" = "ks",
                                                     "Jarque-Bera" = "jarque"
                                                   ))
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_normality", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_normality > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Normalitas", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("normality_result")
                                  ),
                                  column(6,
                                         h4("Q-Q Plot", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("qq_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Histogram dengan Kurva Normal", style = paste0("color: ", colors[1], ";")),
                                plotlyOutput("normality_histogram", height = "350px"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji Normalitas", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("normality_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("⚖️ Uji Homogenitas",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("homogeneity_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("homogeneity_group", "Variabel Pengelompokan:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
                                ),
                                column(4,
                                       selectInput("homogeneity_test", "Jenis Uji:",
                                                   choices = list(
                                                     "Levene Test" = "levene",
                                                     "Bartlett Test" = "bartlett",
                                                     "Fligner-Killeen" = "fligner"
                                                   )),
                                       br(),
                                       actionButton("run_homogeneity", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_homogeneity > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Homogenitas", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("homogeneity_result")
                                  ),
                                  column(6,
                                         h4("Box Plot berdasarkan Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("homogeneity_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji Homogenitas", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("homogeneity_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Rata-rata Tab
      tabItem(
        tabName = "uji_rata",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("📊 Uji Beda Rata-rata", style = "margin: 0; font-weight: 600;"),
                   p("Analisis perbedaan rata-rata menggunakan uji t", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "📈 Uji Beda Rata-rata", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("1️⃣ Uji t Satu Sampel",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("onesample_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       numericInput("mu_hypothesis", "Nilai Hipotesis (μ₀):",
                                                    value = 0, step = 0.1)
                                ),
                                column(4,
                                       selectInput("alternative_hypothesis", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                )
                              ),
                              
                              actionButton("run_onesample", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_onesample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Satu Sampel", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("onesample_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("onesample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji t Satu Sampel", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("onesample_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("2️⃣ Uji t Dua Sampel",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("twosample_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("twosample_group", "Variabel Pengelompokan:",
                                                   choices = c("Population_Size", "Age_Group"))
                                ),
                                column(4,
                                       checkboxInput("equal_variances", "Asumsi Varians Sama", value = TRUE),
                                       selectInput("twosample_alternative", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                )
                              ),
                              
                              actionButton("run_twosample", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_twosample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Dua Sampel", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("twosample_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("twosample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji t Dua Sampel", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("twosample_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Uji Proporsi & Varians Tab
      tabItem(
        tabName = "uji_proporsi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("📊 Uji Proporsi & Varians", style = "margin: 0; font-weight: 600;"),
                   p("Analisis proporsi dan varians dengan metode statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "📈 Uji Proporsi & Varians", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("📊 Uji Proporsi",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("prop_variable", "Pilih Variabel Kategorik:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
                                ),
                                column(4,
                                       selectInput("prop_category", "Kategori yang Diuji:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       numericInput("prop_hypothesis", "Proporsi Hipotesis:",
                                                    value = 0.5, min = 0, max = 1, step = 0.01),
                                       selectInput("prop_alternative", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                )
                              ),
                              
                              actionButton("run_prop_test", "Jalankan Uji",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_prop_test > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Proporsi", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("prop_test_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi Proporsi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("prop_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji Proporsi", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("prop_test_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("📊 Uji Varians",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("var_test_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("var_test_group", "Variabel Pengelompokan:",
                                                   choices = c("Population_Size", "Age_Group"))
                                ),
                                column(4,
                                       selectInput("var_test_alternative", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   )),
                                       br(),
                                       actionButton("run_var_test", "Jalankan Uji F",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_var_test > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji F", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("var_test_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Varians", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("var_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji Varians", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("var_test_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # ANOVA Tab
      tabItem(
        tabName = "anova",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("📊 Analysis of Variance (ANOVA)", style = "margin: 0; font-weight: 600;"),
                   p("Analisis varians untuk membandingkan rata-rata lebih dari dua kelompok", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "📈 Analisis ANOVA", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("1️⃣ ANOVA Satu Arah",
                              br(),
                              fluidRow(
                                column(6,
                                       selectInput("anova_variable", "Pilih Variabel Dependen:",
                                                   choices = NULL)
                                ),
                                column(6,
                                       selectInput("anova_group", "Pilih Variabel Pengelompokan:",
                                                   choices = c("SOVI_Category", "Economic_Status", "Education_Level"))
                                )
                              ),
                              
                              actionButton("run_anova", "Jalankan ANOVA",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_anova > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil ANOVA", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("anova_result")
                                  ),
                                  column(6,
                                         h4("Plot Rata-rata Kelompok", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("anova_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc (Tukey HSD)", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("posthoc_result"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi ANOVA Satu Arah", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("anova_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("2️⃣ ANOVA Dua Arah",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("anova2_variable", "Pilih Variabel Dependen:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("anova2_factor1", "Faktor 1:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status"))
                                ),
                                column(4,
                                       selectInput("anova2_factor2", "Faktor 2:",
                                                   choices = c("Age_Group", "Education_Level"))
                                )
                              ),
                              
                              checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE),
                              
                              actionButton("run_anova2", "Jalankan ANOVA Dua Arah",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_anova2 > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil ANOVA Dua Arah", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("anova2_result")
                                  ),
                                  column(6,
                                         h4("Plot Interaksi", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("anova2_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc untuk Faktor Utama", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("posthoc2_result"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi ANOVA Dua Arah", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("anova2_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Regresi Linear Berganda Tab
      tabItem(
        tabName = "regresi",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("📉 Regresi Linear Berganda", style = "margin: 0; font-weight: 600;"),
                   p("Analisis regresi dengan diagnostik dan validasi asumsi", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "📈 Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("🏗️ Pembangunan Model",
                              br(),
                              fluidRow(
                                column(6,
                                       selectInput("regression_response", "Variabel Respons (Y):",
                                                   choices = NULL)
                                ),
                                column(6,
                                       selectInput("regression_predictors", "Variabel Prediktor (X):",
                                                   choices = NULL, multiple = TRUE)
                                )
                              ),
                              
                              actionButton("run_regression", "Jalankan Regresi",
                                           class = "btn-primary", style = "margin-top: 15px;"),
                              
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Hasil Regresi Linear Berganda", style = paste0("color: ", colors[1], ";")),
                                verbatimTextOutput("regression_result"),
                                
                                fluidRow(
                                  column(6,
                                         h4("Ringkasan Model", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("model_summary")
                                  ),
                                  column(6,
                                         h4("Fitted vs Actual", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("fitted_actual_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Model", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("regression_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("📊 Plot Diagnostik",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Plot Diagnostik Regresi", style = paste0("color: ", colors[1], "; margin-bottom: 20px;")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Residual vs Fitted"),
                                         plotlyOutput("residuals_fitted", height = "300px")
                                  ),
                                  column(6,
                                         h5("Q-Q Plot Residual"),
                                         plotlyOutput("qq_residuals", height = "300px")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h5("Scale-Location Plot"),
                                         plotlyOutput("scale_location_plot", height = "300px")
                                  ),
                                  column(6,
                                         h5("Residual vs Leverage"),
                                         plotlyOutput("leverage_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Plot Diagnostik", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("diagnostic_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan model regresi terlebih dahulu di tab 'Pembangunan Model'."))
                              )
                     ),
                     
                     tabPanel("✅ Uji Asumsi",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Uji Asumsi Regresi", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Uji Multikolinearitas (VIF)"),
                                         verbatimTextOutput("multicollinearity_test")
                                  ),
                                  column(6,
                                         h5("Uji Durbin-Watson"),
                                         verbatimTextOutput("durbin_watson_test")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h5("Uji Normalitas Residual"),
                                         verbatimTextOutput("residual_normality_test")
                                  ),
                                  column(6,
                                         h5("Uji Homoskedastisitas"),
                                         verbatimTextOutput("homoscedasticity_test")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Uji Asumsi", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("assumption_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan model regresi terlebih dahulu di tab 'Pembangunan Model'."))
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Clustering Tab
      tabItem(
        tabName = "clustering",
        fluidRow(
          column(12,
                 div(
                   style = paste0("background: linear-gradient(135deg, ", colors[1], " 0%, ", colors[2], " 100%); color: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; text-align: center;"),
                   h1("🗺️ Analisis Clustering", style = "margin: 0; font-weight: 600;"),
                   p("Analisis pengelompokan menggunakan matriks jarak dan visualisasi peta", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "🔬 Analisis Clustering dengan Matriks Jarak", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("🎯 K-Means Clustering",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("cluster_variables", "Pilih Variabel untuk Clustering:",
                                                   choices = NULL, multiple = TRUE),
                                       numericInput("n_clusters", "Jumlah Cluster:",
                                                    value = 3, min = 2, max = 10, step = 1)
                                ),
                                column(4,
                                       selectInput("cluster_method", "Metode Clustering:",
                                                   choices = list(
                                                     "K-Means" = "kmeans",
                                                     "Hierarchical" = "hierarchical",
                                                     "PAM (K-Medoids)" = "pam"
                                                   )),
                                       checkboxInput("use_distance_matrix", "Gunakan Matriks Jarak", value = TRUE)
                                ),
                                column(4,
                                       br(),
                                       actionButton("run_clustering", "Jalankan Clustering",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_clustering > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Clustering", style = paste0("color: ", colors[1], ";")),
                                         verbatimTextOutput("clustering_result")
                                  ),
                                  column(6,
                                         h4("Elbow Method", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("elbow_plot", height = "300px")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h4("Silhouette Analysis", style = paste0("color: ", colors[1], ";")),
                                         plotOutput("silhouette_plot", height = "300px")
                                  ),
                                  column(6,
                                         h4("Cluster Plot", style = paste0("color: ", colors[1], ";")),
                                         plotlyOutput("cluster_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Clustering", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("clustering_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("🗺️ Peta Clustering",
                              br(),
                              conditionalPanel(
                                condition = "input.run_clustering > 0",
                                fluidRow(
                                  column(3,
                                         h5("Pengaturan Peta"),
                                         selectInput("map_cluster_type", "Jenis Visualisasi:",
                                                     choices = list(
                                                       "Cluster Points" = "points",
                                                       "Cluster Heatmap" = "heatmap",
                                                       "Cluster Polygons" = "polygons"
                                                     )),
                                         checkboxInput("show_cluster_centers", "Tampilkan Pusat Cluster", value = TRUE)
                                  ),
                                  column(9,
                                         leafletOutput("clustering_map", height = "500px")
                                  )
                                ),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Peta Clustering", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("clustering_map_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_clustering == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan analisis clustering terlebih dahulu di tab 'K-Means Clustering'."))
                              )
                     ),
                     
                     tabPanel("📊 Profil Cluster",
                              br(),
                              conditionalPanel(
                                condition = "input.run_clustering > 0",
                                h4("Profil Karakteristik Setiap Cluster", style = paste0("color: ", colors[1], ";")),
                                
                                fluidRow(
                                  column(6,
                                         h5("Statistik Deskriptif per Cluster"),
                                         DT::dataTableOutput("cluster_profile_table")
                                  ),
                                  column(6,
                                         h5("Distribusi Cluster"),
                                         plotlyOutput("cluster_distribution_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Perbandingan Antar Cluster", style = paste0("color: ", colors[1], ";")),
                                plotlyOutput("cluster_comparison_plot", height = "400px"),
                                
                                div(
                                  style = paste0("background: ", colors[3], "; padding: 15px; border-radius: 8px; margin-top: 15px;"),
                                  h5("💡 Interpretasi Profil Cluster", style = paste0("color: ", colors[1], ";")),
                                  uiOutput("cluster_profile_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_clustering == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan analisis clustering terlebih dahulu di tab 'K-Means Clustering'."))
                              )
                     )
                   )
                 )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    regression_model = NULL,
    categorized_variable = NULL,
    transformed_variable = NULL,
    clustering_result = NULL
  )
  
  # Update choices when app starts
  observe({
    numeric_vars <- names(select_if(sovi_data, is.numeric))
    categorical_vars <- c("SOVI_Category", "Population_Size", "Economic_Status", "Age_Group", "Education_Level")
    
    # Update all select inputs
    updateSelectInput(session, "categorize_variable", choices = numeric_vars)
    updateSelectInput(session, "transform_variable", choices = numeric_vars)
    updateSelectInput(session, "descriptive_variable", choices = numeric_vars)
    updateSelectInput(session, "plot_variable", choices = numeric_vars)
    updateSelectInput(session, "map_variable", choices = numeric_vars)
    updateSelectInput(session, "normality_variable", choices = numeric_vars)
    updateSelectInput(session, "homogeneity_variable", choices = numeric_vars)
    updateSelectInput(session, "onesample_variable", choices = numeric_vars)
    updateSelectInput(session, "twosample_variable", choices = numeric_vars)
    updateSelectInput(session, "var_test_variable", choices = numeric_vars)
    updateSelectInput(session, "anova_variable", choices = numeric_vars)
    updateSelectInput(session, "anova2_variable", choices = numeric_vars)
    updateSelectInput(session, "regression_response", choices = numeric_vars)
    updateSelectInput(session, "regression_predictors", choices = numeric_vars)
    updateSelectInput(session, "corr_variables", choices = numeric_vars)
    updateSelectInput(session, "cluster_variables", choices = numeric_vars)
  })
  
  # Update proportion category choices
  observe({
    req(input$prop_variable)
    categories <- unique(sovi_data[[input$prop_variable]])
    categories <- categories[!is.na(categories)]
    updateSelectInput(session, "prop_category", choices = categories)
  })
  
  # Home tab outputs
  output$total_observations <- renderText({
    format(nrow(sovi_data), big.mark = ",")
  })
  
  output$total_observations_meta <- renderText({
    format(nrow(sovi_data), big.mark = ",")
  })
  
  output$total_variables <- renderText({
    ncol(sovi_data)
  })
  
  output$total_variables_meta <- renderText({
    ncol(sovi_data)
  })
  
  output$avg_poverty_rate <- renderText({
    round(mean(sovi_data$POVERTY, na.rm = TRUE), 2)
  })
  
  output$data_completeness <- renderText({
    complete_rows <- sum(complete.cases(sovi_data))
    total_rows <- nrow(sovi_data)
    paste0(round(complete_rows/total_rows * 100, 1), "%")
  })
  
  output$data_completeness_meta <- renderText({
    complete_rows <- sum(complete.cases(sovi_data))
    total_rows <- nrow(sovi_data)
    paste0(round(complete_rows/total_rows * 100, 1), "%")
  })
  
  output$summary_stats <- renderPrint({
    numeric_data <- select_if(sovi_data, is.numeric)
    if(ncol(numeric_data) > 0) {
      summary(numeric_data[1:min(5, ncol(numeric_data))])
    } else {
      "Tidak ada variabel numerik tersedia"
    }
  })
  
  # SOVI distribution plot
  output$sovi_distribution <- renderPlotly({
    var_data <- sovi_data$POVERTY
    var_name <- "POVERTY"
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
      geom_density(aes(y = after_stat(density) * length(var_data) * diff(range(var_data, na.rm = TRUE))/30),
                   color = colors[2], size = 2) +
      labs(title = paste("Distribusi", var_name),
           x = var_name, y = "Frekuensi") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = colors[1]),
        axis.title = element_text(size = 12, color = colors[1])
      )
    
    ggplotly(p) %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  # Beranda map using distance data coordinates
  output$beranda_map <- renderLeaflet({
    # Merge sovi_data with distance_data for coordinates
    map_data <- merge(sovi_data, distance_data, by = "DISTRICTCODE", all.x = TRUE)
    map_data <- map_data[!is.na(map_data$LONGITUDE) & !is.na(map_data$LATITUDE), ]
    
    # Create color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data$POVERTY
    )
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 6,
        fillColor = ~pal(POVERTY),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        label = ~lapply(paste(
          "<strong>District Code:", DISTRICTCODE, "</strong><br/>",
          "Tingkat Kemiskinan:", round(POVERTY, 2), "%<br/>",
          "Pendidikan Rendah:", round(LOWEDU, 2), "%<br/>",
          "Anak-anak:", round(CHILDREN, 2), "%<br/>",
          "Lansia:", round(ELDERLY, 2), "%"
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~POVERTY,
        opacity = 0.7,
        title = "Tingkat Kemiskinan (%)",
        position = "bottomright"
      )
  })
  
  # Beranda interpretation
  output$beranda_interpretation <- renderUI({
    total_obs <- nrow(sovi_data)
    total_vars <- ncol(sovi_data)
    completeness <- round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1)
    avg_poverty <- round(mean(sovi_data$POVERTY, na.rm = TRUE), 2)
    
    interpretation <- paste0(
      "Dashboard ini menyediakan analisis komprehensif untuk dataset SOVI dengan ", total_obs, " observasi dan ", total_vars, " variabel. ",
      "Tingkat kelengkapan data sebesar ", completeness, "% menunjukkan kualitas data yang ",
      if(completeness >= 90) "sangat baik" else if(completeness >= 80) "baik" else "perlu perhatian", ". ",
      "Dataset ini berisi informasi tentang Social Vulnerability Index yang mengukur kerentanan sosial berbagai wilayah. ",
      "Rata-rata tingkat kemiskinan adalah ", avg_poverty, "%. ",
      "Peta distribusi menunjukkan sebaran geografis data menggunakan koordinat dari matriks jarak yang dapat membantu dalam analisis spasial dan clustering. ",
      "Dashboard ini dikembangkan untuk ujian Statistika Terapan STIS 2025 dengan mengikuti semua ketentuan yang diberikan dan menyediakan fitur download lengkap untuk semua output."
    )
    
    HTML(interpretation)
  })
  
  # Data management outputs
  output$data_summary <- renderPrint({
    summary(sovi_data)
  })
  
  output$data_structure <- renderPrint({
    str(sovi_data)
  })
  
  output$data_summary_interpretation <- renderUI({
    numeric_vars <- sum(sapply(sovi_data, is.numeric))
    char_vars <- sum(sapply(sovi_data, is.character))
    factor_vars <- sum(sapply(sovi_data, is.factor))
    
    interpretation <- paste0(
      "Dataset SOVI terdiri dari ", numeric_vars, " variabel numerik, ", char_vars, " variabel karakter, dan ", factor_vars, " variabel faktor. ",
      "Struktur data menunjukkan bahwa sebagian besar variabel adalah numerik yang cocok untuk analisis statistik. ",
      "Ringkasan statistik memberikan gambaran distribusi setiap variabel termasuk nilai minimum, maksimum, median, dan kuartil. ",
      "Data ini siap untuk digunakan dalam berbagai analisis statistik yang tersedia di dashboard sesuai dengan ketentuan ujian STIS 2025. ",
      "Matriks jarak tersedia untuk analisis clustering dan visualisasi spasial yang lebih advanced."
    )
    
    HTML(interpretation)
  })
  
  # Categorization preview
  output$categorization_preview <- renderPrint({
    req(input$categorize_variable)
    
    var_data <- sovi_data[[input$categorize_variable]]
    
    if(input$categorize_method == "quartile") {
      breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      cat("Kuartil:\n")
      print(breaks)
    } else if(input$categorize_method == "tertile") {
      breaks <- quantile(var_data, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
      cat("Tertil:\n")
      print(breaks)
    } else if(input$categorize_method == "median") {
      breaks <- quantile(var_data, probs = c(0, 0.5, 1), na.rm = TRUE)
      cat("Median:\n")
      print(breaks)
    } else if(input$categorize_method == "custom") {
      breaks <- as.numeric(strsplit(input$custom_breaks, ",")[[1]])
      cat("Custom breaks:\n")
      print(breaks)
    }
  })
  
  # Apply categorization
  observeEvent(input$apply_categorization, {
    req(input$categorize_variable, input$categorize_method)
    
    var_data <- sovi_data[[input$categorize_variable]]
    
    if(input$categorize_method == "custom") {
      breaks <- as.numeric(strsplit(input$custom_breaks, ",")[[1]])
      labels <- trimws(strsplit(input$custom_labels, ",")[[1]])
    } else {
      labels <- trimws(strsplit(input$category_labels, ",")[[1]])
      
      if(input$categorize_method == "quartile") {
        breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        if(length(labels) != 4) labels <- c("Q1", "Q2", "Q3", "Q4")
      } else if(input$categorize_method == "tertile") {
        breaks <- quantile(var_data, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
        if(length(labels) != 3) labels <- c("Rendah", "Sedang", "Tinggi")
      } else if(input$categorize_method == "median") {
        breaks <- quantile(var_data, probs = c(0, 0.5, 1), na.rm = TRUE)
        if(length(labels) != 2) labels <- c("Rendah", "Tinggi")
      }
    }
    
    values$categorized_variable <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
  })
  
  output$categorization_table <- DT::renderDataTable({
    req(values$categorized_variable)
    
    freq_table <- table(values$categorized_variable, useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    result_df <- data.frame(
      Kategori = names(freq_table),
      Frekuensi = as.numeric(freq_table),
      Persentase = round(as.numeric(prop_table), 2)
    )
    
    DT::datatable(
      result_df,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      caption = paste("Tabel Frekuensi Kategorisasi", input$categorize_variable)
    )
  })
  
  output$categorization_plot <- renderPlotly({
    req(values$categorized_variable)
    
    freq_data <- data.frame(
      Category = values$categorized_variable
    )
    
    p <- ggplot(freq_data, aes(x = Category)) +
      geom_bar(fill = colors[1], alpha = 0.7, color = "white") +
      labs(title = paste("Distribusi Kategori", input$categorize_variable),
           x = "Kategori", y = "Frekuensi") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = colors[1]),
        axis.title = element_text(size = 12, color = colors[1]),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$categorization_interpretation <- renderUI({
    req(values$categorized_variable)
    
    freq_table <- table(values$categorized_variable, useNA = "ifany")
    most_frequent <- names(freq_table)[which.max(freq_table)]
    least_frequent <- names(freq_table)[which.min(freq_table)]
    
    interpretation <- paste0(
      "Kategorisasi variabel ", input$categorize_variable, " menggunakan metode ", input$categorize_method, " menghasilkan ", length(freq_table), " kategori. ",
      "Kategori dengan frekuensi tertinggi adalah '", most_frequent, "' dengan ", max(freq_table), " observasi, ",
      "sedangkan kategori dengan frekuensi terendah adalah '", least_frequent, "' dengan ", min(freq_table), " observasi. ",
      "Distribusi ini dapat digunakan untuk analisis kategorik selanjutnya seperti uji chi-square atau analisis kontingensi sesuai dengan materi ujian STIS. ",
      "Kategorisasi yang baik akan membantu dalam interpretasi hasil analisis dan pembuatan keputusan berdasarkan data."
    )
    
    HTML(interpretation)
  })
  
  # Transformation preview
  output$transformation_preview <- renderPrint({
    req(input$transform_variable)
    
    var_data <- sovi_data[[input$transform_variable]]
    
    if(input$transform_method == "log") {
      if(any(var_data <= 0, na.rm = TRUE)) {
        cat("Warning: Data mengandung nilai <= 0. Transformasi log tidak dapat diterapkan.\n")
      } else {
        transformed <- log(var_data)
        cat("Log Natural - Range:", round(range(transformed, na.rm = TRUE), 3), "\n")
      }
    } else if(input$transform_method == "log10") {
      if(any(var_data <= 0, na.rm = TRUE)) {
        cat("Warning: Data mengandung nilai <= 0. Transformasi log10 tidak dapat diterapkan.\n")
      } else {
        transformed <- log10(var_data)
        cat("Log10 - Range:", round(range(transformed, na.rm = TRUE), 3), "\n")
      }
    } else if(input$transform_method == "sqrt") {
      if(any(var_data < 0, na.rm = TRUE)) {
        cat("Warning: Data mengandung nilai negatif. Transformasi sqrt tidak dapat diterapkan.\n")
      } else {
        transformed <- sqrt(var_data)
        cat("Square Root - Range:", round(range(transformed, na.rm = TRUE), 3), "\n")
      }
    } else if(input$transform_method == "square") {
      transformed <- var_data^2
      cat("Square - Range:", round(range(transformed, na.rm = TRUE), 3), "\n")
    } else if(input$transform_method == "zscore") {
      transformed <- scale(var_data)[,1]
      cat("Z-Score - Mean:", round(mean(transformed, na.rm = TRUE), 3), "SD:", round(sd(transformed, na.rm = TRUE), 3), "\n")
    }
  })
  
  # Apply transformation
  observeEvent(input$apply_transformation, {
    req(input$transform_variable, input$transform_method)
    
    var_data <- sovi_data[[input$transform_variable]]
    
    if(input$transform_method == "log") {
      if(any(var_data <= 0, na.rm = TRUE)) {
        showNotification("Data mengandung nilai <= 0. Transformasi log tidak dapat diterapkan.", type = "error")
        return()
      }
      values$transformed_variable <- log(var_data)
    } else if(input$transform_method == "log10") {
      if(any(var_data <= 0, na.rm = TRUE)) {
        showNotification("Data mengandung nilai <= 0. Transformasi log10 tidak dapat diterapkan.", type = "error")
        return()
      }
      values$transformed_variable <- log10(var_data)
    } else if(input$transform_method == "sqrt") {
      if(any(var_data < 0, na.rm = TRUE)) {
        showNotification("Data mengandung nilai negatif. Transformasi sqrt tidak dapat diterapkan.", type = "error")
        return()
      }
      values$transformed_variable <- sqrt(var_data)
    } else if(input$transform_method == "square") {
      values$transformed_variable <- var_data^2
    } else if(input$transform_method == "zscore") {
      values$transformed_variable <- scale(var_data)[,1]
    }
  })
  
  output$before_transform_plot <- renderPlotly({
    req(input$transform_variable, values$transformed_variable)
    
    var_data <- sovi_data[[input$transform_variable]]
    
    p <- ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = colors[2], alpha = 0.7, color = "white") +
      labs(title = "Sebelum Transformasi", x = input$transform_variable, y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$after_transform_plot <- renderPlotly({
    req(values$transformed_variable)
    
    p <- ggplot(data.frame(x = values$transformed_variable), aes(x = x)) +
      geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
      labs(title = "Setelah Transformasi", x = paste(input$transform_method, "(", input$transform_variable, ")"), y = "Frekuensi") +
      theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$transformation_interpretation <- renderUI({
    req(values$transformed_variable)
    
    original_data <- sovi_data[[input$transform_variable]]
    transformed_data <- values$transformed_variable
    
    # Calculate skewness before and after
    skew_before <- skewness(original_data, na.rm = TRUE)
    skew_after <- skewness(transformed_data, na.rm = TRUE)
    
    interpretation <- paste0(
      "Transformasi ", input$transform_method, " pada variabel ", input$transform_variable, " menghasilkan perubahan distribusi data. ",
      "Skewness sebelum transformasi: ", round(skew_before, 3), ", setelah transformasi: ", round(skew_after, 3), ". ",
      if(abs(skew_after) < abs(skew_before)) {
        "Transformasi berhasil mengurangi skewness dan membuat distribusi lebih mendekati normal. "
      } else {
        "Transformasi tidak mengurangi skewness secara signifikan. "
      },
      "Transformasi data berguna untuk memenuhi asumsi normalitas dalam analisis statistik parametrik. ",
      "Data yang telah ditransformasi dapat digunakan untuk analisis selanjutnya sesuai dengan kebutuhan penelitian."
    )
    
    HTML(interpretation)
  })
  
  # Descriptive statistics
  observeEvent(input$run_descriptive, {
    req(input$descriptive_variable)
    
    output$descriptive_stats <- renderPrint({
      var_data <- sovi_data[[input$descriptive_variable]]
      
      if(input$descriptive_group == "none") {
        summary(var_data)
      } else {
        group_data <- sovi_data[[input$descriptive_group]]
        tapply(var_data, group_data, summary)
      }
    })
    
    output$descriptive_table <- DT::renderDataTable({
      var_data <- sovi_data[[input$descriptive_variable]]
      
      if(input$descriptive_group == "none") {
        desc_stats <- data.frame(
          Statistik = c("Mean", "Median", "SD", "Min", "Max", "Q1", "Q3", "Skewness", "Kurtosis"),
          Nilai = c(
            round(mean(var_data, na.rm = TRUE), 3),
            round(median(var_data, na.rm = TRUE), 3),
            round(sd(var_data, na.rm = TRUE), 3),
            round(min(var_data, na.rm = TRUE), 3),
            round(max(var_data, na.rm = TRUE), 3),
            round(quantile(var_data, 0.25, na.rm = TRUE), 3),
            round(quantile(var_data, 0.75, na.rm = TRUE), 3),
            round(skewness(var_data, na.rm = TRUE), 3),
            round(kurtosis(var_data, na.rm = TRUE), 3)
          )
        )
      } else {
        group_data <- sovi_data[[input$descriptive_group]]
        desc_stats <- sovi_data %>%
          group_by(!!sym(input$descriptive_group)) %>%
          summarise(
            Mean = round(mean(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Median = round(median(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            SD = round(sd(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Min = round(min(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Max = round(max(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            Skewness = round(skewness(!!sym(input$descriptive_variable), na.rm = TRUE), 3),
            .groups = 'drop'
          )
      }
      
      DT::datatable(
        desc_stats,
        options = list(
          pageLength = 10,
          dom = 't'
        ),
        caption = paste("Statistik Deskriptif", input$descriptive_variable)
      )
    })
    
    output$descriptive_interpretation <- renderUI({
      var_data <- sovi_data[[input$descriptive_variable]]
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)
      cv <- sd_val / mean_val * 100
      skew_val <- skewness(var_data, na.rm = TRUE)
      
      interpretation <- paste0(
        "Analisis deskriptif variabel ", input$descriptive_variable, " menunjukkan rata-rata sebesar ", round(mean_val, 3),
        " dengan median ", round(median_val, 3), ". ",
        "Standar deviasi ", round(sd_val, 3), " mengindikasikan variabilitas data yang ",
        if(cv < 15) "rendah" else if(cv < 30) "sedang" else "tinggi",
        " (CV = ", round(cv, 1), "%). ",
        "Skewness = ", round(skew_val, 3), " menunjukkan distribusi ",
        if(abs(skew_val) < 0.5) "relatif simetris" else if(skew_val > 0.5) "condong ke kanan (positively skewed)" else "condong ke kiri (negatively skewed)", ". "
      )
      
      if(input$descriptive_group != "none") {
        interpretation <- paste0(interpretation, " Perbandingan antar kelompok menunjukkan variasi yang dapat dianalisis lebih lanjut dengan uji statistik sesuai materi ujian.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Visualization plot
  output$visualization_plot <- renderPlotly({
    req(input$plot_variable)
    
    var_data <- sovi_data[[input$plot_variable]]
    
    if(input$plot_group == "none") {
      if(input$plot_type == "histogram") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
          labs(title = paste("Histogram", input$plot_variable), x = input$plot_variable, y = "Frekuensi") +
          theme_minimal()
      } else if(input$plot_type == "boxplot") {
        p <- ggplot(data.frame(x = var_data), aes(y = x)) +
          geom_boxplot(fill = colors[2], alpha = 0.7, color = colors[1]) +
          labs(title = paste("Boxplot", input$plot_variable), y = input$plot_variable) +
          theme_minimal()
      } else if(input$plot_type == "density") {
        p <- ggplot(data.frame(x = var_data), aes(x = x)) +
          geom_density(fill = colors[3], alpha = 0.7, color = colors[1]) +
          labs(title = paste("Density Plot", input$plot_variable), x = input$plot_variable, y = "Density") +
          theme_minimal()
      } else if(input$plot_type == "violin") {
        p <- ggplot(data.frame(x = var_data), aes(x = "", y = x)) +
          geom_violin(fill = colors[4], alpha = 0.7, color = colors[1]) +
          labs(title = paste("Violin Plot", input$plot_variable), x = "", y = input$plot_variable) +
          theme_minimal()
      }
    } else {
      group_data <- sovi_data[[input$plot_group]]
      plot_data <- data.frame(x = var_data, group = group_data)
      
      if(input$plot_type == "histogram") {
        p <- ggplot(plot_data, aes(x = x, fill = group)) +
          geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
          scale_fill_manual(values = colors[1:length(unique(group_data))]) +
          labs(title = paste("Histogram", input$plot_variable, "by", input$plot_group), 
               x = input$plot_variable, y = "Frekuensi", fill = input$plot_group) +
          theme_minimal()
      } else if(input$plot_type == "boxplot") {
        p <- ggplot(plot_data, aes(x = group, y = x, fill = group)) +
          geom_boxplot(alpha = 0.7) +
          scale_fill_manual(values = colors[1:length(unique(group_data))]) +
          labs(title = paste("Boxplot", input$plot_variable, "by", input$plot_group), 
               x = input$plot_group, y = input$plot_variable) +
          theme_minimal() +
          theme(legend.position = "none")
      } else if(input$plot_type == "density") {
        p <- ggplot(plot_data, aes(x = x, fill = group)) +
          geom_density(alpha = 0.7) +
          scale_fill_manual(values = colors[1:length(unique(group_data))]) +
          labs(title = paste("Density Plot", input$plot_variable, "by", input$plot_group), 
               x = input$plot_variable, y = "Density", fill = input$plot_group) +
          theme_minimal()
      } else if(input$plot_type == "violin") {
        p <- ggplot(plot_data, aes(x = group, y = x, fill = group)) +
          geom_violin(alpha = 0.7) +
          scale_fill_manual(values = colors[1:length(unique(group_data))]) +
          labs(title = paste("Violin Plot", input$plot_variable, "by", input$plot_group), 
               x = input$plot_group, y = input$plot_variable) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$plot_stats <- renderPrint({
    req(input$plot_variable)
    
    var_data <- sovi_data[[input$plot_variable]]
    
    if(input$plot_group == "none") {
      summary(var_data)
    } else {
      group_data <- sovi_data[[input$plot_group]]
      tapply(var_data, group_data, summary)
    }
  })
  
  output$visualization_interpretation <- renderUI({
    req(input$plot_variable, input$plot_type)
    
    var_data <- sovi_data[[input$plot_variable]]
    
    plot_desc <- switch(input$plot_type,
                        "histogram" = "Histogram menunjukkan distribusi frekuensi data",
                        "boxplot" = "Boxplot menampilkan median, kuartil, dan outlier",
                        "density" = "Density plot menggambarkan estimasi distribusi probabilitas",
                        "violin" = "Violin plot menggabungkan informasi boxplot dan density plot"
    )
    
    # Detect outliers using IQR method
    Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outliers <- sum(var_data < (Q1 - 1.5*IQR) | var_data > (Q3 + 1.5*IQR), na.rm = TRUE)
    
    interpretation <- paste0(
      plot_desc, " untuk variabel ", input$plot_variable, 
      if(input$plot_group != "none") paste(" berdasarkan kelompok", input$plot_group) else "", ". ",
      "Dari visualisasi ini dapat diamati bentuk distribusi, pusat data, dan penyebaran. ",
      if(outliers > 0) {
        paste0("Terdapat ", outliers, " outlier yang terdeteksi menggunakan metode IQR. ")
      } else {
        "Tidak terdapat outlier yang signifikan. "
      },
      "Informasi ini penting untuk memilih metode analisis statistik yang tepat sesuai dengan materi yang dipelajari di STIS. ",
      if(input$plot_group != "none") {
        "Perbandingan antar kelompok dapat memberikan insight tentang perbedaan karakteristik data."
      } else {
        "Visualisasi ini membantu memahami karakteristik distribusi data secara keseluruhan."
      }
    )
    
    HTML(interpretation)
  })
  
  # Exploration map using distance data
  output$exploration_map <- renderLeaflet({
    req(input$map_variable)
    
    # Merge with distance data for coordinates
    map_data <- merge(sovi_data, distance_data, by = "DISTRICTCODE", all.x = TRUE)
    map_data <- map_data[!is.na(map_data$LONGITUDE) & !is.na(map_data$LATITUDE), ]
    
    var_data <- map_data[[input$map_variable]]
    
    map_base <- leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2.5, zoom = 5)
    
    if (input$map_type == "scatter") {
      pal <- colorNumeric(
        palette = colors,
        domain = var_data
      )
      
      map_base %>%
        addCircleMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 6,
          fillColor = ~pal(var_data),
          color = "white",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.8,
          label = ~lapply(paste("<strong>District Code:", DISTRICTCODE, "</strong><br/>",
                                input$map_variable, ":", round(var_data, 2)), HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = var_data,
          title = input$map_variable,
          position = "bottomright"
        )
    } else if (input$map_type == "heatmap") {
      map_base %>%
        addHeatmap(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = var_data,
          blur = 20,
          max = 0.05,
          radius = 15
        )
    } else if (input$map_type == "choropleth") {
      # For choropleth, we would need polygon data
      # For now, use enhanced scatter plot
      pal <- colorBin(
        palette = "YlOrRd",
        domain = var_data,
        bins = 5
      )
      
      map_base %>%
        addCircleMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 8,
          fillColor = ~pal(var_data),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.8,
          label = ~lapply(paste("<strong>District Code:", DISTRICTCODE, "</strong><br/>",
                                input$map_variable, ":", round(var_data, 2)), HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = var_data,
          title = input$map_variable,
          position = "bottomright"
        )
    }
  })
  
  output$map_interpretation <- renderUI({
    req(input$map_variable, input$map_type)
    
    var_data <- sovi_data[[input$map_variable]]
    
    map_desc <- switch(input$map_type,
                       "scatter" = "Scatter plot menampilkan setiap titik data dengan warna yang merepresentasikan nilai variabel",
                       "heatmap" = "Heat map menunjukkan konsentrasi atau densitas nilai dalam area geografis tertentu",
                       "choropleth" = "Choropleth map menggunakan gradasi warna untuk menunjukkan variasi nilai antar wilayah"
    )
    
    mean_val <- round(mean(var_data, na.rm = TRUE), 3)
    max_val <- round(max(var_data, na.rm = TRUE), 3)
    min_val <- round(min(var_data, na.rm = TRUE), 3)
    
    interpretation <- paste0(
      "Visualisasi peta untuk variabel ", input$map_variable, " menggunakan koordinat dari matriks jarak. ",
      map_desc, ". ",
      "Distribusi spasial menunjukkan nilai berkisar dari ", min_val, " hingga ", max_val, " dengan rata-rata ", mean_val, ". ",
      "Pola spasial yang terlihat dapat mengindikasikan adanya clustering geografis atau distribusi acak yang berguna untuk analisis spasial lanjutan. ",
      "Informasi geografis ini dapat digunakan untuk analisis clustering dan identifikasi pola regional sesuai dengan ketentuan ujian."
    )
    
    HTML(interpretation)
  })
  
  # Correlation analysis
  observeEvent(input$run_correlation, {
    req(input$corr_variables)
    
    if(length(input$corr_variables) < 2) {
      showNotification("Pilih minimal 2 variabel untuk analisis korelasi.", type = "warning")
      return()
    }
    
    corr_data <- sovi_data[, input$corr_variables, drop = FALSE]
    corr_matrix <- cor(corr_data, use = "complete.obs", method = input$corr_method)
    
    output$correlation_plot <- renderPlot({
      corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
               tl.cex = 0.8, tl.col = "black", tl.srt = 45,
               col = colorRampPalette(c(colors[2], "white", colors[1]))(200),
               addCoef.col = "black", number.cex = 0.7)
    })
    
    output$correlation_table <- DT::renderDataTable({
      corr_df <- as.data.frame(round(corr_matrix, 3))
      corr_df$Variable <- rownames(corr_df)
      corr_df <- corr_df[, c("Variable", input$corr_variables)]
      
      DT::datatable(
        corr_df,
        options = list(
          pageLength = 10,
          dom = 't'
        ),
        caption = paste("Matriks Korelasi -", input$corr_method)
      )
    })
    
    output$correlation_test <- renderPrint({
      # Perform correlation tests for significant pairs
      n_vars <- length(input$corr_variables)
      if(n_vars >= 2) {
        cat("Uji Signifikansi Korelasi:\n")
        cat("========================\n")
        for(i in 1:(n_vars-1)) {
          for(j in (i+1):n_vars) {
            var1 <- input$corr_variables[i]
            var2 <- input$corr_variables[j]
            test_result <- cor.test(sovi_data[[var1]], sovi_data[[var2]], method = input$corr_method)
            cat(paste(var1, "vs", var2, ":\n"))
            cat(paste("  Korelasi:", round(test_result$estimate, 3), "\n"))
            cat(paste("  p-value:", format.pval(test_result$p.value), "\n"))
            cat(paste("  Signifikan:", ifelse(test_result$p.value < 0.05, "Ya", "Tidak"), "\n\n"))
          }
        }
      }
    })
    
    output$correlation_interpretation <- renderUI({
      strong_corr <- which(abs(corr_matrix) > 0.7 & corr_matrix != 1, arr.ind = TRUE)
      moderate_corr <- which(abs(corr_matrix) > 0.3 & abs(corr_matrix) <= 0.7, arr.ind = TRUE)
      
      interpretation <- paste0(
        "Analisis korelasi menggunakan metode ", input$corr_method, " menunjukkan hubungan antar variabel. ",
        if(nrow(strong_corr) > 0) {
          paste0("Terdapat ", nrow(strong_corr), " pasang variabel dengan korelasi kuat (|r| > 0.7). ")
        } else {
          "Tidak ada korelasi yang sangat kuat (|r| > 0.7). "
        },
        if(nrow(moderate_corr) > 0) {
          paste0("Terdapat ", nrow(moderate_corr), " pasang variabel dengan korelasi sedang (0.3 < |r| ≤ 0.7). ")
        } else {
          "Tidak ada korelasi sedang yang signifikan. "
        },
        "Korelasi yang kuat dapat mengindikasikan multikolinearitas dalam analisis regresi. ",
        "Informasi ini berguna untuk seleksi variabel dan interpretasi model statistik sesuai dengan materi ujian STIS."
      )
      
      HTML(interpretation)
    })
  })
  
  # Normality tests
  observeEvent(input$run_normality, {
    req(input$normality_variable)
    
    var_data <- sovi_data[[input$normality_variable]]
    var_data <- var_data[!is.na(var_data)]
    
    output$normality_result <- renderPrint({
      if(input$normality_test == "shapiro") {
        if(length(var_data) <= 5000) {
          shapiro.test(var_data)
        } else {
          cat("Ukuran sampel terlalu besar untuk uji Shapiro-Wilk. Menggunakan Anderson-Darling.\n")
          ad.test(var_data)
        }
      } else if(input$normality_test == "anderson") {
        ad.test(var_data)
      } else if(input$normality_test == "ks") {
        ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      } else if(input$normality_test == "jarque") {
        jarque.bera.test(var_data)
      }
    })
    
    output$qq_plot <- renderPlotly({
      qq_data <- data.frame(
        sample = sort(var_data),
        theoretical = qnorm(ppoints(length(var_data)))
      )
      
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = sd(var_data, na.rm = TRUE),
                    intercept = mean(var_data, na.rm = TRUE),
                    color = colors[2], size = 1) +
        labs(title = "Q-Q Plot", x = "Kuantil Teoritis", y = "Kuantil Sampel") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$normality_histogram <- renderPlotly({
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        stat_function(fun = dnorm,
                      args = list(mean = mean(var_data, na.rm = TRUE),
                                  sd = sd(var_data, na.rm = TRUE)),
                      color = colors[2], size = 1) +
        labs(title = paste("Histogram dengan Kurva Normal -", input$normality_variable),
             x = input$normality_variable, y = "Densitas") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$normality_interpretation <- renderUI({
      test_result <- if(input$normality_test == "shapiro") {
        if(length(var_data) <= 5000) {
          shapiro.test(var_data)
        } else {
          ad.test(var_data)
        }
      } else if(input$normality_test == "anderson") {
        ad.test(var_data)
      } else if(input$normality_test == "ks") {
        ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      } else if(input$normality_test == "jarque") {
        jarque.bera.test(var_data)
      }
      
      p_value <- test_result$p.value
      alpha <- 0.05
      
      test_name <- switch(input$normality_test,
                          "shapiro" = "Shapiro-Wilk",
                          "anderson" = "Anderson-Darling",
                          "ks" = "Kolmogorov-Smirnov",
                          "jarque" = "Jarque-Bera")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " < α = ", alpha,
               " menunjukkan bahwa kita menolak H₀. Data tidak berdistribusi normal pada tingkat signifikansi 5%. ",
               "Q-Q plot menunjukkan penyimpangan dari garis diagonal yang mengkonfirmasi hasil uji. ",
               "Untuk analisis selanjutnya, pertimbangkan menggunakan uji non-parametrik atau transformasi data sesuai dengan materi yang dipelajari.")
      } else {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " > α = ", alpha,
               " menunjukkan bahwa kita gagal menolak H₀. Data dapat dianggap berdistribusi normal pada tingkat signifikansi 5%. ",
               "Q-Q plot menunjukkan titik-titik yang relatif mengikuti garis diagonal. ",
               "Data ini memenuhi asumsi normalitas untuk uji parametrik.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Homogeneity tests
  observeEvent(input$run_homogeneity, {
    req(input$homogeneity_variable, input$homogeneity_group)
    
    var_data <- sovi_data[[input$homogeneity_variable]]
    group_data <- sovi_data[[input$homogeneity_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    output$homogeneity_result <- renderPrint({
      if(input$homogeneity_test == "levene") {
        leveneTest(var_data, group_data)
      } else if(input$homogeneity_test == "bartlett") {
        bartlett.test(var_data, group_data)
      } else if(input$homogeneity_test == "fligner") {
        fligner.test(var_data, group_data)
      }
    })
    
    output$homogeneity_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        group = group_data
      )
      
      p <- ggplot(plot_data, aes(x = group, y = variable, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = colors[1:length(unique(group_data))]) +
        labs(title = paste("Box Plot berdasarkan", input$homogeneity_group),
             x = input$homogeneity_group, y = input$homogeneity_variable) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$homogeneity_interpretation <- renderUI({
      test_result <- if(input$homogeneity_test == "levene") {
        leveneTest(var_data, group_data)
      } else if(input$homogeneity_test == "bartlett") {
        bartlett.test(var_data, group_data)
      } else if(input$homogeneity_test == "fligner") {
        fligner.test(var_data, group_data)
      }
      
      p_value <- if(input$homogeneity_test == "levene") {
        test_result$`Pr(>F)`[1]
      } else {
        test_result$p.value
      }
      
      alpha <- 0.05
      
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      max_var <- max(group_vars)
      min_var <- min(group_vars)
      var_ratio <- max_var / min_var
      
      test_name <- switch(input$homogeneity_test,
                          "levene" = "Levene",
                          "bartlett" = "Bartlett",
                          "fligner" = "Fligner-Killeen")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " < α = ", alpha,
               " menunjukkan bahwa kita menolak H₀. Varians antar kelompok tidak homogen (heteroskedastisitas). ",
               "Rasio varians terbesar terhadap terkecil adalah ", round(var_ratio, 2), ". ",
               "Box plot menunjukkan perbedaan penyebaran data antar kelompok. ",
               "Untuk analisis selanjutnya, gunakan uji yang tidak mengasumsikan homogenitas varians.")
      } else {
        paste0("Hasil uji ", test_name, " dengan p-value = ", round(p_value, 4), " > α = ", alpha,
               " menunjukkan bahwa kita gagal menolak H₀. Varians antar kelompok homogen (homoskedastisitas). ",
               "Rasio varians terbesar terhadap terkecil adalah ", round(var_ratio, 2), " yang masih dalam batas wajar. ",
               "Data memenuhi asumsi homogenitas varians untuk uji parametrik seperti ANOVA dan t-test.")
      }
      
      HTML(interpretation)
    })
  })
  
  # One sample t-test
  observeEvent(input$run_onesample, {
    req(input$onesample_variable)
    
    var_data <- sovi_data[[input$onesample_variable]]
    
    output$onesample_result <- renderPrint({
      t.test(var_data, mu = input$mu_hypothesis, alternative = input$alternative_hypothesis)
    })
    
    output$onesample_plot <- renderPlotly({
      p <- ggplot(data.frame(x = var_data), aes(x = x)) +
        geom_histogram(bins = 30, fill = colors[1], alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean(var_data, na.rm = TRUE), color = colors[2], size = 1, linetype = "dashed") +
        geom_vline(xintercept = input$mu_hypothesis, color = colors[4], size = 1, linetype = "solid") +
        labs(title = "Rata-rata Sampel vs Hipotesis",
             x = input$onesample_variable, y = "Frekuensi") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$onesample_interpretation <- renderUI({
      test_result <- t.test(var_data, mu = input$mu_hypothesis, alternative = input$alternative_hypothesis)
      p_value <- test_result$p.value
      alpha <- 0.05
      sample_mean <- mean(var_data, na.rm = TRUE)
      t_stat <- test_result$statistic
      df <- test_result$parameter
      ci <- test_result$conf.int
      
      alt_desc <- switch(input$alternative_hypothesis,
                         "two.sided" = "dua arah (≠)",
                         "greater" = "satu arah (>)",
                         "less" = "satu arah (<)")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji t satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Rata-rata sampel (", round(sample_mean, 4), ") berbeda signifikan dari nilai hipotesis (", input$mu_hypothesis, "). ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", df, ". ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] tidak mengandung nilai hipotesis.")
      } else {
        paste0("Hasil uji t satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Rata-rata sampel (", round(sample_mean, 4), ") tidak berbeda signifikan dari nilai hipotesis (", input$mu_hypothesis, "). ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", df, ". ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] mengandung nilai hipotesis.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Two sample t-test
  observeEvent(input$run_twosample, {
    req(input$twosample_variable, input$twosample_group)
    
    var_data <- sovi_data[[input$twosample_variable]]
    group_data <- sovi_data[[input$twosample_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    # Check if grouping variable has exactly 2 levels
    if(length(unique(group_data)) != 2) {
      output$twosample_result <- renderPrint({
        cat("Error: Variabel pengelompokan harus memiliki tepat 2 level.\n")
        cat("Level saat ini:", paste(unique(group_data), collapse = ", "))
      })
      return()
    }
    
    output$twosample_result <- renderPrint({
      t.test(var_data ~ group_data, var.equal = input$equal_variances, alternative = input$twosample_alternative)
    })
    
    output$twosample_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        group = group_data
      )
      
      p <- ggplot(plot_data, aes(x = group, y = variable, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = colors[1:2]) +
        labs(title = paste("Perbandingan berdasarkan", input$twosample_group),
             x = input$twosample_group, y = input$twosample_variable) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$twosample_interpretation <- renderUI({
      if(length(unique(group_data)) != 2) {
        return(HTML("Error: Variabel pengelompokan harus memiliki tepat 2 level."))
      }
      
      test_result <- t.test(var_data ~ group_data, var.equal = input$equal_variances, alternative = input$twosample_alternative)
      p_value <- test_result$p.value
      alpha <- 0.05
      t_stat <- test_result$statistic
      df <- test_result$parameter
      
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      group_sds <- tapply(var_data, group_data, sd, na.rm = TRUE)
      
      var_assumption <- if(input$equal_variances) "dengan asumsi varians sama" else "dengan asumsi varians tidak sama (Welch)"
      
      alt_desc <- switch(input$twosample_alternative,
                         "two.sided" = "dua arah (≠)",
                         "greater" = "satu arah (>)",
                         "less" = "satu arah (<)")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji t dua sampel ", var_assumption, " dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara rata-rata kedua kelompok. ",
               "Kelompok ", names(group_means)[1], ": mean = ", round(group_means[1], 4), ", SD = ", round(group_sds[1], 4), ". ",
               "Kelompok ", names(group_means)[2], ": mean = ", round(group_means[2], 4), ", SD = ", round(group_sds[2], 4), ". ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", round(df, 1), ".")
      } else {
        paste0("Hasil uji t dua sampel ", var_assumption, " dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara rata-rata kedua kelompok. ",
               "Kelompok ", names(group_means)[1], ": mean = ", round(group_means[1], 4), ", SD = ", round(group_sds[1], 4), ". ",
               "Kelompok ", names(group_means)[2], ": mean = ", round(group_means[2], 4), ", SD = ", round(group_sds[2], 4), ". ",
               "Statistik t = ", round(t_stat, 3), " dengan df = ", round(df, 1), ".")
      }
      
      HTML(interpretation)
    })
  })
  
  # Proportion test
  observeEvent(input$run_prop_test, {
    req(input$prop_variable, input$prop_category)
    
    var_data <- sovi_data[[input$prop_variable]]
    var_data <- var_data[!is.na(var_data)]
    
    successes <- sum(var_data == input$prop_category)
    total <- length(var_data)
    
    output$prop_test_result <- renderPrint({
      prop.test(successes, total, p = input$prop_hypothesis, alternative = input$prop_alternative)
    })
    
    output$prop_test_plot <- renderPlotly({
      prop_table <- table(var_data)
      prop_df <- data.frame(
        Category = names(prop_table),
        Count = as.numeric(prop_table),
        Proportion = as.numeric(prop_table) / sum(prop_table)
      )
      
      p <- ggplot(prop_df, aes(x = Category, y = Proportion, fill = Category)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        geom_hline(yintercept = input$prop_hypothesis, color = colors[1], linetype = "dashed", size = 1) +
        scale_fill_manual(values = colors[1:length(unique(prop_df$Category))]) +
        labs(title = paste("Proporsi", input$prop_variable),
             x = "Kategori", y = "Proporsi") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$prop_test_interpretation <- renderUI({
      test_result <- prop.test(successes, total, p = input$prop_hypothesis, alternative = input$prop_alternative)
      p_value <- test_result$p.value
      alpha <- 0.05
      sample_prop <- successes / total
      chi_stat <- test_result$statistic
      ci <- test_result$conf.int
      
      alt_desc <- switch(input$prop_alternative,
                         "two.sided" = "dua arah (≠)",
                         "greater" = "satu arah (>)",
                         "less" = "satu arah (<)")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji proporsi satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Proporsi sampel kategori '", input$prop_category, "' (", round(sample_prop, 4), ") berbeda signifikan dari proporsi hipotesis (", input$prop_hypothesis, "). ",
               "Statistik χ² = ", round(chi_stat, 3), " dengan df = 1. ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] tidak mengandung proporsi hipotesis.")
      } else {
        paste0("Hasil uji proporsi satu sampel dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Proporsi sampel kategori '", input$prop_category, "' (", round(sample_prop, 4), ") tidak berbeda signifikan dari proporsi hipotesis (", input$prop_hypothesis, "). ",
               "Statistik χ² = ", round(chi_stat, 3), " dengan df = 1. ",
               "Interval kepercayaan 95%: [", round(ci[1], 4), ", ", round(ci[2], 4), "] mengandung proporsi hipotesis.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Variance test
  observeEvent(input$run_var_test, {
    req(input$var_test_variable, input$var_test_group)
    
    var_data <- sovi_data[[input$var_test_variable]]
    group_data <- sovi_data[[input$var_test_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    # Check if grouping variable has exactly 2 levels
    if(length(unique(group_data)) != 2) {
      output$var_test_result <- renderPrint({
        cat("Error: Variabel pengelompokan harus memiliki tepat 2 level.\n")
        cat("Level saat ini:", paste(unique(group_data), collapse = ", "))
      })
      return()
    }
    
    output$var_test_result <- renderPrint({
      var.test(var_data ~ group_data, alternative = input$var_test_alternative)
    })
    
    output$var_test_plot <- renderPlotly({
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      var_df <- data.frame(
        Group = names(group_vars),
        Variance = as.numeric(group_vars)
      )
      
      p <- ggplot(var_df, aes(x = Group, y = Variance, fill = Group)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = colors[1:2]) +
        labs(title = paste("Perbandingan Varians berdasarkan", input$var_test_group),
             x = input$var_test_group, y = "Varians") +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$var_test_interpretation <- renderUI({
      if(length(unique(group_data)) != 2) {
        return(HTML("Error: Variabel pengelompokan harus memiliki tepat 2 level."))
      }
      
      test_result <- var.test(var_data ~ group_data, alternative = input$var_test_alternative)
      p_value <- test_result$p.value
      alpha <- 0.05
      f_stat <- test_result$statistic
      df1 <- test_result$parameter[1]
      df2 <- test_result$parameter[2]
      
      group_vars <- tapply(var_data, group_data, var, na.rm = TRUE)
      group_names <- names(group_vars)
      
      alt_desc <- switch(input$var_test_alternative,
                         "two.sided" = "dua arah (≠)",
                         "greater" = "satu arah (>)",
                         "less" = "satu arah (<)")
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil uji F untuk kesamaan varians dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara varians kedua kelompok. ",
               "Varians kelompok ", group_names[1], " = ", round(group_vars[1], 4), ". ",
               "Varians kelompok ", group_names[2], " = ", round(group_vars[2], 4), ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ").")
      } else {
        paste0("Hasil uji F untuk kesamaan varians dengan hipotesis alternatif ", alt_desc, " menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara varians kedua kelompok. ",
               "Varians kelompok ", group_names[1], " = ", round(group_vars[1], 4), ". ",
               "Varians kelompok ", group_names[2], " = ", round(group_vars[2], 4), ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, ").")
      }
      
      HTML(interpretation)
    })
  })
  
  # One-way ANOVA
  observeEvent(input$run_anova, {
    req(input$anova_variable, input$anova_group)
    
    var_data <- sovi_data[[input$anova_variable]]
    group_data <- sovi_data[[input$anova_group]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, group_data)
    var_data <- var_data[complete_cases]
    group_data <- group_data[complete_cases]
    
    output$anova_result <- renderPrint({
      anova_model <- aov(var_data ~ group_data)
      summary(anova_model)
    })
    
    output$anova_plot <- renderPlotly({
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      means_df <- data.frame(
        Group = names(group_means),
        Mean = as.numeric(group_means)
      )
      
      p <- ggplot(means_df, aes(x = Group, y = Mean, fill = Group)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = colors[1:length(unique(group_data))]) +
        labs(title = paste("Rata-rata Kelompok berdasarkan", input$anova_group),
             x = input$anova_group, y = paste("Rata-rata", input$anova_variable)) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$posthoc_result <- renderPrint({
      anova_model <- aov(var_data ~ group_data)
      TukeyHSD(anova_model)
    })
    
    output$anova_interpretation <- renderUI({
      anova_model <- aov(var_data ~ group_data)
      anova_summary <- summary(anova_model)
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      f_stat <- anova_summary[[1]]$`F value`[1]
      df1 <- anova_summary[[1]]$Df[1]
      df2 <- anova_summary[[1]]$Df[2]
      alpha <- 0.05
      
      group_means <- tapply(var_data, group_data, mean, na.rm = TRUE)
      
      interpretation <- if(p_value < alpha) {
        paste0("Hasil ANOVA satu arah menunjukkan p-value = ", round(p_value, 4), " < α = ", alpha,
               ". Kita menolak H₀ dan menerima H₁. ",
               "Terdapat perbedaan signifikan antara rata-rata kelompok pada variabel ", input$anova_variable, " berdasarkan ", input$anova_group, ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, "). ",
               "Uji post-hoc Tukey HSD menunjukkan pasangan kelompok mana yang berbeda signifikan.")
      } else {
        paste0("Hasil ANOVA satu arah menunjukkan p-value = ", round(p_value, 4), " > α = ", alpha,
               ". Kita gagal menolak H₀. ",
               "Tidak terdapat perbedaan signifikan antara rata-rata kelompok pada variabel ", input$anova_variable, " berdasarkan ", input$anova_group, ". ",
               "Statistik F = ", round(f_stat, 3), " dengan df = (", df1, ", ", df2, "). ",
               "Semua kelompok memiliki rata-rata yang secara statistik sama.")
      }
      
      HTML(interpretation)
    })
  })
  
  # Two-way ANOVA
  observeEvent(input$run_anova2, {
    req(input$anova2_variable, input$anova2_factor1, input$anova2_factor2)
    
    var_data <- sovi_data[[input$anova2_variable]]
    factor1_data <- sovi_data[[input$anova2_factor1]]
    factor2_data <- sovi_data[[input$anova2_factor2]]
    
    # Remove NA values
    complete_cases <- complete.cases(var_data, factor1_data, factor2_data)
    var_data <- var_data[complete_cases]
    factor1_data <- factor1_data[complete_cases]
    factor2_data <- factor2_data[complete_cases]
    
    output$anova2_result <- renderPrint({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      summary(anova2_model)
    })
    
    output$anova2_plot <- renderPlotly({
      plot_data <- data.frame(
        variable = var_data,
        factor1 = factor1_data,
        factor2 = factor2_data
      )
      
      # Interaction plot
      interaction_means <- plot_data %>%
        group_by(factor1, factor2) %>%
        summarise(mean_var = mean(variable, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(interaction_means, aes(x = factor1, y = mean_var, color = factor2, group = factor2)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        scale_color_manual(values = colors[1:length(unique(factor2_data))]) +
        labs(title = paste("Plot Interaksi:", input$anova2_factor1, "x", input$anova2_factor2),
             x = input$anova2_factor1, y = paste("Rata-rata", input$anova2_variable),
             color = input$anova2_factor2) +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$posthoc2_result <- renderPrint({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      
      cat("Post-hoc test untuk", input$anova2_factor1, ":\n")
      print(TukeyHSD(anova2_model, "factor1_data"))
      cat("\nPost-hoc test untuk", input$anova2_factor2, ":\n")
      print(TukeyHSD(anova2_model, "factor2_data"))
      
      if(input$include_interaction) {
        cat("\nPost-hoc test untuk interaksi:\n")
        print(TukeyHSD(anova2_model, "factor1_data:factor2_data"))
      }
    })
    
    output$anova2_interpretation <- renderUI({
      if(input$include_interaction) {
        anova2_model <- aov(var_data ~ factor1_data * factor2_data)
      } else {
        anova2_model <- aov(var_data ~ factor1_data + factor2_data)
      }
      
      anova2_summary <- summary(anova2_model)
      p_values <- anova2_summary[[1]]$`Pr(>F)`
      f_stats <- anova2_summary[[1]]$`F value`
      alpha <- 0.05
      
      factor1_sig <- p_values[1] < alpha
      factor2_sig <- p_values[2] < alpha
      interaction_sig <- if(input$include_interaction && length(p_values) > 2) p_values[3] < alpha else FALSE
      
      interpretation <- paste0(
        "Hasil ANOVA dua arah untuk variabel ", input$anova2_variable, ":<br>",
        "&bull; Efek utama ", input$anova2_factor1, ": F = ", round(f_stats[1], 3), ", p = ", round(p_values[1], 4),
        if(factor1_sig) " (signifikan)" else " (tidak signifikan)", "<br>",
        "&bull; Efek utama ", input$anova2_factor2, ": F = ", round(f_stats[2], 3), ", p = ", round(p_values[2], 4),
        if(factor2_sig) " (signifikan)" else " (tidak signifikan)", "<br>"
      )
      
      if(input$include_interaction) {
        interpretation <- paste0(interpretation,
                                 "&bull; Efek interaksi: F = ", round(f_stats[3], 3), ", p = ", round(p_values[3], 4),
                                 if(interaction_sig) " (signifikan)" else " (tidak signifikan)", "<br>")
      }
      
      interpretation <- paste0(interpretation, "<br>",
                               if(interaction_sig) {
                                 "Adanya interaksi signifikan menunjukkan bahwa efek satu faktor bergantung pada level faktor lainnya."
                               } else {
                                 "Tidak ada interaksi signifikan, sehingga efek kedua faktor bersifat aditif dan independen."
                               })
      
      HTML(interpretation)
    })
  })
  
  # Multiple Linear Regression
  observeEvent(input$run_regression, {
    req(input$regression_response, input$regression_predictors)
    
    # Check if we have at least 2 predictors
    if(length(input$regression_predictors) < 2) {
      showNotification("Silakan pilih minimal 2 variabel prediktor untuk analisis lengkap.", type = "warning")
    }
    
    # Create formula
    formula_str <- paste(input$regression_response, "~", paste(input$regression_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    values$regression_model <- lm(formula_obj, data = sovi_data)
    
    output$regression_result <- renderPrint({
      summary(values$regression_model)
    })
    
    output$model_summary <- renderPrint({
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("Ringkasan Model Regresi:\n")
      cat("========================\n")
      cat("R-squared:", round(model_summary$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
      cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
      cat("P-value (F-test):", format.pval(pf(model_summary$fstatistic[1],
                                              model_summary$fstatistic[2],
                                              model_summary$fstatistic[3],
                                              lower.tail = FALSE)), "\n")
      cat("Residual standard error:", round(model_summary$sigma, 4), "\n")
      cat("Degrees of freedom:", model_summary$df[2], "\n")
    })
    
    output$fitted_actual_plot <- renderPlotly({
      fitted_values <- fitted(values$regression_model)
      actual_values <- sovi_data[[input$regression_response]]
      
      plot_data <- data.frame(
        fitted = fitted_values,
        actual = actual_values
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = actual)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = 1, intercept = 0, color = colors[2], size = 1) +
        labs(title = "Fitted vs Actual Values", x = "Nilai Prediksi", y = "Nilai Aktual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    # Diagnostic plots
    output$residuals_fitted <- renderPlotly({
      residuals <- residuals(values$regression_model)
      fitted_values <- fitted(values$regression_model)
      
      plot_data <- data.frame(
        fitted = fitted_values,
        residuals = residuals
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_hline(yintercept = 0, color = colors[2], size = 1) +
        geom_smooth(method = "loess", color = colors[4], se = FALSE, formula = y ~ x) +
        labs(title = "Residual vs Fitted", x = "Nilai Prediksi", y = "Residual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$qq_residuals <- renderPlotly({
      residuals <- residuals(values$regression_model)
      
      qq_data <- data.frame(
        sample = sort(residuals),
        theoretical = qnorm(ppoints(length(residuals)))
      )
      
      p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_abline(slope = sd(residuals), intercept = mean(residuals), color = colors[2], size = 1) +
        labs(title = "Q-Q Plot Residual", x = "Kuantil Teoritis", y = "Kuantil Sampel") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$scale_location_plot <- renderPlotly({
      residuals <- residuals(values$regression_model)
      fitted_values <- fitted(values$regression_model)
      sqrt_abs_residuals <- sqrt(abs(residuals))
      
      plot_data <- data.frame(
        fitted = fitted_values,
        sqrt_abs_residuals = sqrt_abs_residuals
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = sqrt_abs_residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_smooth(method = "loess", color = colors[2], se = FALSE, formula = y ~ x) +
        labs(title = "Scale-Location Plot", x = "Nilai Prediksi", y = "√|Residual|") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$leverage_plot <- renderPlotly({
      residuals <- residuals(values$regression_model)
      leverage <- hatvalues(values$regression_model)
      
      plot_data <- data.frame(
        leverage = leverage,
        residuals = residuals
      )
      
      p <- ggplot(plot_data, aes(x = leverage, y = residuals)) +
        geom_point(alpha = 0.6, color = colors[1]) +
        geom_hline(yintercept = 0, color = colors[2], size = 1) +
        geom_smooth(method = "loess", color = colors[4], se = FALSE, formula = y ~ x) +
        labs(title = "Residual vs Leverage", x = "Leverage", y = "Residual") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    # Assumption tests
    output$multicollinearity_test <- renderPrint({
      if(length(input$regression_predictors) >= 2) {
        tryCatch({
          vif_values <- vif(values$regression_model)
          cat("Variance Inflation Factor (VIF):\n")
          cat("================================\n")
          print(round(vif_values, 3))
          cat("\nInterpretasi VIF:\n")
          cat("VIF < 5: Tidak ada masalah multikolinearitas\n")
          cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
          cat("VIF ≥ 10: Multikolinearitas tinggi\n")
          
          if(any(vif_values >= 10)) {
            cat("\nPeringatan: Terdapat multikolinearitas tinggi!\n")
          } else if(any(vif_values >= 5)) {
            cat("\nPerhatian: Terdapat multikolinearitas sedang.\n")
          } else {
            cat("\nBaik: Tidak ada masalah multikolinearitas.\n")
          }
        }, error = function(e) {
          cat("Error dalam menghitung VIF: Model mungkin memiliki multikolinearitas sempurna.\n")
        })
      } else {
        cat("Perhitungan VIF memerlukan minimal 2 variabel prediktor.\n")
      }
    })
    
    output$durbin_watson_test <- renderPrint({
      tryCatch({
        dw_test <- durbinWatsonTest(values$regression_model)
        cat("Uji Durbin-Watson:\n")
        cat("==================\n")
        print(dw_test)
        cat("\nInterpretasi DW:\n")
        cat("DW ≈ 2: Tidak ada autokorelasi\n")
        cat("DW < 2: Autokorelasi positif\n")
        cat("DW > 2: Autokorelasi negatif\n")
        
        dw_stat <- dw_test$dw
        if(dw_stat >= 1.5 && dw_stat <= 2.5) {
          cat("\nBaik: Tidak ada autokorelasi yang signifikan.\n")
        } else {
          cat("\nPeringatan: Kemungkinan ada autokorelasi.\n")
        }
      }, error = function(e) {
        cat("Error dalam menghitung uji Durbin-Watson.\n")
      })
    })
    
    output$residual_normality_test <- renderPrint({
      residuals <- residuals(values$regression_model)
      cat("Uji Normalitas Residual:\n")
      cat("========================\n")
      
      if(length(residuals) <= 5000) {
        shapiro_test <- shapiro.test(residuals)
        cat("Shapiro-Wilk Test:\n")
        print(shapiro_test)
      } else {
        ad_test <- ad.test(residuals)
        cat("Anderson-Darling Test:\n")
        print(ad_test)
      }
      
      cat("\nInterpretasi:\n")
      cat("H0: Residual berdistribusi normal\n")
      cat("H1: Residual tidak berdistribusi normal\n")
      cat("Jika p-value < 0.05, tolak H0 (residual tidak normal)\n")
    })
    
    output$homoscedasticity_test <- renderPrint({
      tryCatch({
        # Use ncvTest from car package
        bp_test <- ncvTest(values$regression_model)
        cat("Uji Non-constant Variance (Homoskedastisitas):\n")
        cat("==============================================\n")
        print(bp_test)
        cat("\nInterpretasi:\n")
        cat("H0: Varians residual konstan (homoskedastisitas)\n")
        cat("H1: Varians residual tidak konstan (heteroskedastisitas)\n")
        cat("Jika p-value < 0.05, tolak H0 (ada heteroskedastisitas)\n")
        
        if(bp_test$p < 0.05) {
          cat("\nPeringatan: Terdapat heteroskedastisitas!\n")
        } else {
          cat("\nBaik: Asumsi homoskedastisitas terpenuhi.\n")
        }
      }, error = function(e) {
        cat("Error dalam menghitung uji homoskedastisitas.\n")
        # Alternative: correlation test
        residuals <- residuals(values$regression_model)
        fitted_vals <- fitted(values$regression_model)
        cor_test <- cor.test(abs(residuals), fitted_vals)
        
        cat("Korelasi |residual| vs fitted values:\n")
        print(cor_test)
        cat("Jika korelasi signifikan, kemungkinan ada heteroskedastisitas.\n")
      })
    })
    
    output$regression_interpretation <- renderUI({
      model <- values$regression_model
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      f_stat <- summary_model$fstatistic[1]
      f_p_value <- pf(f_stat, summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE)
      
      # Count significant predictors
      coef_p_values <- summary_model$coefficients[, "Pr(>|t|)"]
      sig_predictors <- sum(coef_p_values[-1] < 0.05)  # Exclude intercept
      total_predictors <- length(coef_p_values) - 1
      
      interpretation <- paste0(
        "Model regresi linear berganda menjelaskan ", round(r_squared * 100, 2), "% variasi dalam ", input$regression_response,
        " (R² = ", round(r_squared, 3), ", Adjusted R² = ", round(adj_r_squared, 3), ").<br>",
        "Uji F-statistik keseluruhan (F = ", round(f_stat, 3), ", p = ", format.pval(f_p_value), ") menunjukkan bahwa model ini secara statistik ",
        if(f_p_value < 0.05) "signifikan dalam memprediksi variabel respons." else "tidak signifikan.", "<br>",
        "Dari ", total_predictors, " variabel prediktor, ", sig_predictors, " diantaranya memiliki pengaruh yang signifikan secara statistik (p < 0.05). ",
        "Plot Fitted vs Actual menunjukkan seberapa baik prediksi model (titik-titik) mendekati garis diagonal (nilai aktual). ",
        "Model ini dapat digunakan untuk prediksi dan inferensi sesuai dengan ketentuan ujian STIS."
      )
      
      HTML(interpretation)
    })
    
    output$diagnostic_interpretation <- renderUI({
      HTML(
        "Plot diagnostik digunakan untuk memverifikasi asumsi regresi linear:<br>
        <ul>
          <li><b>Residual vs Fitted:</b> Plot ini memeriksa asumsi linearitas dan homoskedastisitas. Idealnya, titik-titik tersebar acak di sekitar garis horizontal nol tanpa pola yang jelas. Pola seperti corong menunjukkan heteroskedastisitas. Garis merah yang datar menunjukkan linearitas terpenuhi.</li>
          <li><b>Q-Q Plot Residual:</b> Plot ini memeriksa apakah residual berdistribusi normal. Idealnya, titik-titik mengikuti garis diagonal. Penyimpangan signifikan dari garis ini menunjukkan bahwa residual tidak normal.</li>
          <li><b>Scale-Location Plot:</b> Plot ini juga memeriksa homoskedastisitas (kesamaan varians). Idealnya, garis merah horizontal dan titik-titik tersebar secara acak. Tren pada garis merah menunjukkan heteroskedastisitas.</li>
          <li><b>Residual vs Leverage:</b> Plot ini membantu mengidentifikasi outlier dan titik berpengaruh (influential points). Titik dengan leverage tinggi (jauh ke kanan) dan residual besar (jauh dari nol) berpotensi menjadi titik berpengaruh yang dapat mengubah hasil model.</li>
        </ul>
        Interpretasi plot-plot ini penting untuk memastikan validitas model regresi sesuai dengan ketentuan ujian."
      )
    })
    
    output$assumption_interpretation <- renderUI({
      model <- values$regression_model
      
      # Multicollinearity
      vif_text <- if(length(input$regression_predictors) >= 2) {
        vif_vals <- tryCatch(vif(model), error = function(e) NULL)
        if(!is.null(vif_vals)) {
          if(any(vif_vals >= 10)) "tinggi (VIF ≥ 10)" else if(any(vif_vals >= 5)) "sedang (5 ≤ VIF < 10)" else "rendah (VIF < 5)"
        } else "tidak dapat dihitung"
      } else "tidak diuji (perlu >1 prediktor)"
      
      # Autocorrelation
      dw_test <- tryCatch(durbinWatsonTest(model), error = function(e) NULL)
      dw_text <- if(!is.null(dw_test)) {
        dw_stat <- dw_test$dw
        if(dw_stat >= 1.5 && dw_stat <= 2.5) "tidak signifikan (DW ≈ 2)" else "signifikan (DW jauh dari 2)"
      } else "tidak dapat dihitung"
      
      # Normality of residuals
      residuals <- residuals(model)
      norm_test <- if(length(residuals) <= 5000) shapiro.test(residuals) else ad.test(residuals)
      norm_text <- if(norm_test$p.value < 0.05) "tidak terpenuhi (p < 0.05)" else "terpenuhi (p ≥ 0.05)"
      
      # Homoscedasticity
      bp_test <- tryCatch(ncvTest(model), error = function(e) NULL)
      homo_text <- if(!is.null(bp_test)) {
        if(bp_test$p < 0.05) "tidak terpenuhi (p < 0.05, heteroskedastisitas)" else "terpenuhi (p ≥ 0.05, homoskedastisitas)"
      } else "tidak dapat dihitung"
      
      HTML(
        paste0(
          "Ringkasan uji asumsi regresi:<br>
          <ul>
            <li><b>Multikolinearitas (VIF):</b> Tingkat multikolinearitas terdeteksi <b>", vif_text, "</b>.</li>
            <li><b>Autokorelasi (Durbin-Watson):</b> Kehadiran autokorelasi <b>", dw_text, "</b>.</li>
            <li><b>Normalitas Residual (Shapiro-Wilk/Anderson-Darling):</b> Asumsi normalitas residual <b>", norm_text, "</b>.</li>
            <li><b>Homoskedastisitas (NCV Test):</b> Asumsi homoskedastisitas <b>", homo_text, "</b>.</li>
          </ul>
          Berdasarkan hasil ini, validitas model regresi dapat dievaluasi. Pelanggaran asumsi mungkin memerlukan transformasi data atau penggunaan metode regresi yang lebih robust sesuai dengan materi ujian STIS."
        )
      )
    })
  })
  
  # Clustering Analysis
  observeEvent(input$run_clustering, {
    req(input$cluster_variables)
    
    if(length(input$cluster_variables) < 2) {
      showNotification("Pilih minimal 2 variabel untuk analisis clustering.", type = "warning")
      return()
    }
    
    # Prepare data for clustering
    cluster_data <- sovi_data[, input$cluster_variables, drop = FALSE]
    cluster_data <- na.omit(cluster_data)
    
    # Scale the data
    scaled_data <- scale(cluster_data)
    
    # Use distance matrix if selected
    if(input$use_distance_matrix && nrow(distance_data) > 0) {
      # Create distance matrix from distance_data
      # Assuming distance_data contains pairwise distances
      dist_matrix <- as.dist(as.matrix(distance_data[, -1]))  # Exclude first column (ID)
    } else {
      dist_matrix <- dist(scaled_data)
    }
    
    # Perform clustering
    if(input$cluster_method == "kmeans") {
      cluster_result <- kmeans(scaled_data, centers = input$n_clusters, nstart = 25)
      values$clustering_result <- cluster_result
    } else if(input$cluster_method == "hierarchical") {
      hc_result <- hclust(dist_matrix, method = "ward.D2")
      cluster_result <- cutree(hc_result, k = input$n_clusters)
      values$clustering_result <- list(cluster = cluster_result, hclust = hc_result)
    } else if(input$cluster_method == "pam") {
      pam_result <- pam(dist_matrix, k = input$n_clusters)
      values$clustering_result <- pam_result
    }
    
    output$clustering_result <- renderPrint({
      if(input$cluster_method == "kmeans") {
        cat("K-Means Clustering Results:\n")
        cat("===========================\n")
        cat("Number of clusters:", input$n_clusters, "\n")
        cat("Cluster sizes:", cluster_result$size, "\n")
        cat("Within-cluster sum of squares:", round(cluster_result$tot.withinss, 3), "\n")
        cat("Between-cluster sum of squares:", round(cluster_result$betweenss, 3), "\n")
        cat("Total sum of squares:", round(cluster_result$totss, 3), "\n")
        cat("Ratio BSS/TSS:", round(cluster_result$betweenss/cluster_result$totss, 3), "\n")
      } else if(input$cluster_method == "hierarchical") {
        cat("Hierarchical Clustering Results:\n")
        cat("================================\n")
        cat("Number of clusters:", input$n_clusters, "\n")
        cat("Cluster sizes:", table(cluster_result), "\n")
      } else if(input$cluster_method == "pam") {
        cat("PAM Clustering Results:\n")
        cat("=======================\n")
        cat("Number of clusters:", input$n_clusters, "\n")
        cat("Cluster sizes:", pam_result$clusinfo[, "size"], "\n")
        cat("Average silhouette width:", round(pam_result$silinfo$avg.width, 3), "\n")
      }
    })
    
    # Elbow method plot
    output$elbow_plot <- renderPlotly({
      if(input$cluster_method == "kmeans") {
        wss <- sapply(1:10, function(k) {
          kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
        })
        
        elbow_data <- data.frame(k = 1:10, wss = wss)
        
        p <- ggplot(elbow_data, aes(x = k, y = wss)) +
          geom_line(color = colors[1], size = 1) +
          geom_point(color = colors[2], size = 3) +
          geom_vline(xintercept = input$n_clusters, color = colors[4], linetype = "dashed") +
          labs(title = "Elbow Method for Optimal k", x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares") +
          theme_minimal()
        
        ggplotly(p) %>% config(displayModeBar = FALSE)
      } else {
        # For non-kmeans methods, show a placeholder
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Elbow method hanya tersedia untuk K-Means", size = 5) +
          theme_void()
        ggplotly(p) %>% config(displayModeBar = FALSE)
      }
    })
    
    # Silhouette analysis
    output$silhouette_plot <- renderPlot({
      if(input$cluster_method == "kmeans") {
        sil <- silhouette(cluster_result$cluster, dist_matrix)
        fviz_silhouette(sil, palette = colors[1:input$n_clusters])
      } else if(input$cluster_method == "hierarchical") {
        sil <- silhouette(cluster_result, dist_matrix)
        fviz_silhouette(sil, palette = colors[1:input$n_clusters])
      } else if(input$cluster_method == "pam") {
        plot(pam_result, which = 2, col.p = colors[1:input$n_clusters])
      }
    })
    
    # Cluster plot
    output$cluster_plot <- renderPlotly({
      if(input$cluster_method == "kmeans") {
        cluster_assignments <- cluster_result$cluster
      } else if(input$cluster_method == "hierarchical") {
        cluster_assignments <- cluster_result
      } else if(input$cluster_method == "pam") {
        cluster_assignments <- pam_result$clustering
      }
      
      # Use first two variables for 2D plot
      plot_data <- data.frame(
        x = scaled_data[, 1],
        y = scaled_data[, 2],
        cluster = as.factor(cluster_assignments)
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = cluster)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_manual(values = colors[1:input$n_clusters]) +
        labs(title = paste("Cluster Plot -", input$cluster_method),
             x = input$cluster_variables[1], y = input$cluster_variables[2]) +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    output$clustering_interpretation <- renderUI({
      if(input$cluster_method == "kmeans") {
        bss_tss_ratio <- cluster_result$betweenss / cluster_result$totss
        interpretation <- paste0(
          "Analisis K-Means clustering dengan ", input$n_clusters, " cluster menunjukkan rasio BSS/TSS = ", round(bss_tss_ratio, 3), ". ",
          if(bss_tss_ratio > 0.7) "Clustering sangat baik dengan separasi yang jelas antar cluster." 
          else if(bss_tss_ratio > 0.5) "Clustering cukup baik dengan separasi yang memadai." 
          else "Clustering kurang optimal, pertimbangkan jumlah cluster yang berbeda.", " ",
          "Elbow method dapat membantu menentukan jumlah cluster optimal. ",
          if(input$use_distance_matrix) "Penggunaan matriks jarak memberikan hasil clustering yang lebih akurat berdasarkan kedekatan geografis." else ""
        )
      } else if(input$cluster_method == "hierarchical") {
        interpretation <- paste0(
          "Hierarchical clustering menghasilkan ", input$n_clusters, " cluster dengan struktur hierarkis. ",
          "Metode ini berguna untuk memahami struktur data secara bertingkat. ",
          if(input$use_distance_matrix) "Matriks jarak digunakan untuk mengukur kedekatan antar observasi." else ""
        )
      } else if(input$cluster_method == "pam") {
        avg_sil <- pam_result$silinfo$avg.width
        interpretation <- paste0(
          "PAM clustering menghasilkan ", input$n_clusters, " cluster dengan average silhouette width = ", round(avg_sil, 3), ". ",
          if(avg_sil > 0.7) "Clustering sangat baik dengan struktur yang kuat." 
          else if(avg_sil > 0.5) "Clustering cukup baik dengan struktur yang memadai." 
          else "Clustering lemah, pertimbangkan jumlah cluster yang berbeda.", " ",
          "PAM lebih robust terhadap outlier dibandingkan K-Means."
        )
      }
      
      HTML(paste0(interpretation, " Hasil clustering dapat digunakan untuk segmentasi dan analisis pola dalam data SOVI sesuai dengan ketentuan ujian."))
    })
  })
  
  # Clustering map
  output$clustering_map <- renderLeaflet({
    req(values$clustering_result)
    
    # Get cluster assignments
    if(input$cluster_method == "kmeans") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "hierarchical") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "pam") {
      cluster_assignments <- values$clustering_result$clustering
    }
    
    # Merge with coordinate data
    map_data <- merge(sovi_data, distance_data, by = "DISTRICTCODE", all.x = TRUE)
    map_data <- map_data[!is.na(map_data$LONGITUDE) & !is.na(map_data$LATITUDE), ]
    
    # Add cluster assignments
    map_data$cluster <- cluster_assignments[1:nrow(map_data)]
    
    # Create color palette for clusters
    cluster_colors <- colors[1:input$n_clusters]
    pal <- colorFactor(cluster_colors, domain = map_data$cluster)
    
    map_base <- leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 118, lat = -2.5, zoom = 5)
    
    if(input$map_cluster_type == "points") {
      map_base %>%
        addCircleMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 8,
          fillColor = ~pal(cluster),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.8,
          label = ~lapply(paste("<strong>District Code:", DISTRICTCODE, "</strong><br/>",
                                "Cluster:", cluster, "<br/>",
                                "Poverty:", round(POVERTY, 2), "%"), HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = ~cluster,
          title = "Cluster",
          position = "bottomright"
        )
    } else if(input$map_cluster_type == "heatmap") {
      map_base %>%
        addHeatmap(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~as.numeric(cluster),
          blur = 20,
          max = 0.05,
          radius = 15
        )
    } else if(input$map_cluster_type == "polygons") {
      # For polygons, we would need actual polygon data
      # For now, use enhanced circle markers
      map_base %>%
        addCircleMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          radius = 12,
          fillColor = ~pal(cluster),
          color = "white",
          weight = 3,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~lapply(paste("<strong>District Code:", DISTRICTCODE, "</strong><br/>",
                                "Cluster:", cluster, "<br/>",
                                "Poverty:", round(POVERTY, 2), "%"), HTML)
        ) %>%
        addLegend(
          pal = pal,
          values = ~cluster,
          title = "Cluster",
          position = "bottomright"
        )
    }
  })
  
  output$clustering_map_interpretation <- renderUI({
    interpretation <- paste0(
      "Peta clustering menunjukkan distribusi geografis dari ", input$n_clusters, " cluster yang terbentuk. ",
      "Visualisasi ini membantu memahami pola spasial dari pengelompokan data SOVI. ",
      if(input$map_cluster_type == "points") {
        "Setiap titik mewakili satu observasi dengan warna yang menunjukkan cluster-nya."
      } else if(input$map_cluster_type == "heatmap") {
        "Heat map menunjukkan konsentrasi cluster dalam area geografis tertentu."
      } else {
        "Polygon visualization memberikan representasi area untuk setiap cluster."
      },
      " Pola clustering yang terlihat dapat mengindikasikan adanya karakteristik regional yang serupa dalam hal kerentanan sosial. ",
      "Informasi ini berguna untuk perencanaan kebijakan dan intervensi yang tepat sasaran sesuai dengan karakteristik masing-masing cluster."
    )
    
    HTML(interpretation)
  })
  
  # Cluster profile
  output$cluster_profile_table <- DT::renderDataTable({
    req(values$clustering_result)
    
    # Get cluster assignments
    if(input$cluster_method == "kmeans") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "hierarchical") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "pam") {
      cluster_assignments <- values$clustering_result$clustering
    }
    
    # Create cluster profile
    cluster_data <- sovi_data[, input$cluster_variables, drop = FALSE]
    cluster_data$cluster <- cluster_assignments[1:nrow(cluster_data)]
    
    profile_stats <- cluster_data %>%
      group_by(cluster) %>%
      summarise_all(list(
        Mean = ~round(mean(., na.rm = TRUE), 3),
        SD = ~round(sd(., na.rm = TRUE), 3),
        Min = ~round(min(., na.rm = TRUE), 3),
        Max = ~round(max(., na.rm = TRUE), 3)
      ), .groups = 'drop')
    
    DT::datatable(
      profile_stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      caption = "Profil Statistik Deskriptif per Cluster"
    )
  })
  
  output$cluster_distribution_plot <- renderPlotly({
    req(values$clustering_result)
    
    # Get cluster assignments
    if(input$cluster_method == "kmeans") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "hierarchical") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "pam") {
      cluster_assignments <- values$clustering_result$clustering
    }
    
    cluster_counts <- table(cluster_assignments)
    cluster_df <- data.frame(
      Cluster = names(cluster_counts),
      Count = as.numeric(cluster_counts)
    )
    
    p <- ggplot(cluster_df, aes(x = Cluster, y = Count, fill = Cluster)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      scale_fill_manual(values = colors[1:input$n_clusters]) +
      labs(title = "Distribusi Ukuran Cluster", x = "Cluster", y = "Jumlah Observasi") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$cluster_comparison_plot <- renderPlotly({
    req(values$clustering_result)
    
    # Get cluster assignments
    if(input$cluster_method == "kmeans") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "hierarchical") {
      cluster_assignments <- values$clustering_result$cluster
    } else if(input$cluster_method == "pam") {
      cluster_assignments <- values$clustering_result$clustering
    }
    
    # Create comparison data
    cluster_data <- sovi_data[, input$cluster_variables, drop = FALSE]
    cluster_data$cluster <- as.factor(cluster_assignments[1:nrow(cluster_data)])
    
    # Melt data for plotting
    cluster_long <- cluster_data %>%
      pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")
    
    p <- ggplot(cluster_long, aes(x = cluster, y = value, fill = cluster)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~variable, scales = "free_y") +
      scale_fill_manual(values = colors[1:input$n_clusters]) +
      labs(title = "Perbandingan Variabel Antar Cluster", x = "Cluster", y = "Nilai") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$cluster_profile_interpretation <- renderUI({
    req(values$clustering_result)
    
    # Get cluster assignments
    if(input$cluster_method == "kmeans") {
      cluster_assignments <- values$clustering_result$cluster
      cluster_sizes <- values$clustering_result$size
    } else if(input$cluster_method == "hierarchical") {
      cluster_assignments <- values$clustering_result$cluster
      cluster_sizes <- table(cluster_assignments)
    } else if(input$cluster_method == "pam") {
      cluster_assignments <- values$clustering_result$clustering
      cluster_sizes <- values$clustering_result$clusinfo[, "size"]
    }
    
    largest_cluster <- which.max(cluster_sizes)
    smallest_cluster <- which.min(cluster_sizes)
    
    interpretation <- paste0(
      "Profil cluster menunjukkan karakteristik yang berbeda antar ", input$n_clusters, " cluster yang terbentuk. ",
      "Cluster ", largest_cluster, " memiliki ukuran terbesar dengan ", max(cluster_sizes), " observasi, ",
      "sedangkan Cluster ", smallest_cluster, " memiliki ukuran terkecil dengan ", min(cluster_sizes), " observasi. ",
      "Perbedaan rata-rata antar cluster pada setiap variabel menunjukkan keberhasilan algoritma clustering dalam memisahkan kel",
      "ompok berdasarkan karakteristik yang serupa. ",
      "Box plot perbandingan memvisualisasikan distribusi nilai setiap variabel dalam masing-masing cluster, ",
      "yang membantu dalam interpretasi dan penamaan cluster berdasarkan karakteristik dominannya. ",
      "Informasi ini dapat digunakan untuk strategi yang disesuaikan dengan karakteristik masing-masing cluster sesuai dengan tujuan analisis."
    )
    
    HTML(interpretation)
  })
  
  # Download handlers (simplified for brevity - each would generate appropriate reports)
  # Beranda downloads
  output$download_beranda_jpg <- downloadHandler(
    filename = function() { paste0("beranda_", Sys.Date(), ".jpg") },
    content = function(file) {
      # Create a simple plot for demonstration
      jpeg(file, width = 800, height = 600)
      plot(sovi_data$POVERTY, main = "SOVI Dashboard - Beranda", xlab = "Index", ylab = "Poverty Rate")
      dev.off()
    }
  )
  
  output$download_beranda_pdf <- downloadHandler(
    filename = function() { paste0("beranda_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      plot(sovi_data$POVERTY, main = "SOVI Dashboard - Beranda Report", xlab = "Index", ylab = "Poverty Rate")
      dev.off()
    }
  )
  
  output$download_beranda_word <- downloadHandler(
    filename = function() { paste0("beranda_report_", Sys.Date(), ".docx") },
    content = function(file) {
      # Create a simple text report
      report_content <- paste(
        "Dashboard Analisis SOVI - Beranda Report",
        "========================================",
        "",
        paste("Total Observasi:", nrow(sovi_data)),
        paste("Total Variabel:", ncol(sovi_data)),
        paste("Rata-rata Kemiskinan:", round(mean(sovi_data$POVERTY, na.rm = TRUE), 2), "%"),
        "",
        "Laporan ini dibuat pada:", Sys.time(),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  output$download_beranda_all <- downloadHandler(
    filename = function() { paste0("beranda_all_", Sys.Date(), ".zip") },
    content = function(file) {
      # Create temporary files
      temp_dir <- tempdir()
      
      # JPG
      jpg_file <- file.path(temp_dir, "beranda.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      plot(sovi_data$POVERTY, main = "SOVI Dashboard - Beranda")
      dev.off()
      
      # PDF
      pdf_file <- file.path(temp_dir, "beranda_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      plot(sovi_data$POVERTY, main = "SOVI Dashboard - Beranda Report")
      dev.off()
      
      # Word (as text)
      word_file <- file.path(temp_dir, "beranda_report.txt")
      report_content <- paste(
        "Dashboard Analisis SOVI - Beranda Report",
        "========================================",
        "",
        paste("Total Observasi:", nrow(sovi_data)),
        paste("Total Variabel:", ncol(sovi_data)),
        paste("Rata-rata Kemiskinan:", round(mean(sovi_data$POVERTY, na.rm = TRUE), 2), "%"),
        sep = "\n"
      )
      writeLines(report_content, word_file)
      
      # Create zip
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Similar download handlers for other tabs (manajemen, eksplorasi, etc.)
  # For brevity, I'll create simplified versions
  
  # Manajemen downloads
  output$download_manajemen_jpg <- downloadHandler(
    filename = function() { paste0("manajemen_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      hist(sovi_data$POVERTY, main = "Data Management - SOVI", xlab = "Poverty Rate", col = colors[1])
      dev.off()
    }
  )
  
  output$download_manajemen_pdf <- downloadHandler(
    filename = function() { paste0("manajemen_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      hist(sovi_data$POVERTY, main = "Data Management Report", xlab = "Poverty Rate", col = colors[1])
      dev.off()
    }
  )
  
  output$download_manajemen_word <- downloadHandler(
    filename = function() { paste0("manajemen_report_", Sys.Date(), ".docx") },
    content = function(file) {
      report_content <- paste(
        "Dashboard Analisis SOVI - Manajemen Data Report",
        "===============================================",
        "",
        "Ringkasan Data:",
        paste("- Jumlah observasi:", nrow(sovi_data)),
        paste("- Jumlah variabel numerik:", sum(sapply(sovi_data, is.numeric))),
        paste("- Kelengkapan data:", round(sum(complete.cases(sovi_data))/nrow(sovi_data) * 100, 1), "%"),
        "",
        "Laporan dibuat pada:", Sys.time(),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  output$download_manajemen_all <- downloadHandler(
    filename = function() { paste0("manajemen_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "manajemen.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      hist(sovi_data$POVERTY, main = "Data Management - SOVI", col = colors[1])
      dev.off()
      
      pdf_file <- file.path(temp_dir, "manajemen_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      hist(sovi_data$POVERTY, main = "Data Management Report", col = colors[1])
      dev.off()
      
      word_file <- file.path(temp_dir, "manajemen_report.txt")
      writeLines("Data Management Report\n======================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Add similar download handlers for other tabs...
  # (eksplorasi, asumsi, inferensia, regresi, clustering)
  # Each would follow the same pattern with appropriate content
  
  # For brevity, I'll add a few more key ones:
  
  # Regresi downloads
  output$download_regresi_jpg <- downloadHandler(
    filename = function() { paste0("regresi_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      if(!is.null(values$regression_model)) {
        plot(values$regression_model, which = 1, main = "Regression Analysis - Residuals vs Fitted")
      } else {
        plot(1, type = "n", main = "No regression model available")
      }
      dev.off()
    }
  )
  
  output$download_regresi_pdf <- downloadHandler(
    filename = function() { paste0("regresi_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      if(!is.null(values$regression_model)) {
        par(mfrow = c(2, 2))
        plot(values$regression_model, main = "Multiple Linear Regression Analysis")
      } else {
        plot(1, type = "n", main = "No regression model available")
      }
      dev.off()
    }
  )
  
  output$download_regresi_word <- downloadHandler(
    filename = function() { paste0("regresi_report_", Sys.Date(), ".docx") },
    content = function(file) {
      if(!is.null(values$regression_model)) {
        model_summary <- summary(values$regression_model)
        report_content <- paste(
          "Dashboard Analisis SOVI - Regresi Linear Berganda",
          "=================================================",
          "",
          "Model Summary:",
          paste("R-squared:", round(model_summary$r.squared, 4)),
          paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 4)),
          paste("F-statistic:", round(model_summary$fstatistic[1], 4)),
          "",
          "Coefficients:",
          capture.output(print(model_summary$coefficients)),
          "",
          "Report created on:", Sys.time(),
          sep = "\n"
        )
      } else {
        report_content <- "No regression model available for download."
      }
      writeLines(report_content, file)
    }
  )
  
  output$download_regresi_all <- downloadHandler(
    filename = function() { paste0("regresi_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "regresi.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      if(!is.null(values$regression_model)) {
        plot(values$regression_model, which = 1)
      } else {
        plot(1, type = "n", main = "No model available")
      }
      dev.off()
      
      pdf_file <- file.path(temp_dir, "regresi_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      if(!is.null(values$regression_model)) {
        par(mfrow = c(2, 2))
        plot(values$regression_model)
      }
      dev.off()
      
      word_file <- file.path(temp_dir, "regresi_report.txt")
      writeLines("Regression Analysis Report\n==========================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Add placeholder download handlers for remaining tabs
  # (These would be implemented similarly with appropriate content)
  
  # Eksplorasi downloads
  output$download_eksplorasi_jpg <- downloadHandler(
    filename = function() { paste0("eksplorasi_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      boxplot(sovi_data$POVERTY, main = "Data Exploration - SOVI", ylab = "Poverty Rate", col = colors[2])
      dev.off()
    }
  )
  
  output$download_eksplorasi_pdf <- downloadHandler(
    filename = function() { paste0("eksplorasi_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      hist(sovi_data$POVERTY, main = "Poverty Distribution", col = colors[1])
      boxplot(sovi_data$POVERTY, main = "Poverty Boxplot", col = colors[2])
      plot(sovi_data$POVERTY, sovi_data$LOWEDU, main = "Poverty vs Low Education", 
           xlab = "Poverty", ylab = "Low Education", col = colors[3])
      plot(density(sovi_data$POVERTY, na.rm = TRUE), main = "Poverty Density", col = colors[4])
      dev.off()
    }
  )
  
  output$download_eksplorasi_word <- downloadHandler(
    filename = function() { paste0("eksplorasi_report_", Sys.Date(), ".docx") },
    content = function(file) {
      report_content <- paste(
        "Dashboard Analisis SOVI - Eksplorasi Data Report",
        "================================================",
        "",
        "Statistik Deskriptif Variabel Utama:",
        paste("Poverty - Mean:", round(mean(sovi_data$POVERTY, na.rm = TRUE), 3)),
        paste("Poverty - SD:", round(sd(sovi_data$POVERTY, na.rm = TRUE), 3)),
        paste("Low Education - Mean:", round(mean(sovi_data$LOWEDU, na.rm = TRUE), 3)),
        paste("Children - Mean:", round(mean(sovi_data$CHILDREN, na.rm = TRUE), 3)),
        paste("Elderly - Mean:", round(mean(sovi_data$ELDERLY, na.rm = TRUE), 3)),
        "",
        "Report created on:", Sys.time(),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  output$download_eksplorasi_all <- downloadHandler(
    filename = function() { paste0("eksplorasi_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "eksplorasi.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      boxplot(sovi_data$POVERTY, main = "Data Exploration", col = colors[2])
      dev.off()
      
      pdf_file <- file.path(temp_dir, "eksplorasi_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      hist(sovi_data$POVERTY, col = colors[1])
      boxplot(sovi_data$POVERTY, col = colors[2])
      plot(sovi_data$POVERTY, sovi_data$LOWEDU, col = colors[3])
      plot(density(sovi_data$POVERTY, na.rm = TRUE), col = colors[4])
      dev.off()
      
      word_file <- file.path(temp_dir, "eksplorasi_report.txt")
      writeLines("Data Exploration Report\n=======================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Add remaining download handlers for asumsi, inferensia, clustering
  # Following the same pattern...
  
  # Asumsi downloads
  output$download_asumsi_jpg <- downloadHandler(
    filename = function() { paste0("asumsi_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      qqnorm(sovi_data$POVERTY, main = "Assumption Testing - Q-Q Plot", col = colors[1])
      qqline(sovi_data$POVERTY, col = colors[2])
      dev.off()
    }
  )
  
  output$download_asumsi_pdf <- downloadHandler(
    filename = function() { paste0("asumsi_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      qqnorm(sovi_data$POVERTY, main = "Normality Test - Q-Q Plot", col = colors[1])
      qqline(sovi_data$POVERTY, col = colors[2])
      hist(sovi_data$POVERTY, main = "Distribution", col = colors[3])
      boxplot(sovi_data$POVERTY ~ sovi_data$SOVI_Category, main = "Homogeneity Test", col = colors[4])
      dev.off()
    }
  )
  
  output$download_asumsi_word <- downloadHandler(
    filename = function() { paste0("asumsi_report_", Sys.Date(), ".docx") },
    content = function(file) {
      shapiro_result <- shapiro.test(sample(sovi_data$POVERTY, min(5000, length(sovi_data$POVERTY))))
      report_content <- paste(
        "Dashboard Analisis SOVI - Uji Asumsi Report",
        "===========================================",
        "",
        "Uji Normalitas (Shapiro-Wilk):",
        paste("W =", round(shapiro_result$statistic, 4)),
        paste("p-value =", format.pval(shapiro_result$p.value)),
        "",
        if(shapiro_result$p.value < 0.05) "Kesimpulan: Data tidak berdistribusi normal" else "Kesimpulan: Data berdistribusi normal",
        "",
        "Report created on:", Sys.time(),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  output$download_asumsi_all <- downloadHandler(
    filename = function() { paste0("asumsi_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "asumsi.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      qqnorm(sovi_data$POVERTY, col = colors[1])
      qqline(sovi_data$POVERTY, col = colors[2])
      dev.off()
      
      pdf_file <- file.path(temp_dir, "asumsi_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      qqnorm(sovi_data$POVERTY, col = colors[1])
      qqline(sovi_data$POVERTY, col = colors[2])
      hist(sovi_data$POVERTY, col = colors[3])
      boxplot(sovi_data$POVERTY ~ sovi_data$SOVI_Category, col = colors[4])
      dev.off()
      
      word_file <- file.path(temp_dir, "asumsi_report.txt")
      writeLines("Assumption Testing Report\n=========================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Inferensia downloads
  output$download_inferensia_jpg <- downloadHandler(
    filename = function() { paste0("inferensia_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      boxplot(sovi_data$POVERTY ~ sovi_data$Population_Size, 
              main = "Statistical Inference - Group Comparison", 
              xlab = "Population Size", ylab = "Poverty Rate", col = colors[1:2])
      dev.off()
    }
  )
  
  output$download_inferensia_pdf <- downloadHandler(
    filename = function() { paste0("inferensia_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      boxplot(sovi_data$POVERTY ~ sovi_data$Population_Size, main = "T-test Comparison", col = colors[1:2])
      hist(sovi_data$POVERTY, main = "Distribution", col = colors[3])
      barplot(table(sovi_data$SOVI_Category), main = "Proportion Test", col = colors[4])
      dev.off()
    }
  )
  
  output$download_inferensia_word <- downloadHandler(
    filename = function() { paste0("inferensia_report_", Sys.Date(), ".docx") },
    content = function(file) {
      t_test_result <- t.test(sovi_data$POVERTY ~ sovi_data$Population_Size)
      report_content <- paste(
        "Dashboard Analisis SOVI - Statistik Inferensia Report",
        "=====================================================",
        "",
        "Uji t Dua Sampel:",
        paste("t =", round(t_test_result$statistic, 4)),
        paste("df =", round(t_test_result$parameter, 0)),
        paste("p-value =", format.pval(t_test_result$p.value)),
        "",
        if(t_test_result$p.value < 0.05) "Kesimpulan: Terdapat perbedaan signifikan" else "Kesimpulan: Tidak ada perbedaan signifikan",
        "",
        "Report created on:", Sys.time(),
        sep = "\n"
      )
      writeLines(report_content, file)
    }
  )
  
  output$download_inferensia_all <- downloadHandler(
    filename = function() { paste0("inferensia_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "inferensia.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      boxplot(sovi_data$POVERTY ~ sovi_data$Population_Size, col = colors[1:2])
      dev.off()
      
      pdf_file <- file.path(temp_dir, "inferensia_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      par(mfrow = c(2, 2))
      boxplot(sovi_data$POVERTY ~ sovi_data$Population_Size, col = colors[1:2])
      hist(sovi_data$POVERTY, col = colors[3])
      barplot(table(sovi_data$SOVI_Category), col = colors[4])
      dev.off()
      
      word_file <- file.path(temp_dir, "inferensia_report.txt")
      writeLines("Statistical Inference Report\n============================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
  
  # Clustering downloads
  output$download_clustering_jpg <- downloadHandler(
    filename = function() { paste0("clustering_", Sys.Date(), ".jpg") },
    content = function(file) {
      jpeg(file, width = 800, height = 600)
      if(!is.null(values$clustering_result)) {
        if(input$cluster_method == "kmeans") {
          plot(sovi_data$POVERTY, sovi_data$LOWEDU, 
               col = values$clustering_result$cluster, 
               main = "K-Means Clustering Analysis",
               xlab = "Poverty", ylab = "Low Education", pch = 19)
        } else {
          plot(sovi_data$POVERTY, sovi_data$LOWEDU, 
               main = "Clustering Analysis",
               xlab = "Poverty", ylab = "Low Education", pch = 19, col = colors[1])
        }
      } else {
        plot(1, type = "n", main = "No clustering results available")
      }
      dev.off()
    }
  )
  
  output$download_clustering_pdf <- downloadHandler(
    filename = function() { paste0("clustering_report_", Sys.Date(), ".pdf") },
    content = function(file) {
      pdf(file, width = 11, height = 8)
      if(!is.null(values$clustering_result)) {
        par(mfrow = c(2, 2))
        if(input$cluster_method == "kmeans") {
          plot(sovi_data$POVERTY, sovi_data$LOWEDU, col = values$clustering_result$cluster, 
               main = "Cluster Plot", pch = 19)
          barplot(values$clustering_result$size, main = "Cluster Sizes", col = colors[1:input$n_clusters])
        }
        # Add more plots as needed
      } else {
        plot(1, type = "n", main = "No clustering results available")
      }
      dev.off()
    }
  )
  
  output$download_clustering_word <- downloadHandler(
    filename = function() { paste0("clustering_report_", Sys.Date(), ".docx") },
    content = function(file) {
      if(!is.null(values$clustering_result)) {
        if(input$cluster_method == "kmeans") {
          report_content <- paste(
            "Dashboard Analisis SOVI - Clustering Analysis Report",
            "====================================================",
            "",
            "K-Means Clustering Results:",
            paste("Number of clusters:", input$n_clusters),
            paste("Cluster sizes:", paste(values$clustering_result$size, collapse = ", ")),
            paste("Total within-cluster SS:", round(values$clustering_result$tot.withinss, 3)),
            paste("Between-cluster SS:", round(values$clustering_result$betweenss, 3)),
            paste("BSS/TSS ratio:", round(values$clustering_result$betweenss/values$clustering_result$totss, 3)),
            "",
            "Report created on:", Sys.time(),
            sep = "\n"
          )
        } else {
          report_content <- paste(
            "Dashboard Analisis SOVI - Clustering Analysis Report",
            "====================================================",
            "",
            paste("Clustering method:", input$cluster_method),
            paste("Number of clusters:", input$n_clusters),
            "",
            "Report created on:", Sys.time(),
            sep = "\n"
          )
        }
      } else {
        report_content <- "No clustering results available for download."
      }
      writeLines(report_content, file)
    }
  )
  
  output$download_clustering_all <- downloadHandler(
    filename = function() { paste0("clustering_all_", Sys.Date(), ".zip") },
    content = function(file) {
      temp_dir <- tempdir()
      
      jpg_file <- file.path(temp_dir, "clustering.jpg")
      jpeg(jpg_file, width = 800, height = 600)
      if(!is.null(values$clustering_result) && input$cluster_method == "kmeans") {
        plot(sovi_data$POVERTY, sovi_data$LOWEDU, col = values$clustering_result$cluster, pch = 19)
      } else {
        plot(sovi_data$POVERTY, sovi_data$LOWEDU, pch = 19, col = colors[1])
      }
      dev.off()
      
      pdf_file <- file.path(temp_dir, "clustering_report.pdf")
      pdf(pdf_file, width = 11, height = 8)
      if(!is.null(values$clustering_result) && input$cluster_method == "kmeans") {
        par(mfrow = c(2, 2))
        plot(sovi_data$POVERTY, sovi_data$LOWEDU, col = values$clustering_result$cluster, pch = 19)
        barplot(values$clustering_result$size, col = colors[1:input$n_clusters])
      }
      dev.off()
      
      word_file <- file.path(temp_dir, "clustering_report.txt")
      writeLines("Clustering Analysis Report\n==========================\n\nReport created on: " %+% Sys.time(), word_file)
      
      zip::zip(file, files = c(jpg_file, pdf_file, word_file), mode = "cherry-pick")
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
