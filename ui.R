# =====================================================
# UI COMPONENTS - DASHBOARD ANALISIS SOVI
# =====================================================

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

# Professional color palette
colors <- c("#2C3E50", "#34495E", "#ECF0F1", "#3498DB", "#5DADE2", "#85C1E9", "#AED6F1")

# Custom CSS
custom_css <- paste0("
.content-wrapper, .right-side {
  background-color: #F8F9FA;
}
.main-header .navbar {
  background-color: #2C3E50 !important;
}
.main-header .logo {
  background-color: #2C3E50 !important;
}
.sidebar {
  background-color: #34495E !important;
}
.box {
  border-radius: 8px !important;
  box-shadow: 0 4px 12px rgba(44, 62, 80, 0.1) !important;
  border-top: 3px solid #3498DB !important;
}
.btn-primary {
  background-color: #3498DB !important;
  border-color: #3498DB !important;
}
.nav-tabs-custom > .nav-tabs > li.active {
  border-top-color: #3498DB !important;
}
.info-box {
  background: linear-gradient(135deg, #3498DB 0%, #5DADE2 100%);
  color: white;
  border-radius: 8px;
  padding: 20px;
  margin-bottom: 20px;
}
.metric-card {
  background: white;
  border-radius: 8px;
  padding: 20px;
  text-align: center;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  border-left: 4px solid #3498DB;
}
")

# UI
ui <- dashboardPage(
  title = "Dashboard Analisis SOVI - STIS UAS 2025",
  skin = "blue",
  
  dashboardHeader(
    title = "Dashboard Analisis SOVI - STIS UAS 2025",
    titleWidth = 450
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("Uji Rata-rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Varians", tabName = "uji_proporsi"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("line-chart"))
    ),
    
    # Download section in sidebar
    div(
      style = "background: rgba(255,255,255,0.1); margin: 10px; padding: 15px; border-radius: 8px;",
      conditionalPanel(
        condition = "input.sidebar == 'beranda'",
        h5("Download Beranda", style = "color: white;"),
        downloadButton("download_beranda_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_beranda_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'manajemen'",
        h5("Download Manajemen", style = "color: white;"),
        downloadButton("download_manajemen_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_manajemen_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'eksplorasi'",
        h5("Download Eksplorasi", style = "color: white;"),
        downloadButton("download_eksplorasi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_eksplorasi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'asumsi'",
        h5("Download Uji Asumsi", style = "color: white;"),
        downloadButton("download_asumsi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_asumsi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'uji_rata' || input.sidebar == 'uji_proporsi' || input.sidebar == 'anova'",
        h5("Download Inferensia", style = "color: white;"),
        downloadButton("download_inferensia_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_inferensia_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'regresi'",
        h5("Download Regresi", style = "color: white;"),
        downloadButton("download_regresi_jpg", "JPG", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_pdf", "PDF", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_word", "Word", class = "btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
        downloadButton("download_regresi_all", "Semua", class = "btn-primary btn-sm", style = "width: 100%;")
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
                   class = "info-box",
                   h1("Dashboard Analisis SOVI - STIS UAS 2025", style = "margin: 0; font-weight: 600;"),
                   p("Social Vulnerability Index Analysis - Ujian Akhir Semester Komputasi Statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        # Metadata Dashboard
        fluidRow(
          column(12,
                 box(
                   title = "Metadata Dashboard", status = "primary", solidHeader = TRUE, width = NULL,
                   div(
                     style = "padding: 15px;",
                     h4("Informasi Dataset SOVI", style = "color: #2C3E50; margin-bottom: 15px;"),
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
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("total_observations")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Total Observasi")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("total_variables")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Total Variabel")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("avg_poverty_rate")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Rata-rata Kemiskinan (%)")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("data_completeness")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Kelengkapan Data")
                 )
          )
        ),
        
        # Additional metrics
        fluidRow(
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("avg_education_rate")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Rata-rata Pendidikan Rendah (%)")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("avg_children_rate")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Rata-rata Anak-anak (%)")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("avg_elderly_rate")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Rata-rata Lansia (%)")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2.5em; font-weight: 700; color: #3498DB;", textOutput("correlation_strength")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Korelasi Terkuat")
                 )
          )
        ),
        
        # Main content
        fluidRow(
          column(6,
                 box(
                   title = "Distribusi Tingkat Kemiskinan", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("poverty_distribution", height = "350px")
                 )
          ),
          column(6,
                 box(
                   title = "Distribusi Pendidikan Rendah", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("education_distribution", height = "350px")
                 )
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Distribusi Anak-anak", status = "primary", solidHeader = TRUE, width = NULL,
                   plotlyOutput("children_distribution", height = "350px")
                 )
          ),
          column(6,
                 box(
                   title = "Statistik Ringkasan Utama", status = "info", solidHeader = TRUE, width = NULL,
                   verbatimTextOutput("summary_stats")
                 )
          )
        ),
        
        # Peta Distribusi
        fluidRow(
          column(12,
                 box(
                   title = "Peta Distribusi SOVI", status = "primary", solidHeader = TRUE, width = NULL,
                   leafletOutput("beranda_map", height = "400px")
                 )
          )
        ),
        
        # Interpretasi
        fluidRow(
          column(12,
                 div(
                   style = "background: #ECF0F1; padding: 15px; border-radius: 8px; border-left: 4px solid #3498DB;",
                   h4("Interpretasi Dashboard Beranda", style = "color: #2C3E50;"),
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
                   class = "info-box",
                   h1("Manajemen Data", style = "margin: 0; font-weight: 600;"),
                   p("Kelola dan transformasi data SOVI untuk analisis optimal", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        # Data Quality Overview
        fluidRow(
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2em; font-weight: 700; color: #3498DB;", textOutput("missing_values_count")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Missing Values")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2em; font-weight: 700; color: #3498DB;", textOutput("numeric_vars_count")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Variabel Numerik")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2em; font-weight: 700; color: #3498DB;", textOutput("categorical_vars_count")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Variabel Kategorik")
                 )
          ),
          column(3,
                 div(
                   class = "metric-card",
                   div(style = "font-size: 2em; font-weight: 700; color: #3498DB;", textOutput("outliers_count")),
                   div(style = "font-size: 1em; margin-top: 8px; color: #2C3E50;", "Outliers Detected")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Alat Manajemen Data", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Ringkasan Data",
                              br(),
                              fluidRow(
                                column(6,
                                       h4("Ringkasan Dataset", style = "color: #2C3E50;"),
                                       div(
                                         style = "background: white; padding: 15px; border-radius: 8px; border: 1px solid #BDC3C7;",
                                         verbatimTextOutput("data_summary")
                                       )
                                ),
                                column(6,
                                       h4("Struktur Data", style = "color: #2C3E50;"),
                                       div(
                                         style = "background: white; padding: 15px; border-radius: 8px; border: 1px solid #BDC3C7;",
                                         verbatimTextOutput("data_structure")
                                       )
                                )
                              ),
                              
                              br(),
                              fluidRow(
                                column(6,
                                       h4("Korelasi Antar Variabel", style = "color: #2C3E50;"),
                                       plotOutput("correlation_heatmap", height = "300px")
                                ),
                                column(6,
                                       h4("Distribusi Missing Values", style = "color: #2C3E50;"),
                                       plotlyOutput("missing_values_plot", height = "300px")
                                )
                              ),
                              
                              div(
                                style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                h5("Interpretasi Ringkasan Data", style = "color: #2C3E50;"),
                                uiOutput("data_summary_interpretation")
                              )
                     ),
                     
                     tabPanel("Kategorisasi Data",
                              br(),
                              div(
                                style = "background: white; border-radius: 8px; padding: 20px; border: 1px solid #BDC3C7;",
                                h4("Mengubah Data Kontinu menjadi Kategorik", style = "color: #2C3E50;"),
                                
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
                                           style = "background: #ECF0F1; padding: 15px; border-radius: 6px;",
                                           verbatimTextOutput("categorization_preview")
                                         )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.apply_categorization > 0",
                                  br(),
                                  h4("Hasil Kategorisasi", style = "color: #2C3E50;"),
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
                                    style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                    h5("Interpretasi Kategorisasi", style = "color: #2C3E50;"),
                                    uiOutput("categorization_interpretation")
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("Transformasi Data",
                              br(),
                              div(
                                style = "background: white; border-radius: 8px; padding: 20px; border: 1px solid #BDC3C7;",
                                h4("Transformasi Variabel Numerik", style = "color: #2C3E50;"),
                                
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
                                           style = "background: #ECF0F1; padding: 15px; border-radius: 6px;",
                                           verbatimTextOutput("transformation_preview")
                                         )
                                  )
                                ),
                                
                                conditionalPanel(
                                  condition = "input.apply_transformation > 0",
                                  br(),
                                  h4("Hasil Transformasi", style = "color: #2C3E50;"),
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
                                    style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                    h5("Interpretasi Transformasi", style = "color: #2C3E50;"),
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
      
      # Eksplorasi Data Tab (with Clustering moved here)
      tabItem(
        tabName = "eksplorasi",
        fluidRow(
          column(12,
                 div(
                   class = "info-box",
                   h1("Eksplorasi Data", style = "margin: 0; font-weight: 600;"),
                   p("Analisis deskriptif, visualisasi data, dan clustering SOVI", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Eksplorasi Data Interaktif", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Statistik Deskriptif",
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
                                         h4("Statistik Deskriptif", style = "color: #2C3E50;"),
                                         verbatimTextOutput("descriptive_stats")
                                  ),
                                  column(6,
                                         h4("Tabel Ringkasan", style = "color: #2C3E50;"),
                                         DT::dataTableOutput("descriptive_table")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Statistik Deskriptif", style = "color: #2C3E50;"),
                                  uiOutput("descriptive_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Visualisasi Data",
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput("plot_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(3,
                                       selectInput("plot_type", "Jenis Plot:",
                                                   choices = list(
                                                     "Histogram" = "histogram",
                                                     "Box Plot" = "boxplot",
                                                     "Density Plot" = "density",
                                                     "Violin Plot" = "violin"
                                                   ))
                                ),
                                column(3,
                                       selectInput("plot_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status"))
                                ),
                                column(3,
                                       br(),
                                       actionButton("run_visualization", "Update Plot",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              fluidRow(
                                column(8,
                                       plotlyOutput("visualization_plot", height = "450px")
                                ),
                                column(4,
                                       h4("Statistik Deskriptif", style = "color: #2C3E50;"),
                                       verbatimTextOutput("plot_stats")
                                )
                              ),
                              
                              div(
                                style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                h5("Interpretasi Visualisasi", style = "color: #2C3E50;"),
                                uiOutput("visualization_interpretation")
                              )
                     ),
                     
                     tabPanel("Peta Interaktif",
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
                                style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                h5("Interpretasi Peta", style = "color: #2C3E50;"),
                                uiOutput("map_interpretation")
                              )
                     ),
                     
                     tabPanel("Analisis Korelasi",
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
                                         h4("Matriks Korelasi", style = "color: #2C3E50;"),
                                         DT::dataTableOutput("correlation_table")
                                  ),
                                  column(6,
                                         h4("Uji Signifikansi", style = "color: #2C3E50;"),
                                         verbatimTextOutput("correlation_test")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Korelasi", style = "color: #2C3E50;"),
                                  uiOutput("correlation_interpretation")
                                )
                              )
                     ),
                     
                     # Clustering Analysis moved here
                     tabPanel("Analisis Clustering",
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
                                         h4("Hasil Clustering", style = "color: #2C3E50;"),
                                         verbatimTextOutput("clustering_result")
                                  ),
                                  column(6,
                                         h4("Elbow Method", style = "color: #2C3E50;"),
                                         plotlyOutput("elbow_plot", height = "300px")
                                  )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         h4("Silhouette Analysis", style = "color: #2C3E50;"),
                                         plotOutput("silhouette_plot", height = "300px")
                                  ),
                                  column(6,
                                         h4("Cluster Plot", style = "color: #2C3E50;"),
                                         plotlyOutput("cluster_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Peta Clustering", style = "color: #2C3E50;"),
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
                                         leafletOutput("clustering_map", height = "400px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Clustering", style = "color: #2C3E50;"),
                                  uiOutput("clustering_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Continue with other tabs (Uji Asumsi, Statistik Inferensia, Regresi)
      # For brevity, I'll include the key structure for remaining tabs
      
      # Uji Asumsi Tab
      tabItem(
        tabName = "asumsi",
        fluidRow(
          column(12,
                 div(
                   class = "info-box",
                   h1("Uji Asumsi Statistik", style = "margin: 0; font-weight: 600;"),
                   p("Verifikasi asumsi normalitas dan homogenitas untuk analisis statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Pengujian Asumsi Statistik", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji Normalitas",
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
                                       selectInput("normality_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status")),
                                       br(),
                                       actionButton("run_normality", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_normality > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Normalitas", style = "color: #2C3E50;"),
                                         verbatimTextOutput("normality_result")
                                  ),
                                  column(6,
                                         h4("Q-Q Plot", style = "color: #2C3E50;"),
                                         plotlyOutput("qq_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Histogram dengan Kurva Normal", style = "color: #2C3E50;"),
                                plotlyOutput("normality_histogram", height = "350px"),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji Normalitas", style = "color: #2C3E50;"),
                                  uiOutput("normality_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji Homogenitas",
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
                                         h4("Hasil Uji Homogenitas", style = "color: #2C3E50;"),
                                         verbatimTextOutput("homogeneity_result")
                                  ),
                                  column(6,
                                         h4("Box Plot berdasarkan Kelompok", style = "color: #2C3E50;"),
                                         plotlyOutput("homogeneity_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji Homogenitas", style = "color: #2C3E50;"),
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
                   class = "info-box",
                   h1("Uji Beda Rata-rata", style = "margin: 0; font-weight: 600;"),
                   p("Analisis perbedaan rata-rata menggunakan uji t", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Uji Beda Rata-rata", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji t Satu Sampel",
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput("onesample_variable", "Pilih Variabel:",
                                                   choices = NULL)
                                ),
                                column(3,
                                       numericInput("mu_hypothesis", "Nilai Hipotesis (μ₀):",
                                                    value = 0, step = 0.1)
                                ),
                                column(3,
                                       selectInput("alternative_hypothesis", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                ),
                                column(3,
                                       selectInput("onesample_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status")),
                                       br(),
                                       actionButton("run_onesample", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_onesample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Satu Sampel", style = "color: #2C3E50;"),
                                         verbatimTextOutput("onesample_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi", style = "color: #2C3E50;"),
                                         plotlyOutput("onesample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji t Satu Sampel", style = "color: #2C3E50;"),
                                  uiOutput("onesample_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji t Dua Sampel",
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
                                                   )),
                                       br(),
                                       actionButton("run_twosample", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_twosample > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji t Dua Sampel", style = "color: #2C3E50;"),
                                         verbatimTextOutput("twosample_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Kelompok", style = "color: #2C3E50;"),
                                         plotlyOutput("twosample_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji t Dua Sampel", style = "color: #2C3E50;"),
                                  uiOutput("twosample_interpretation")
                                )
                              )
                     )
                   )
                 )
          )
        )
      ),
      
      # Continue with other inference tabs and regression tab...
      # For brevity, I'll include the structure for the remaining key tabs
      
      # Uji Proporsi & Varians Tab
      tabItem(
        tabName = "uji_proporsi",
        fluidRow(
          column(12,
                 div(
                   class = "info-box",
                   h1("Uji Proporsi & Varians", style = "margin: 0; font-weight: 600;"),
                   p("Analisis proporsi dan varians dengan metode statistik", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Uji Proporsi & Varians", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Uji Proporsi",
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput("prop_variable", "Pilih Variabel Kategorik:",
                                                   choices = c("SOVI_Category", "Population_Size", "Economic_Status", "Age_Group", "Education_Level"))
                                ),
                                column(3,
                                       selectInput("prop_category", "Kategori yang Diuji:",
                                                   choices = NULL)
                                ),
                                column(3,
                                       numericInput("prop_hypothesis", "Proporsi Hipotesis:",
                                                    value = 0.5, min = 0, max = 1, step = 0.01),
                                       selectInput("prop_alternative", "Hipotesis Alternatif:",
                                                   choices = list(
                                                     "≠ (Dua arah)" = "two.sided",
                                                     "> (Lebih besar)" = "greater",
                                                     "< (Lebih kecil)" = "less"
                                                   ))
                                ),
                                column(3,
                                       selectInput("prop_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size")),
                                       br(),
                                       actionButton("run_prop_test", "Jalankan Uji",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_prop_test > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil Uji Proporsi", style = "color: #2C3E50;"),
                                         verbatimTextOutput("prop_test_result")
                                  ),
                                  column(6,
                                         h4("Visualisasi Proporsi", style = "color: #2C3E50;"),
                                         plotlyOutput("prop_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji Proporsi", style = "color: #2C3E50;"),
                                  uiOutput("prop_test_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Uji Varians",
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
                                         h4("Hasil Uji F", style = "color: #2C3E50;"),
                                         verbatimTextOutput("var_test_result")
                                  ),
                                  column(6,
                                         h4("Perbandingan Varians", style = "color: #2C3E50;"),
                                         plotlyOutput("var_test_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji Varians", style = "color: #2C3E50;"),
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
                   class = "info-box",
                   h1("Analysis of Variance (ANOVA)", style = "margin: 0; font-weight: 600;"),
                   p("Analisis varians untuk membandingkan rata-rata lebih dari dua kelompok", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Analisis ANOVA", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("ANOVA Satu Arah",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("anova_variable", "Pilih Variabel Dependen:",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("anova_group", "Pilih Variabel Pengelompokan:",
                                                   choices = c("SOVI_Category", "Economic_Status", "Education_Level"))
                                ),
                                column(4,
                                       selectInput("anova_additional_group", "Kelompokkan lebih lanjut:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Kelompok Usia" = "Age_Group")),
                                       br(),
                                       actionButton("run_anova", "Jalankan ANOVA",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_anova > 0",
                                fluidRow(
                                  column(6,
                                         h4("Hasil ANOVA", style = "color: #2C3E50;"),
                                         verbatimTextOutput("anova_result")
                                  ),
                                  column(6,
                                         h4("Plot Rata-rata Kelompok", style = "color: #2C3E50;"),
                                         plotlyOutput("anova_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc (Tukey HSD)", style = "color: #2C3E50;"),
                                verbatimTextOutput("posthoc_result"),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi ANOVA Satu Arah", style = "color: #2C3E50;"),
                                  uiOutput("anova_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("ANOVA Dua Arah",
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
                                         h4("Hasil ANOVA Dua Arah", style = "color: #2C3E50;"),
                                         verbatimTextOutput("anova2_result")
                                  ),
                                  column(6,
                                         h4("Plot Interaksi", style = "color: #2C3E50;"),
                                         plotlyOutput("anova2_plot", height = "300px")
                                  )
                                ),
                                
                                h4("Uji Post-hoc untuk Faktor Utama", style = "color: #2C3E50;"),
                                verbatimTextOutput("posthoc2_result"),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi ANOVA Dua Arah", style = "color: #2C3E50;"),
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
                   class = "info-box",
                   h1("Regresi Linear Berganda", style = "margin: 0; font-weight: 600;"),
                   p("Analisis regresi dengan diagnostik dan validasi asumsi", style = "margin: 10px 0 0 0; opacity: 0.9;")
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 box(
                   title = "Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = NULL,
                   
                   tabsetPanel(
                     tabPanel("Pembangunan Model",
                              br(),
                              fluidRow(
                                column(4,
                                       selectInput("regression_response", "Variabel Respons (Y):",
                                                   choices = NULL)
                                ),
                                column(4,
                                       selectInput("regression_predictors", "Variabel Prediktor (X):",
                                                   choices = NULL, multiple = TRUE)
                                ),
                                column(4,
                                       selectInput("regression_group", "Kelompokkan berdasarkan:",
                                                   choices = c("Tidak ada" = "none",
                                                               "Kategori SOVI" = "SOVI_Category",
                                                               "Ukuran Populasi" = "Population_Size",
                                                               "Status Ekonomi" = "Economic_Status")),
                                       br(),
                                       actionButton("run_regression", "Jalankan Regresi",
                                                    class = "btn-primary", style = "width: 100%;")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Hasil Regresi Linear Berganda", style = "color: #2C3E50;"),
                                verbatimTextOutput("regression_result"),
                                
                                fluidRow(
                                  column(6,
                                         h4("Ringkasan Model", style = "color: #2C3E50;"),
                                         verbatimTextOutput("model_summary")
                                  ),
                                  column(6,
                                         h4("Fitted vs Actual", style = "color: #2C3E50;"),
                                         plotlyOutput("fitted_actual_plot", height = "300px")
                                  )
                                ),
                                
                                div(
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Model", style = "color: #2C3E50;"),
                                  uiOutput("regression_interpretation")
                                )
                              )
                     ),
                     
                     tabPanel("Plot Diagnostik",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Plot Diagnostik Regresi", style = "color: #2C3E50; margin-bottom: 20px;"),
                                
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
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Plot Diagnostik", style = "color: #2C3E50;"),
                                  uiOutput("diagnostic_interpretation")
                                )
                              ),
                              
                              conditionalPanel(
                                condition = "input.run_regression == 0",
                                div(style = "text-align: center; padding: 50px; color: #999;",
                                    p("Silakan jalankan model regresi terlebih dahulu di tab 'Pembangunan Model'."))
                              )
                     ),
                     
                     tabPanel("Uji Asumsi",
                              br(),
                              conditionalPanel(
                                condition = "input.run_regression > 0",
                                h4("Uji Asumsi Regresi", style = "color: #2C3E50;"),
                                
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
                                  style = "background: #ECF0F1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                                  h5("Interpretasi Uji Asumsi", style = "color: #2C3E50;"),
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
      )
    )
  )
)
