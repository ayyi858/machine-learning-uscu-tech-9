library(shiny)
options(shiny.error = function() {
  err <- geterrmessage()
  trace <- paste(capture.output(traceback(2)), collapse="\n")
  cat(paste("ERROR:\n", err, "\nTRACE:\n", trace, "\n\n"), file="error_log.txt", append=TRUE)
})
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(cluster)
library(dbscan)
library(DT)
library(plotly)
library(factoextra)
library(shinycssloaders)
library(shinyWidgets)
library(scales)
library(tidyr)
library(httr)
library(jsonlite)
library(rmarkdown)
library(knitr)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ============================================================
# DATA KEPADATAN PENDUDUK BPS 2024
# Sumber: BPS Sulawesi Selatan 2024 + estimasi per kecamatan Makassar
# ============================================================
# ============================================================
# KEPADATAN PENDUDUK BPS 2024 (DATA TERBARU)
# ============================================================
DATA_KEPADATAN_BPS <- data.frame(
  wilayah    = c("Makassar","Parepare","Palopo","Takalar","Gowa","Jeneponto","Bantaeng","Bulukumba","Maros","Pangkep"),
  kepadatan  = c(8282,1795,716,571,449,526,525,387,287,404),
  stringsAsFactors = FALSE
)

# Kepadatan per kecamatan Makassar (estimasi berbasis luas wilayah BPS)
# Muat Data Kepadatan Penduduk dari CSV jika ada
if (file.exists("kepadatan penduduk.csv")) {
  df_pop <- read.csv("kepadatan penduduk.csv", sep=";", stringsAsFactors=FALSE)
  names(df_pop) <- c("kec", "pop")
  # Mapping untuk mempermudah join
  KEPADATAN_KEC <- setNames(as.numeric(df_pop$pop), df_pop$kec)
} else {
  KEPADATAN_KEC <- c(
    "Mariso"=32000, "Mamajang"=25000, "Tamalate"=9000, "Rappocini"=16000,
    "Makassar"=32000, "Ujung Pandang"=9000, "Wajo"=26000, "Bontoala"=25000,
    "Ujung Tanah"=12000, "Tallo"=12000, "Panakkukang"=11000, "Manggala"=6000,
    "Biringkanaya"=4000, "Tamalanrea"=3000, "Sangkarrang"=500
  )
}

# ============================================================
# LOAD DATA
# ============================================================
data_usaha <- read.csv("data_usaha_makassar_clean.csv", stringsAsFactors = FALSE)

# Rename columns safely to avoid duplicates
safe_rename <- function(df, old, new) {
  if (old %in% names(df)) {
    if (new %in% names(df)) df[[new]] <- NULL # Remove existing target to avoid duplicates
    names(df)[names(df) == old] <- new
  }
  df
}

data_usaha <- safe_rename(data_usaha, "Latitude", "latitude")
data_usaha <- safe_rename(data_usaha, "Longitude", "longitude")
data_usaha <- safe_rename(data_usaha, "kategori_usaha", "kategori_grup")

# Ensure all names are unique just in case
names(data_usaha) <- make.unique(names(data_usaha))

# Ensure essential columns exist
if (!"skor_potensi"    %in% names(data_usaha)) data_usaha$skor_potensi <- 0
if (!"label_kelayakan" %in% names(data_usaha)) data_usaha$label_kelayakan <- "Belum Diketahui"
if (!"popularitas"     %in% names(data_usaha)) data_usaha$popularitas     <- "Baru"
if (!"kepadatan"       %in% names(data_usaha)) data_usaha$kepadatan       <- 3000

# Calculate a basic skor_potensi if it's all zeros or missing
if (all(data_usaha$skor_potensi == 0, na.rm = TRUE)) {
  data_usaha$skor_potensi <- round(
    pmin(100, (ifelse(is.na(data_usaha$rating), 3, data_usaha$rating) * 15) + 
         (pmin(500, ifelse(is.na(data_usaha$jumlah_ulasan), 0, data_usaha$jumlah_ulasan)) / 20)), 
    1
  )
}

data_usaha$latitude      <- as.numeric(data_usaha$latitude)
data_usaha$longitude     <- as.numeric(data_usaha$longitude)
data_usaha$rating        <- as.numeric(data_usaha$rating)
data_usaha$jumlah_ulasan <- as.numeric(data_usaha$jumlah_ulasan)
data_usaha$skor_potensi  <- as.numeric(data_usaha$skor_potensi)
data_usaha$kepadatan     <- as.numeric(data_usaha$kepadatan)
data_usaha$foto_url      <- ifelse(is.na(data_usaha$foto_url)|data_usaha$foto_url=="", NA, data_usaha$foto_url)
data_usaha$telepon       <- ifelse(is.na(data_usaha$telepon) |data_usaha$telepon=="",  "-", data_usaha$telepon)

# Merge kepadatan from our table
data_usaha$kepadatan <- as.numeric(KEPADATAN_KEC[data_usaha$kecamatan])
data_usaha$kepadatan[is.na(data_usaha$kepadatan) | !is.finite(data_usaha$kepadatan)] <- 3000

# Final numeric guarantee
data_usaha$rating        <- suppressWarnings(as.numeric(data_usaha$rating))
data_usaha$jumlah_ulasan <- suppressWarnings(as.numeric(data_usaha$jumlah_ulasan))
data_usaha$skor_potensi  <- suppressWarnings(as.numeric(data_usaha$skor_potensi))
data_usaha$kepadatan     <- suppressWarnings(as.numeric(data_usaha$kepadatan))
data_usaha$latitude      <- suppressWarnings(as.numeric(data_usaha$latitude))
data_usaha$longitude     <- suppressWarnings(as.numeric(data_usaha$longitude))

# Cleaning strings
data_usaha$kecamatan     <- trimws(as.character(data_usaha$kecamatan))
data_usaha$kategori_grup <- trimws(as.character(data_usaha$kategori_grup))
data_usaha$nama          <- trimws(as.character(data_usaha$nama))

# Replace NaN / Inf with NA
data_usaha$rating[!is.finite(data_usaha$rating)]               <- NA
data_usaha$rating[data_usaha$rating > 5 | data_usaha$rating < 1] <- NA
data_usaha$jumlah_ulasan[!is.finite(data_usaha$jumlah_ulasan)] <- NA
data_usaha$skor_potensi[!is.finite(data_usaha$skor_potensi)]   <- NA
data_usaha$kepadatan[!is.finite(data_usaha$kepadatan)]         <- 3000
data_usaha$latitude[!is.finite(data_usaha$latitude)]           <- NA
data_usaha$longitude[!is.finite(data_usaha$longitude)]         <- NA

data_usaha_spatial <- data_usaha %>%
  filter(!is.na(latitude), !is.na(longitude),
         latitude >= -5.25, latitude <= -5.05,
         longitude >= 119.35, longitude <= 119.55)

KECS   <- sort(unique(data_usaha$kecamatan))
GRUPS  <- sort(unique(data_usaha$kategori_grup))
LAYAKS <- sort(unique(data_usaha$label_kelayakan))

JENIS_USAHA <- c(
  "Coffee Shop / Kafe"      = "cafe",
  "Laundry Kiloan"          = "laundry",
  "Kos-kosan / Guest House" = "kost",
  "Warung Makan / Restoran" = "resto",
  "Barbershop / Salon"      = "salon",
  "Minimarket / Toko"       = "toko",
  "Bengkel Motor/Mobil"     = "bengkel",
  "Klinik / Apotek"         = "klinik",
  "Percetakan / Fotocopy"   = "cetak",
  "Futsal / Olahraga"       = "olahraga"
)

JENIS_KAT <- list(
  cafe     = c("Cafe & Modern Coffee", "Kafe", "Kedai Kopi"),
  laundry  = c("Laundry", "Layanan Binatu"),
  kost     = c("Lain-lain / Belum Terkategori", "Penginapan"),
  resto    = c("Restoran/Rumah Makan","Warung Makan Spesifik","Warkop Tradisional", "Restoran", "Rumah Makan"),
  salon    = c("Kecantikan & Barbershop", "Barbershop", "Salon"),
  toko     = c("Minimarket & Toko Sembako", "Toko", "Minimarket"),
  bengkel  = c("Otomotif & Bengkel", "Bengkel"),
  klinik   = c("Kesehatan & Medis", "Klinik", "Apotek"),
  cetak    = c("Percetakan & Fotocopy", "Percetakan"),
  olahraga = c("Lain-lain / Belum Terkategori", "Futsal", "Gym")
)


PROFIT <- list(
  cafe     = list(min=8,  max=35, modal=50),
  laundry  = list(min=3,  max=15, modal=20),
  kost     = list(min=5,  max=30, modal=150),
  resto    = list(min=5,  max=25, modal=30),
  salon    = list(min=4,  max=18, modal=25),
  toko     = list(min=6,  max=20, modal=40),
  bengkel  = list(min=4,  max=20, modal=35),
  klinik   = list(min=8,  max=40, modal=80),
  cetak    = list(min=3,  max=12, modal=15),
  olahraga = list(min=5,  max=25, modal=60)
)

# ============================================================
# GROQ AI
# ============================================================
GROQ_KEY   <- Sys.getenv("GROQ_API_KEY")
GROQ_MODEL <- "llama-3.3-70b-versatile"

groq_chat <- function(msgs, sys = NULL) {
  all_msgs <- if (!is.null(sys)) c(list(list(role="system", content=sys)), msgs) else msgs
  tryCatch({
    r <- httr::POST(
      "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers(Authorization=paste("Bearer",GROQ_KEY), `Content-Type`="application/json"),
      body = jsonlite::toJSON(list(model=GROQ_MODEL, messages=all_msgs,
                                   max_tokens=900, temperature=0.7), auto_unbox=TRUE),
      encode = "raw", httr::timeout(30)
    )
    ct <- httr::content(r, as="parsed")
    if (!is.null(ct$error)) return(paste0("⚠️ ", ct$error$message))
    ct$choices[[1]]$message$content
  }, error = function(e) paste0("⚠️ Koneksi error: ", e$message))
}

build_sys_prompt <- function(d) {
  kec <- d %>% group_by(kecamatan) %>%
    summarise(n=n(), rat=round(mean(rating,na.rm=T),2),
              pot=round(mean(skor_potensi,na.rm=T),1),
              kep=mean(kepadatan,na.rm=T),
              top=names(sort(table(kategori_grup),dec=T))[1], .groups="drop") %>%
    arrange(desc(pot))
  kec_txt <- paste(apply(kec,1,function(r) paste0(r["kecamatan"],"(n=",r["n"],
                                                  ",pot=",r["pot"],",rating=",r["rat"],",kep/km2=",round(as.numeric(r["kep"])),
                                                  ",top:",r["top"],")")), collapse="; ")
  kat <- d %>% count(kategori_grup) %>% arrange(desc(n))
  kat_txt <- paste(paste0(kat$kategori_grup,"(",kat$n,")"), collapse=", ")
  top5 <- d %>% arrange(desc(skor_potensi)) %>% head(5)
  t5_txt <- paste(paste0(top5$nama," di ",top5$kecamatan," (skor:",top5$skor_potensi,",⭐",top5$rating,")"),collapse="; ")
  
  paste0(
    "Kamu adalah kamanakang uscu, AI Business Advisor dari platform Uscu Technology — sistem analisis lokasi usaha Makassar.\n",
    "Kamu cerdas, ramah, dan memberikan saran berbasis DATA NYATA dari dataset.\n\n",
    "=== DATA REAL-TIME ===\n",
    "Total: ",nrow(d)," lokasi usaha di ",nrow(kec)," kecamatan\n",
    "Kategori: ",kat_txt,"\n",
    "Per kecamatan: ",kec_txt,"\n",
    "Top 5 lokasi terbaik: ",t5_txt,"\n",
    "Rata-rata: rating=",round(mean(d$rating,na.rm=T),2)," | potensi=",round(mean(d$skor_potensi,na.rm=T),1),"/100\n\n",
    "=== DATA KEPADATAN BPS 2024 ===\n",
    "Makassar Kota: 8.282 org/km² (paling padat di Sulsel)\n",
    "Per kecamatan: Ujung Pandang(12.450) > Wajo(11.800) > Bontoala(10.900) > Mamajang(10.200) > Mariso(9.850)\n",
    "Biringkanaya(2.900) dan Tamalanrea(2.600) → kawasan berkembang dengan lahan lebih luas\n\n",
    "=== INSIGHT BISNIS ===\n",
    "• Cafe/F&B paling cocok di Panakkukang & Rappocini (kepadatan sedang-tinggi, daya beli tinggi)\n",
    "• Laundry kiloan → ROI 6-10 bulan, cocok di sekitar kampus (Tamalanrea, Biringkanaya)\n",
    "• Kos-kosan → passive income, return 8-15 tahun, paling menarik dekat UNHAS & universitas lain\n",
    "• Ujung Pandang: pusat bisnis, kompetisi tinggi, butuh diferensiasi kuat\n\n",
    "=== CARA MENJAWAB ===\n",
    "• Bahasa Indonesia yang natural, ramah, dan informatif\n",
    "• Gunakan data angka spesifik dari dataset di atas\n",
    "• Berikan rekomendasi yang actionable\n",
    "• Ingat seluruh konteks percakapan sebelumnya\n",
    "• Format rapi dengan poin atau paragraf sesuai konteks pertanyaan"
  )
}

# ============================================================
# CHART HELPERS
# ============================================================
PAL <- c("#4F46E5","#10B981","#F59E0B","#EF4444","#8B5CF6",
         "#06B6D4","#EC4899","#84CC16","#F97316","#6366F1",
         "#14B8A6","#F43F5E","#A78BFA","#34D399","#FCD34D")

mk <- function(...) {
  base <- list(
    plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)",
    font=list(family="Inter,sans-serif", color="#6B7280", size=11),
    xaxis=list(gridcolor="#F3F4F6", zerolinecolor="#F3F4F6",
               tickfont=list(color="#9CA3AF",size=11), linecolor="#E5E7EB"),
    yaxis=list(gridcolor="#F3F4F6", zerolinecolor="#F3F4F6",
               tickfont=list(color="#9CA3AF",size=11), linecolor="#E5E7EB"),
    margin=list(t=12,b=44,l=54,r=22),
    legend=list(bgcolor="rgba(0,0,0,0)", font=list(color="#6B7280",size=11))
  )
  modifyList(base, list(...))
}

gg_clean <- function() {
  theme_minimal(base_size=10.5) +
    theme(
      plot.background  = element_rect(fill="#FFFFFF", color=NA),
      panel.background = element_rect(fill="#FFFFFF", color=NA),
      panel.grid.major = element_line(color="#F3F4F6", linewidth=0.5),
      panel.grid.minor = element_blank(),
      axis.text  = element_text(color="#9CA3AF", size=9.5),
      axis.title = element_text(color="#6B7280", size=10),
      plot.margin = margin(14,14,14,14)
    )
}

# JS
JS_CHAT <- "
Shiny.addCustomMessageHandler('scrollChat', function(m) {
  setTimeout(function(){
    var el = document.getElementById('chat_body');
    if(el) el.scrollTop = el.scrollHeight;
  }, 80);
});
"
JS_ENTER <- "
$(document).on('keydown','#chat_input',function(e){
  if(e.key==='Enter' && !e.shiftKey){ e.preventDefault(); $('#btn_send').click(); }
});
"

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  title = "Uscu Technology — Makassar Business Dashboard",
  skin = "black",
  
  dashboardHeader(
    titleWidth = 256,
    title = tags$div(
      style = "display:flex; align-items:center; justify-content:center; height:60px; width:100%;",
      
      # 🔥 LOGO CONTAINER
      tags$div(
        style = "display:flex; align-items:center; gap:0px;",
        
        # 🔥 LOGO GAMBAR (Ukuran Presisi)
        tags$img(
          src = "idcu.png",
          height = "125px",
          style = "object-fit:contain;"
        ),
        
        # 🔥 TEKS BRAND
        tags$div(
          style = "display:flex; flex-direction:column; align-items:flex-start; line-height:1.0; margin-left:-12px; margin-top:2px;",
          tags$span(
            style = "font-size:18px; font-weight:900; color:#FFFFFF; letter-spacing:-0.5px;",
            "Uscu"
          ),
          tags$span(
            style = "font-size:10px; font-weight:600; color:rgba(255,255,255,0.8); text-transform:uppercase; letter-spacing:1px;",
            "Technology"
          )
        )
      )
    )
  ),
  
  dashboardSidebar(
    width = 256,
    tags$head(
      tags$link(rel="shortcut icon", href="favicon.ico"),
      tags$link(rel="stylesheet", href="global_styles.css"),
      tags$script(HTML(JS_CHAT)),
      tags$script(HTML(JS_ENTER))
    ),
    
    sidebarMenu(
      id = "tabs",
      
      # Status
      # tags$div(class="status-badge",
      #          tags$span(class="status-dot"),
      #          tags$span(class="status-text","Live · 6.394 Lokasi")),
      
      tags$span(class="nav-section-label","Analisis"),
      menuItem("Dashboard",           tabName="home",       icon=icon("chart-pie")),
      menuItem("Peta Interaktif",     tabName="map",        icon=icon("map-location-dot")),
      menuItem("K-Means Clustering",  tabName="kmeans",     icon=icon("circle-nodes")),
      menuItem("DBSCAN Spatial",      tabName="dbscan_tab", icon=icon("layer-group")),
      menuItem("Komparasi Algoritma", tabName="compare",    icon=icon("code-compare")),
      menuItem("Eksplorasi Data",     tabName="explore",    icon=icon("magnifying-glass-chart")),
      
      tags$hr(class="nav-divider"),
      tags$span(class="nav-section-label","Tools"),
      menuItem("Simulasi Usaha",      tabName="simulasi",   icon=icon("store")),
      menuItem("Ranking Lokasi",      tabName="ranking",    icon=icon("trophy")),
      menuItem("Kepadatan Penduduk",  tabName="kepadatan",  icon=icon("people-group")),
      menuItem("AI Chat Advisor",     tabName="ai_chat",    icon=icon("robot")),
      menuItem("Data Lab",            tabName="data_tab",   icon=icon("table")),
      menuItem("Tentang",             tabName="about",      icon=icon("circle-info")),
      
      tags$hr(class="nav-divider"),
      tags$span(class="nav-section-label","Filter"),
      tags$div(class="sidebar-panel",
               pickerInput("f_kec","Kecamatan",
                           choices=c("Semua",KECS), selected="Semua",
                           options=pickerOptions(liveSearch=TRUE,size=8)),
               pickerInput("f_grup","Kategori",
                           choices=c("Semua",GRUPS), selected="Semua",
                           options=pickerOptions(liveSearch=TRUE,size=8)),
               sliderInput("f_rating","Min Rating",   min=1, max=5,   value=1,   step=0.5),
               sliderInput("f_pot",   "Min Potensi",  min=0, max=100, value=0,   step=5)
      ),
      
      tags$hr(class="nav-divider"),
      tags$span(class="nav-section-label","Clustering"),
      tags$div(class="sidebar-panel",
               sliderInput("k_n","K — K-Means",        min=2, max=10,  value=4,   step=1),
               sliderInput("eps","Epsilon DBSCAN",     min=0.01,max=0.40,value=0.08,step=0.01),
               sliderInput("mpts","MinPts DBSCAN",     min=2, max=20,  value=5,   step=1),
               tags$br(),
               tags$button(id="run_analysis",
                           class="btn btn-run action-button shiny-bound-input",
                           icon("bolt"),"  Jalankan Analisis")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── DASHBOARD ──────────────────────────────────────
      tabItem("home",
              fluidRow(
                valueBoxOutput("vb1",3), valueBoxOutput("vb2",3),
                valueBoxOutput("vb3",3), valueBoxOutput("vb4",3)
              ),
              fluidRow(
                valueBoxOutput("vb5",3), valueBoxOutput("vb6",3),
                valueBoxOutput("vb7",3), valueBoxOutput("vb8",3)
              ),
              fluidRow(
                box("Kategori Usaha",  width=6, withSpinner(plotlyOutput("ph_kat",   height="260px"),color="#4F46E5",type=4)),
                box("Rating",          width=6, withSpinner(plotlyOutput("ph_rat",   height="260px"),color="#4F46E5",type=4))
              ),
              fluidRow(
                box("Kelayakan",       width=6, withSpinner(plotlyOutput("ph_layak", height="260px"),color="#4F46E5",type=4)),
                box("Popularitas",     width=6, withSpinner(plotlyOutput("ph_pop",   height="260px"),color="#4F46E5",type=4))
              ),
              fluidRow(
                box("Scatter 3D — Rating × Ulasan × Potensi", width=8,
                    withSpinner(plotlyOutput("ph_3d",   height="340px"),color="#4F46E5",type=4)),
                box("Treemap Kecamatan × Kategori",           width=4,
                    withSpinner(plotlyOutput("ph_tree", height="340px"),color="#4F46E5",type=4))
              ),
              fluidRow(
                box("Top 15 Lokasi — Skor Potensi Tertinggi", width=12,
                    withSpinner(plotlyOutput("ph_top",  height="230px"),color="#4F46E5",type=4))
              )
      ),
      
      # ── EKSPLORASI DATA ────────────────────────────────
      tabItem("explore",
              tags$div(class="tab-content-wrapper",
                       # 1 Large Box
                       fluidRow(
                         box(title="Market Intelligence Summary", width=12, status="primary", solidHeader=TRUE,
                             tags$p(style="color:#6B7280;margin-bottom:15px;", 
                                    "Analisis mendalam distribusi bisnis di Kota Makassar."),
                             fluidRow(
                               column(3, uiOutput("ex_total_biz")),
                               column(3, uiOutput("ex_avg_rating")),
                               column(3, uiOutput("ex_top_kec")),
                               column(3, uiOutput("ex_top_kat"))
                             )
                         )
                       ),
                       # 2 Medium Boxes
                       fluidRow(
                         box(title="Distribusi Rating", width=6,
                             withSpinner(plotlyOutput("ex_rating_chart", height="320px"), color="#4F46E5", type=4)),
                         box(title="Tingkat Popularitas Usaha", width=6,
                             withSpinner(plotlyOutput("ex_pop_chart", height="320px"), color="#4F46E5", type=4))
                       ),
                       # 2 Medium Boxes
                       fluidRow(
                         box(title="Kepadatan Penduduk per Wilayah", width=6,
                             withSpinner(plotlyOutput("ex_kep_chart", height="320px"), color="#4F46E5", type=4)),
                         box(title="Ranking Potensi Usaha per Kecamatan", width=6,
                             withSpinner(plotlyOutput("ex_pot_chart", height="320px"), color="#4F46E5", type=4))
                       )
              )
      ),
      
      # ── PETA ───────────────────────────────────────────
      tabItem("map",
              fluidRow(
                box(width=12, title="Peta Interaktif Lokasi Usaha — Kota Makassar",
                    fluidRow(
                      column(2, radioGroupButtons("map_mode","Tampilan",
                                                  choices=c("Toko"="all","Wilayah"="agg","Kecamatan"="kc","K-Means"="km","Potensi"="heat","Kepadatan"="pop"),
                                                  selected="all",size="xs",status="primary")),
                      column(2, radioGroupButtons("map_tile","Basemap",
                                                  choices=c("Terang"="lt","Gelap"="dk","Satelit"="st","Topografi"="tp"),
                                                  selected="lt",size="xs",status="primary")),
                      column(2, switchInput("sw_cluster","Cluster",   FALSE,size="small",onStatus="primary")),
                      column(2, switchInput("sw_heat",   "Heatmap",   FALSE,size="small",onStatus="success")),
                      column(2, switchInput("sw_foto",   "Foto",      TRUE, size="small",onStatus="warning")),
                      column(2, switchInput("sw_mini",   "Minimap",   TRUE, size="small",onStatus="info"))
                    ),
                    tags$div(style="height:10px"),
                    withSpinner(leafletOutput("map_out",height="520px"),color="#4F46E5",type=4),
                    tags$div(style="height:8px"),
                    uiOutput("map_chips")
                )
              )
      ),
      
      # ── K-MEANS ────────────────────────────────────────
      tabItem("kmeans",
              fluidRow(
                box(title="PCA Biplot", width=8,
                    fluidRow(
                      column(5, radioGroupButtons("pca_col","Warna",
                                                  choices=c("Cluster"="cl","Kecamatan"="kc","Kelayakan"="pt"),
                                                  selected="cl",size="xs",status="primary")),
                      column(4, switchInput("pca_3d","Mode 3D",FALSE,size="small",onStatus="warning"))
                    ),
                    tags$div(style="height:6px"),
                    withSpinner(uiOutput("pca_out"),color="#4F46E5",type=4)
                ),
                box(title="Diagnostik", width=4,
                    tabsetPanel(
                      tabPanel("Elbow",     withSpinner(plotOutput("p_elbow",height="280px"),color="#4F46E5",type=4)),
                      tabPanel("Silhouette",withSpinner(plotOutput("p_sil",  height="280px"),color="#4F46E5",type=4)),
                      tabPanel("Gap Stat",  withSpinner(plotOutput("p_gap",  height="280px"),color="#4F46E5",type=4))
                    )
                )
              ),
              fluidRow(
                box("Radar Chart — Profil Cluster",width=5,
                    withSpinner(plotlyOutput("p_radar",height="320px"),color="#4F46E5",type=4)),
                box("Box Plot per Cluster",        width=4,
                    selectInput("box_v","Variabel",
                                choices=c("Rating"="rating","Ulasan"="jumlah_ulasan","Potensi"="skor_potensi"),
                                selected="skor_potensi"),
                    withSpinner(plotlyOutput("p_box",height="260px"),color="#4F46E5",type=4)),
                box("Cluster Stats",               width=3,
                    withSpinner(tableOutput("cl_stats"),color="#4F46E5",type=4))
              ),
              fluidRow(
                box("Rekomendasi Strategis per Cluster",width=12, uiOutput("km_cards"))
              )
      ),
      
      # ── DBSCAN ─────────────────────────────────────────
      tabItem("dbscan_tab",
              fluidRow(
                box("Scatter Plot DBSCAN",width=7,
                    withSpinner(plotlyOutput("db_scatter",height="400px"),color="#4F46E5",type=4)),
                box("Diagnostik DBSCAN", width=5,
                    tabsetPanel(
                      tabPanel("k-NN Distance", withSpinner(plotOutput("db_knn",  height="330px"),color="#4F46E5",type=4)),
                      tabPanel("Reachability",  withSpinner(plotOutput("db_reach",height="330px"),color="#4F46E5",type=4))
                    )
                )
              ),
              fluidRow(
                box("Distribusi Cluster",         width=4,
                    withSpinner(plotlyOutput("db_bar",  height="240px"),color="#4F46E5",type=4)),
                box("Heatmap Cluster × Kategori", width=5,
                    withSpinner(plotlyOutput("db_heat", height="240px"),color="#4F46E5",type=4)),
                box("Statistik Noise",            width=3,
                    withSpinner(uiOutput("db_noise"),   color="#4F46E5",type=4))
              )
      ),
      
      # ── KOMPARASI ──────────────────────────────────────
      tabItem("compare",
              fluidRow(
                box("Confusion Matrix K-Means vs DBSCAN",width=6,
                    withSpinner(plotlyOutput("p_conf",    height="330px"),color="#4F46E5",type=4)),
                box("Metrik Kualitas Clustering",         width=6,
                    withSpinner(uiOutput("p_metrics"),    color="#4F46E5",type=4))
              ),
              fluidRow(
                box("Parallel Coordinates — Profil Dimensi",width=12,
                    withSpinner(plotlyOutput("p_parallel", height="310px"),color="#4F46E5",type=4))
              )
      ),
      
      # ── EKSPLORASI ─────────────────────────────────────
      tabItem("explore",
              fluidRow(
                box("Multi-Variable Explorer",width=8,
                    fluidRow(
                      column(3,selectInput("ex_x","Sumbu X",
                                           choices=c("Rating"="rating","Ulasan"="jumlah_ulasan","Potensi"="skor_potensi","Kepadatan"="kepadatan"),selected="rating")),
                      column(3,selectInput("ex_y","Sumbu Y",
                                           choices=c("Potensi"="skor_potensi","Rating"="rating","Ulasan"="jumlah_ulasan","Kepadatan"="kepadatan"),selected="skor_potensi")),
                      column(3,selectInput("ex_c","Warna",
                                           choices=c("Kategori"="kategori_grup","Kecamatan"="kecamatan","Kelayakan"="label_kelayakan"),selected="kategori_grup")),
                      column(3,selectInput("ex_s","Ukuran",
                                           choices=c("Ulasan"="jumlah_ulasan","Potensi"="skor_potensi","Rating"="rating"),selected="jumlah_ulasan"))
                    ),
                    withSpinner(plotlyOutput("p_ex",height="370px"),color="#4F46E5",type=4)
                ),
                box("Distribusi",width=4,
                    selectInput("ex_hv","Variabel",
                                choices=c("Rating"="rating","Ulasan"="jumlah_ulasan","Potensi"="skor_potensi"),selected="skor_potensi"),
                    withSpinner(plotlyOutput("p_ex_h",height="155px"),color="#4F46E5",type=4),
                    tags$div(style="height:6px"),
                    withSpinner(plotlyOutput("p_ex_b",height="155px"),color="#4F46E5",type=4)
                )
              ),
              fluidRow(
                box("Correlation Heatmap",       width=4,
                    withSpinner(plotlyOutput("p_corr",  height="310px"),color="#4F46E5",type=4)),
                box("Bubble Chart Kecamatan",    width=5,
                    withSpinner(plotlyOutput("p_bubble",height="310px"),color="#4F46E5",type=4)),
                box("Ranking Kecamatan",         width=3,
                    withSpinner(plotlyOutput("p_rank",  height="310px"),color="#4F46E5",type=4))
              )
      ),
      
      # ── SIMULASI ───────────────────────────────────────
      tabItem("simulasi",
              fluidRow(
                box("Parameter & Ringkasan", width=4,
                    radioGroupButtons("sim_mode", "Pilih Mode", 
                                      choices=c("Rekomendasi"="auto", "Analisis Lokasi Saya"="custom"),
                                      selected="auto", status="primary", size="sm", justified=TRUE),
                    tags$hr(class="divider"),
                    uiOutput("sim_input_ui"),
                    sliderInput("sim_budget","Modal (juta Rp)",min=10,max=500,value=50,step=10),
                    actionButton("run_sim", "🚀  Jalankan Analisis", class="btn-action", style="width:100%; margin-bottom:15px;"),
                    uiOutput("sim_sum")
                ),
                box("Hasil Analisis & Keuntungan", width=4,
                    uiOutput("sim_profit"),
                    tags$hr(class="divider"),
                    plotlyOutput("sim_chart",height="230px")
                ),
                box("Insight Kompetisi", width=4,
                    tags$div(class="note-box", style="height:360px; overflow-y:auto;",
                             tags$b("Informasi Lokasi:"),
                             uiOutput("sim_loc_info"),
                             tags$hr(class="divider"),
                             tags$b("Daftar Kompetitor Terdekat:"),
                             uiOutput("sim_near_list"),
                             tags$hr(class="divider"),
                             tags$b("Metrik Kedekatan:"),
                             uiOutput("sim_dist_stats"))
                )
              ),
              fluidRow(
                box("Peta Simulasi Interaktif", width=12,
                    tags$p(style="font-size:12px;color:#6B7280;margin-bottom:8px;",
                           "💡 Klik pada peta dalam mode 'Analisis Lokasi Saya' untuk menentukan titik usaha Anda."),
                    leafletOutput("sim_map", height="500px"))
              ),
              fluidRow(
                box("Ranking Lengkap Rekomendasi Lokasi", width=12,
                    withSpinner(uiOutput("sim_rank"), color="#10B981", type=4))
              )
      ),
      
      # ── RANKING ────────────────────────────────────────
      tabItem("ranking",
              fluidRow(
                box(width=12, title="Filter & Pengurutan",
                    fluidRow(
                      column(3,pickerInput("rk_kat","Kategori",choices=c("Semua",GRUPS),selected="Semua",
                                           options=pickerOptions(liveSearch=TRUE))),
                      column(3,pickerInput("rk_kec","Kecamatan",choices=c("Semua",KECS),selected="Semua",
                                           options=pickerOptions(liveSearch=TRUE))),
                      column(3,selectInput("rk_sort","Urutkan",
                                           choices=c("Skor Potensi"="skor_potensi","Rating"="rating","Ulasan"="jumlah_ulasan"),
                                           selected="skor_potensi")),
                      column(3,sliderInput("rk_n","Top N",min=5,max=50,value=20,step=5))
                    )
                )
              ),
              fluidRow(
                box("Daftar Ranking",  width=5,
                    withSpinner(uiOutput("rk_list"),  color="#4F46E5",type=4)),
                box("Visualisasi",     width=7,
                    withSpinner(plotlyOutput("rk_chart",height="510px"),color="#4F46E5",type=4))
              )
      ),
      
       tabItem("kepadatan",
               fluidRow(
                 box("Analisis Kepadatan per Kecamatan — Kota Makassar", width=8,
                     tags$div(class="kep-section-title","Distribusi Kepadatan Penduduk"),
                     tags$div(class="kep-section-sub","Sumber: Data BPS Makassar 2024 (jiwa/km²)"),
                     withSpinner(plotlyOutput("kep_kec_chart", height="380px"), color="#4F46E5", type=4)
                 ),
                 box("Statistik Kepadatan", width=4,
                     tags$div(class="kep-section-title","Highlight Wilayah"),
                     tags$div(class="kep-section-sub","Kecamatan dengan konsentrasi penduduk tertinggi"),
                     withSpinner(uiOutput("kep_kec_ui"), color="#4F46E5", type=4)
                 )
               ),
              fluidRow(
                box("Korelasi: Kepadatan vs Skor Potensi Usaha", width=6,
                    tags$div(class="kep-section-sub","Melihat hubungan antara keramaian vs peluang"),
                    withSpinner(plotlyOutput("kep_scatter",height="310px"),color="#4F46E5",type=4)),
                box("Ranking Potensi Usaha Terbobot (Kepadatan)", width=6,
                    tags$div(class="kep-section-sub","Skor akhir berdasarkan potensi & kepadatan penduduk"),
                    withSpinner(plotlyOutput("kep_bar",   height="310px"),color="#4F46E5",type=4))
              )
      ),
      
      # ── AI CHAT ────────────────────────────────────────
      tabItem("ai_chat",
              fluidRow(
                # Main chat column
                column(8,
                       tags$div(class="chatbot-outer",
                                
                                # Header
                                tags$div(class="chat-header",
                                         tags$div(class="chat-avatar",
                                                  # Try SVG robot icon
                                                  tags$img(src="idcu.png", style="width:100%;height:130%;",
                                                           onerror="this.style.display='none';this.nextSibling.style.display='flex'"),
                                                  tags$div(class="chat-avatar-emoji", style="display:none","🤖")
                                         ),
                                         tags$div(class="chat-header-info",
                                                  tags$div(class="chat-header-name","kamanakang uscu"),
                                                  tags$div(class="chat-header-brand",
                                                           "Powered by ",
                                                           tags$span(class="chat-header-brand-name","Ucu Technology")
                                                  )
                                         ),
                                         tags$div(class="chat-online",
                                                  tags$span(class="chat-online-dot"),
                                                  tags$span(class="chat-online-text","Online")
                                         )
                                ),
                                
                                # Messages
                                tags$div(id="chat_body", class="chat-messages",
                                         uiOutput("chat_ui")
                                ),
                                
                                # Input bar
                                tags$div(class="chat-input-bar",
                                         tags$div(class="chat-input-wrap",
                                                  tags$textarea(id="chat_input", class="chat-textarea", rows=1,
                                                                placeholder="Tanya tentang lokasi usaha, estimasi modal, analisis data... (Enter kirim)")
                                         ),
                                         actionButton("btn_send", "➤", class="chat-send-btn")
                                )
                                
                       ),
                       tags$p(style="font-size:11px;color:#9CA3AF;margin-top:8px;",
                              "⌨️  Enter untuk kirim  ·  Shift+Enter untuk baris baru  ·  AI mengingat seluruh percakapan sesi ini")
                ),
                
                # Right panel
                column(4,
                       # Quick questions
                       box(title="Pertanyaan Cepat", width=12,
                           tags$div(class="quick-q-wrap",
                                    actionButton("qq1","📍 Kecamatan terbaik untuk coffee shop?",    class="quick-q-btn"),
                                    actionButton("qq2","💰 Modal & ROI laundry kiloan?",              class="quick-q-btn"),
                                    actionButton("qq3","🏆 Kategori usaha paling potensial?",         class="quick-q-btn"),
                                    actionButton("qq4","👥 Pengaruh kepadatan penduduk?",             class="quick-q-btn"),
                                    actionButton("qq5","🎯 Tips memilih lokasi usaha?",               class="quick-q-btn"),
                                    actionButton("qq6","🔥 Analisis peluang Panakkukang?",            class="quick-q-btn"),
                                    actionButton("qq7","📈 Tren bisnis Makassar 2025-2026?",          class="quick-q-btn"),
                                    actionButton("qq8","⚡ Kapan pakai K-Means vs DBSCAN?",           class="quick-q-btn")
                           )
                       ),
                       
                       # Stats
                       box(title="Dataset Aktif", width=12,
                           withSpinner(uiOutput("ai_stats"),color="#8B5CF6",type=4)
                       ),
                       
                       # Clear
                       box(width=12, solidHeader=FALSE,
                           actionButton("btn_clear","🗑  Bersihkan Percakapan",class="btn-clear")
                       )
                )
              )
      ),
      
      # ── DATA LAB ───────────────────────────────────────
      tabItem("data_tab",
              fluidRow(
                box("Data Laboratory", width=12,
                    fluidRow(
                      column(2, downloadButton("dl1","⬇ Full CSV",  class="btn btn-export btn-export-green")),
                      column(2, downloadButton("dl2","⬇ Filtered",  class="btn btn-export btn-export-blue")),
                      column(3, selectInput("tbl_cols","Kolom",
                                            choices=c("Semua"="all","Dasar"="basic","Koordinat"="coord","Cluster"="cluster"),
                                            selected="all")),
                      column(5, 
                             actionButton("show_report_modal", "📄 Buat Laporan Presentasi (PDF)", 
                                          class="btn btn-export", style="background:#8B5CF6; color:white; border:none; width:100%;"))
                    ),
                    tags$div(style="height:10px"),
                    withSpinner(DTOutput("data_tbl"),color="#4F46E5",type=4)
                )
              )
      ),
      
      # ── TENTANG ────────────────────────────────────────
      tabItem("about",
              fluidRow(
                box("Tentang Platform",width=8,
                    tags$h2(style="font-size:24px;font-weight:800;color:#111827;letter-spacing:-.5px;margin-bottom:8px;",
                            "Lokasi Usaha Makassar"),
                    tags$p(style="font-size:14px;color:#4B5563;line-height:1.8;max-width:620px;",
                           "Platform Decision Support System berbasis machine learning untuk analisis dan
               penentuan lokasi usaha optimal di Kota Makassar, Sulawesi Selatan.
               Mengintegrasikan data Google Maps, kepadatan penduduk BPS 2024,
               algoritma clustering K-Means & DBSCAN, dan AI berbasis Groq LLaMA 3.3."),
                    tags$hr(class="divider"),
                    tags$p(style="font-size:11px;font-weight:700;color:#9CA3AF;text-transform:uppercase;letter-spacing:.8px;margin-bottom:10px;",
                           "Fitur Platform"),
                    tags$ul(style="font-size:13.5px;color:#374151;line-height:2.1;padding-left:20px;",
                            tags$li("K-Means Clustering + Auto K Selection (Elbow, Silhouette, Gap Stat)"),
                            tags$li("DBSCAN Spatial Clustering dengan deteksi noise otomatis"),
                            tags$li("Peta Interaktif Leaflet dengan heatmap, cluster marker, dan foto usaha"),
                            tags$li("Data Kepadatan Penduduk BPS 2024 terintegrasi penuh"),
                            tags$li("Simulasi 10 jenis usaha dengan estimasi profit & ROI"),
                            tags$li("Sistem Ranking berbasis weighted scoring"),
                            tags$li("AI Chat Advisor — Groq LLaMA 3.3 dengan memory percakapan")
                    ),
                    tags$hr(class="divider"),
                    tags$div(class="note-box",
                             tags$b("Dataset: "), paste0(nrow(data_usaha)," lokasi usaha · 15 kecamatan · ",
                                                         length(GRUPS)," kategori · Makassar 2026")
                    )
                ),
                box("Dikembangkan Oleh", width=4,
                    tags$div(style="text-align:center; padding:30px 0;",
                             tags$img(src="id2.png", style="height:140px; margin-bottom:20px; object-fit:contain;"),
                             tags$div(style="font-size:18px; font-weight:800; color:#111827;", "Uscu Technology"),
                             tags$div(style="font-size:12px; color:#9CA3AF; margin-top:6px; letter-spacing:1px; text-transform:uppercase;", "Inovasi Berbasis Data")
                    ),
                    tags$hr(class="divider"),
                    tags$p(style="font-size:11px;font-weight:700;color:#9CA3AF;text-transform:uppercase;letter-spacing:.8px;margin-bottom:10px;",
                           "Tech Stack"),
                    tags$div(lapply(c("R 4.3+","Shiny","Leaflet","Plotly","ggplot2",
                                      "K-Means","DBSCAN","Groq LLaMA 3.3","BPS 2024","Google Maps API"),
                                    function(x) tags$span(class="tech-badge",x)))
                )
              )
      )
      
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ── REACTIVE DATA ────────────────────────────────────────
  fd <- reactive({
    d <- data_usaha
    if (!is.null(input$f_kec)  && input$f_kec  !="Semua") d <- d[d$kecamatan    ==input$f_kec, ]
    if (!is.null(input$f_grup) && input$f_grup !="Semua") d <- d[d$kategori_grup==input$f_grup,]
    # Pastikan numeric sebelum compare dengan slider
    rat <- as.numeric(d$rating)
    pot <- as.numeric(d$skor_potensi)
    f_rat <- as.numeric(input$f_rating)
    f_pot <- as.numeric(input$f_pot)
    d <- d[!is.na(rat) & rat >= f_rat, ]
    rat <- rat[!is.na(rat) & rat >= f_rat]   # keep in sync (not used further)
    pot2 <- as.numeric(d$skor_potensi)
    d <- d[!is.na(pot2) & pot2 >= f_pot, ]
    d
  })

  fd_sp <- reactive({
    d <- fd()
    lat <- as.numeric(d$latitude)
    lon <- as.numeric(d$longitude)
    d[!is.na(lat) & !is.na(lon) &
      lat >= -5.25 & lat <= -5.05 &
      lon >= 119.35 & lon <= 119.55, ]
  })
  
  ar <- eventReactive(input$run_analysis, {
    d <- fd_sp()
    k_val <- as.numeric(input$k_n)
    if(is.na(k_val) || length(k_val)==0) k_val <- 4
    
    # Validasi awal
    if(nrow(d) < k_val + 1) {
      showNotification("Data tidak cukup untuk analisis (butuh > k lokasi)", type="warning")
      return(NULL)
    }
    
    # Gunakan Progress object yang lebih stabil
    p <- Progress$new(session, min=0, max=1)
    on.exit(p$close())
    p$set(message="Menjalankan Analisis...", value=0)
    
    tryCatch({
      p$set(value=0.2, detail="K-Means Clustering...")
      num_cols <- c("latitude","longitude","rating","jumlah_ulasan","skor_potensi")
      # Pastikan data bersih
      d_clean <- d[complete.cases(d[num_cols]), ]
      if(nrow(d_clean) < k_val + 1) return(NULL)
      
      fs <- scale(d_clean[, num_cols])
      fs[is.nan(fs) | is.infinite(fs)] <- 0
      
      km <- kmeans(fs, centers=k_val, nstart=25, iter.max=100)
      d_clean$cluster_kmeans <- as.factor(km$cluster)
      
      p$set(value=0.7, detail="DBSCAN Clustering...")
      f2 <- scale(d_clean[,c("latitude","longitude")])
      f2[is.nan(f2) | is.infinite(f2)] <- 0
      
      eps_val <- as.numeric(input$eps); if(is.na(eps_val) || length(eps_val)==0) eps_val <- 0.05
      mpts_val <- as.numeric(input$mpts); if(is.na(mpts_val) || length(mpts_val)==0) mpts_val <- 5
      
      db <- dbscan::dbscan(f2, eps=eps_val, minPts=mpts_val)
      d_clean$cluster_dbscan <- as.factor(db$cluster)
      
      p$set(value=1, detail="Selesai!")
      list(data=d_clean, km=km, scaled=fs)
    }, error = function(e) {
      showNotification(paste("Error Analisis:", e$message), type="error")
      NULL
    })
  }, ignoreNULL=FALSE)
  
  rd <- reactive({ res <- ar(); if(is.null(res)) fd_sp() else res$data })
  
  # ── VALUE BOXES ──────────────────────────────────────────
  mkVB <- function(v, l, sub="", col="#4F46E5") {
    valueBox(
      tags$span(style=paste0("font-family:'JetBrains Mono',monospace;font-size:30px;",
                             "font-weight:800;color:",col,";letter-spacing:-1.5px;"), v),
      tags$span(style="font-size:10px;color:#9CA3AF;text-transform:uppercase;letter-spacing:.8px;font-weight:700;", l),
      icon=NULL, color="black"
    )
  }
  
  output$vb1 <- renderValueBox(mkVB(nrow(fd()),                                         "Total Lokasi",  col="#111827"))
  output$vb2 <- renderValueBox(mkVB(length(unique(fd()$kecamatan)),                     "Kecamatan",     col="#4F46E5"))
  output$vb3 <- renderValueBox(mkVB(length(unique(fd()$kategori_grup)),                 "Kategori",      col="#10B981"))
  output$vb4 <- renderValueBox(mkVB(round(mean(fd()$rating,na.rm=T),2),                 "Avg Rating",    col="#F59E0B"))
  output$vb5 <- renderValueBox(mkVB(round(mean(fd()$skor_potensi,na.rm=T),1),           "Avg Potensi",   col="#8B5CF6"))
  output$vb6 <- renderValueBox(mkVB(round(mean(fd()$jumlah_ulasan,na.rm=T)),            "Avg Ulasan",    col="#06B6D4"))
  output$vb7 <- renderValueBox(mkVB(sum(fd()$rating==5,na.rm=T),                        "Rating 5★",     col="#EC4899"))
  output$vb8 <- renderValueBox(mkVB(sum(grepl("Sangat",fd()$label_kelayakan),na.rm=T),  "Sangat Layak",  col="#10B981"))
  
  # ── DASHBOARD CHARTS ────────────────────────────────────
  output$ph_kat <- renderPlotly({
    d <- fd() %>% count(kategori_grup) %>% arrange(n)
    d$kategori_grup <- factor(d$kategori_grup, levels=d$kategori_grup)
    plot_ly(d, x=~n, y=~kategori_grup, type="bar", orientation="h",
            marker=list(color=PAL[seq_len(nrow(d))], opacity=.85,
                        line=list(color="rgba(0,0,0,0)",width=0)),
            hovertemplate="<b>%{y}</b><br>%{x} lokasi<extra></extra>") %>%
      layout(mk(margin=list(t=5,b=35,l=150,r=12),
                xaxis=list(gridcolor="#F3F4F6",title=list(text="Jumlah Lokasi"),tickangle=0),
                yaxis=list(gridcolor="#F3F4F6",title=list(text=""),tickfont=list(color="#6B7280",size=10))))
  })
  
  output$ph_rat <- renderPlotly({
    plot_ly(fd(), x=~rating, type="histogram", nbinsx=9,
            marker=list(color="#4F46E5",opacity=.8,line=list(color="#F7F8FC",width=.5))) %>%
      layout(mk(margin=list(t=5,b=32,l=42,r=12),
                xaxis=list(gridcolor="#F3F4F6",title="Rating"),
                yaxis=list(gridcolor="#F3F4F6",title="")))
  })
  
  output$ph_layak <- renderPlotly({
    d <- fd() %>% count(label_kelayakan)
    plot_ly(d, labels=~label_kelayakan, values=~n, type="pie", hole=.58,
            marker=list(colors=c("#10B981","#4F46E5","#F59E0B","#EF4444"),
                        line=list(color="#F7F8FC",width=2)),
            textinfo="label+percent", textposition="outside",
            textfont=list(size=10,color="#6B7280"), hoverinfo="label+value+percent") %>%
      layout(mk(margin=list(t=15,b=15,l=25,r=25), showlegend=FALSE))
  })
  
  output$ph_pop <- renderPlotly({
    d <- fd() %>% count(popularitas) %>% arrange(desc(n))
    cm <- c("Sangat Populer"="#4F46E5","Populer"="#10B981","Cukup Dikenal"="#F59E0B",
            "Baru"="#8B5CF6","Tidak Ada Ulasan"="#E5E7EB")
    plot_ly(d, labels=~popularitas, values=~n, type="pie", hole=.58,
            marker=list(colors=cm[d$popularitas], line=list(color="#F7F8FC",width=2)),
            textinfo="label+percent", textposition="outside",
            textfont=list(size=10,color="#6B7280"), hoverinfo="label+value+percent") %>%
      layout(mk(margin=list(t=15,b=15,l=25,r=25), showlegend=FALSE))
  })
  
  output$ph_3d <- renderPlotly({
    d <- fd()
    plot_ly(d, x=~rating, y=~jumlah_ulasan, z=~skor_potensi,
            color=~kategori_grup, colors=PAL, type="scatter3d", mode="markers",
            marker=list(size=4,opacity=.78,line=list(color="rgba(0,0,0,.08)",width=.5)),
            text=~paste0("<b>",nama,"</b><br>",kecamatan,"<br>⭐",rating," | Potensi: ",skor_potensi),
            hoverinfo="text") %>%
      layout(scene=list(
        xaxis=list(title="Rating",  gridcolor="#E5E7EB",backgroundcolor="rgba(247,248,252,0.8)",showbackground=TRUE,tickfont=list(color="#9CA3AF",size=9)),
        yaxis=list(title="Ulasan",  gridcolor="#E5E7EB",backgroundcolor="rgba(247,248,252,0.8)",showbackground=TRUE,tickfont=list(color="#9CA3AF",size=9)),
        zaxis=list(title="Potensi", gridcolor="#E5E7EB",backgroundcolor="rgba(247,248,252,0.8)",showbackground=TRUE,tickfont=list(color="#9CA3AF",size=9)),
        bgcolor="rgba(0,0,0,0)",camera=list(eye=list(x=1.4,y=1.4,z=1.1))),
        paper_bgcolor="rgba(0,0,0,0)",
        legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#6B7280",size=10)),
        margin=list(t=0,b=0,l=0,r=0))
  })
  
  output$ph_tree <- renderPlotly({
    d <- fd() %>% count(kecamatan, kategori_grup)
    kec_counts <- fd() %>% count(kecamatan)
    
    # Menghindari konflik nama: Root diubah jadi "Kota Makassar" karena ada kecamatan bernama "Makassar"
    labels_leaf    <- paste(d$kecamatan, "·", d$kategori_grup)
    parents_leaf   <- d$kecamatan
    
    labels_parent  <- kec_counts$kecamatan
    parents_parent <- rep("Kota Makassar", nrow(kec_counts))
    
    all_labels  <- c("Kota Makassar", labels_parent, labels_leaf)
    all_parents <- c("", parents_parent, parents_leaf)
    all_values  <- c(0, rep(0, nrow(kec_counts)), d$n)
    
    plot_ly(type="treemap",
            labels = all_labels,
            parents = all_parents,
            values = all_values,
            textinfo = "label+value",
            marker=list(line=list(color="#F7F8FC",width=1)),
            textfont=list(color="#111827",size=10)) %>%
      layout(paper_bgcolor="rgba(0,0,0,0)", margin=list(t=10,b=10,l=10,r=10))
  })
  
  output$ph_top <- renderPlotly({
    d <- fd() %>% arrange(desc(skor_potensi)) %>% head(15)
    d$nm <- paste0(substr(d$nama,1,26), ifelse(nchar(d$nama)>26,"…",""))
    plot_ly(d, x=~skor_potensi, y=~reorder(nm,skor_potensi), type="bar", orientation="h",
            marker=list(color=~skor_potensi,
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
                        showscale=FALSE, opacity=.9),
            text=~paste0(kecamatan," · ⭐",rating),
            textposition="outside", textfont=list(color="#9CA3AF",size=10)) %>%
      layout(mk(margin=list(t=5,b=22,l=215,r=75),
                xaxis=list(gridcolor="#F3F4F6",title="Skor Potensi"),
                yaxis=list(gridcolor="#F3F4F6",title="",tickfont=list(color="#6B7280",size=10.5))))
  })
  
  # ── PETA ────────────────────────────────────────────────
  output$map_out <- renderLeaflet({
    d <- rd(); mode <- input$map_mode
    tile <- switch(input$map_tile,
                   "lt"=providers$CartoDB.Positron,
                   "dk"=providers$CartoDB.DarkMatter,
                   "st"=providers$Esri.WorldImagery,
                   "tp"=providers$OpenTopoMap)
    
    pal_fn <- if (mode=="km" && "cluster_kmeans"%in%names(d))
      colorFactor(PAL, domain=d$cluster_kmeans)
    else if (mode=="db" && "cluster_dbscan"%in%names(d))
      colorFactor(c("#9CA3AF",PAL), domain=d$cluster_dbscan)
    else if (mode=="heat")
      colorNumeric(c("#EEF2FF","#818CF8","#4F46E5","#10B981"), domain=d$skor_potensi)
    else if (mode=="pop")
      colorNumeric(c("#FFF7ED","#FB923C","#EA580C","#9A3412"), domain=d$kepadatan)
    else if (mode=="kc")
      colorFactor(PAL, domain=d$kecamatan)
    else
      colorFactor(PAL, domain=d$kategori_grup)
    
    cv <- if (mode=="km"&&"cluster_kmeans"%in%names(d)) d$cluster_kmeans
    else if (mode=="db"&&"cluster_dbscan"%in%names(d)) d$cluster_dbscan
    else if (mode=="heat") d$skor_potensi
    else if (mode=="pop") d$kepadatan
    else if (mode=="kc") d$kecamatan
    else d$kategori_grup
    
    rad <- if (mode=="heat") rescale(d$skor_potensi,to=c(4,15)) else 5.5
    
    foto_html <- ifelse(isTRUE(input$sw_foto) & !is.na(d$foto_url),
                        paste0('<img src="',d$foto_url,
                               '" style="width:100%;border-radius:10px;margin-bottom:10px;max-height:110px;object-fit:cover;"',
                               ' onerror="this.style.display=\'none\'">'), "")
    
    popup <- paste0(
      "<div style='font-family:Inter,sans-serif;min-width:230px;max-width:280px;'>",
      foto_html,
      "<div style='font-size:14px;font-weight:700;color:#111827;margin-bottom:2px;'>",d$nama,"</div>",
      "<div style='font-size:11px;color:#9CA3AF;margin-bottom:10px;'>",d$kecamatan," · ",d$kategori_grup,"</div>",
      "<div style='display:grid;grid-template-columns:1fr 1fr 1fr;gap:8px;'>",
      "<div style='text-align:center;background:#FFFBEB;border-radius:8px;padding:8px 4px;'>",
      "<div style='font-size:17px;font-weight:800;color:#D97706;font-family:JetBrains Mono;'>",d$rating,"</div>",
      "<div style='font-size:9px;color:#9CA3AF;font-weight:600;text-transform:uppercase;letter-spacing:.5px;'>Rating</div></div>",
      "<div style='text-align:center;background:#EEF2FF;border-radius:8px;padding:8px 4px;'>",
      "<div style='font-size:17px;font-weight:800;color:#4F46E5;font-family:JetBrains Mono;'>",d$jumlah_ulasan,"</div>",
      "<div style='font-size:9px;color:#9CA3AF;font-weight:600;text-transform:uppercase;letter-spacing:.5px;'>Ulasan</div></div>",
      "<div style='text-align:center;background:#ECFDF5;border-radius:8px;padding:8px 4px;'>",
      "<div style='font-size:17px;font-weight:800;color:#059669;font-family:JetBrains Mono;'>",d$skor_potensi,"</div>",
      "<div style='font-size:9px;color:#9CA3AF;font-weight:600;text-transform:uppercase;letter-spacing:.5px;'>Potensi</div></div>",
      "</div>",
      "<div style='margin-top:10px;font-size:11px;color:#6B7280;'>",d$label_kelayakan," · ",d$telepon,"</div>",
      "</div>"
    )
    
    m <- leaflet() %>% addProviderTiles(tile) %>%
      setView(lng=119.43, lat=-5.14, zoom=12) %>%
      addScaleBar(position="bottomleft", options=scaleBarOptions(imperial=FALSE))
    
    if (mode == "agg") {
      # AGREGAT PER KECAMATAN
      dk <- d %>% group_by(kecamatan) %>% 
        summarize(n=n(), lat=mean(latitude,na.rm=T), lng=mean(longitude,na.rm=T),
                  kepadatan=mean(kepadatan,na.rm=T)) %>% ungroup()
      
      # HEATMAP PRO (Efek Blok Warna)
      if (isTRUE(input$sw_heat)) {
        m <- m %>% addHeatmap(data=dk, lng=~lng, lat=~lat, intensity=~kepadatan,
                              blur=45, max=max(dk$kepadatan, na.rm=T)*1.2, radius=80,
                              gradient=c('0.2'="#F0FDFA",'0.4'="#5EEAD4",'0.6'="#2DD4BF",'0.8'="#0D9488",'1.0'="#042F2E"))
      }
      
      pal_agg <- colorNumeric(c("#EEF2FF","#818CF8","#4F46E5"), domain=dk$n)
      
      m <- m %>% addCircleMarkers(data=dk, lng=~lng, lat=~lat,
                                  radius=~rescale(n, to=c(15, 45)),
                                  color="white", fillColor=~pal_agg(n),
                                  fillOpacity=0.9, weight=3,
                                  label=~paste0(kecamatan, ": ", n, " lokasi"),
                                  popup=~paste0("<div style='padding:8px;font-family:Inter;'>",
                                                "<b style='color:#4F46E5;font-size:14px;'>",kecamatan,"</b><hr style='margin:5px 0;'>",
                                                "<b>Total Lokasi:</b> ",n,"<br>",
                                                "<b>Kepadatan:</b> ",format(round(kepadatan), big.mark=".", decimal.mark=",")," jiwa/km2</div>")) %>%
        addLegend("bottomright", pal=pal_agg, values=dk$n, title="Jumlah Lokasi", opacity=0.8)
      
    } else {
      # TAMPILAN PER TITIK USAHA (EXISTING LOGIC)
      if (isTRUE(input$sw_heat)) {
        m <- m %>% addHeatmap(data=d, lng=~longitude, lat=~latitude, intensity=~skor_potensi,
                              blur=25, max=0.08, radius=20,
                              gradient=c('0.4'="#EEF2FF", '0.6'="#818CF8", '1.0'="#4F46E5"))
      }
      
      cluster_opts <- markerClusterOptions(iconCreateFunction=JS(
        "function(c){return L.divIcon({html:'<div style=\"background:#4F46E5;color:#fff;border-radius:50%;width:36px;height:36px;display:flex;align-items:center;justify-content:center;font-size:12px;font-weight:700;box-shadow:0 2px 12px rgba(79,70,229,0.4);\">'+c.getChildCount()+'</div>',className:''})}"))
      
      if (isTRUE(input$sw_cluster)) {
        m <- m %>% addCircleMarkers(data=d, lng=~longitude, lat=~latitude,
                                    color=~pal_fn(cv), fillColor=~pal_fn(cv),
                                    fillOpacity=.88, opacity=1, radius=rad, weight=1.5,
                                    popup=popup, label=~nama, group="lokasi",
                                    clusterOptions=cluster_opts)
      } else {
        m <- m %>% addCircleMarkers(data=d, lng=~longitude, lat=~latitude,
                                    color=~pal_fn(cv), fillColor=~pal_fn(cv),
                                    fillOpacity=.88, opacity=1, radius=rad, weight=1.5, 
                                    popup=popup, label=~nama, group="lokasi")
      }
      
      if (mode!="heat") {
        m <- m %>% addLegend("bottomright", pal=pal_fn, values=cv, opacity=.9,
                             title=switch(mode,"all"="Kategori","kc"="Kecamatan","km"="K-Means","db"="DBSCAN","pop"="Kepadatan","Kategori"))
      }
    }
    
    # Tambahkan fitur Search Marker (Nama Usaha)
    m <- m %>% addSearchFeatures(
      targetGroups = "lokasi",
      options = searchFeaturesOptions(
        propertyName = "label",
        zoom=15, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
        textPlaceholder = "Cari nama usaha...",
        position = "topleft"
      )
    )
    
    # Tambahkan fitur Search Wilayah (Jalan/Daerah di Makassar)
    m <- m %>% addSearchOSM(
      options = searchOptions(
        zoom = 15, autoCollapse = TRUE,
        hideMarkerOnCollapse = TRUE,
        textPlaceholder = "Cari jalan/wilayah...",
        position = "topright"
      )
    )
    
    if (mode!="heat") {
      m <- m %>% addLegend("bottomright", pal=pal_fn, values=cv, opacity=.9,
                           title=switch(mode,"all"="Kategori","kc"="Kecamatan","km"="K-Means","db"="DBSCAN","pop"="Kepadatan","Kategori"))
    }
    if (isTRUE(input$sw_mini)) {
      m <- m %>% addMiniMap(tiles=providers$CartoDB.Positron,
                            toggleDisplay=TRUE, width=130, height=100, position="bottomright")
    }
    m
  })
  
  output$map_chips <- renderUI({
    d <- rd()
    tags$div(class="chip-wrap",
             lapply(sort(unique(d$kategori_grup)), function(g) {
               n <- sum(d$kategori_grup==g, na.rm=T)
               tags$div(class="chip",
                        tags$span(class="chip-label",g),
                        tags$span(class="chip-count",n))
             })
    )
  })
  
  # ── K-MEANS ─────────────────────────────────────────────
  output$pca_out <- renderUI({
    if(isTRUE(input$pca_3d)) plotlyOutput("p_pca_3d",height="375px")
    else                      plotlyOutput("p_pca_2d",height="375px")
  })
  
  pca_r <- reactive({
    res <- ar(); req(!is.null(res))
    pca <- prcomp(res$scaled)
    ve  <- summary(pca)$importance[2,]*100
    col <- switch(input$pca_col,
                  "cl"=paste0("C",res$data$cluster_kmeans),
                  "kc"=res$data$kecamatan,
                  "pt"=res$data$label_kelayakan)
    list(pca=pca, d=res$data, col=col, ve=ve)
  })
  
  output$p_pca_2d <- renderPlotly({
    p <- pca_r()
    plot_ly(x=p$pca$x[,1], y=p$pca$x[,2], color=p$col, colors=PAL,
            type="scatter", mode="markers",
            marker=list(size=7,opacity=.76,line=list(color="rgba(0,0,0,.08)",width=.5)),
            text=paste0("<b>",p$d$nama,"</b><br>",p$d$kecamatan,"<br>Potensi: ",p$d$skor_potensi),
            hoverinfo="text") %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title=paste0("PC1 (",round(p$ve[1],1),"%)")),
                yaxis=list(gridcolor="#F3F4F6",title=paste0("PC2 (",round(p$ve[2],1),"%)"))))
  })
  
  output$p_pca_3d <- renderPlotly({
    p <- pca_r(); req(ncol(p$pca$x)>=3)
    plot_ly(x=p$pca$x[,1],y=p$pca$x[,2],z=p$pca$x[,3],
            color=p$col,colors=PAL,type="scatter3d",mode="markers",
            marker=list(size=4,opacity=.76),
            text=paste0("<b>",p$d$nama,"</b><br>",p$d$kecamatan),hoverinfo="text") %>%
      layout(scene=list(bgcolor="rgba(0,0,0,0)",
                        xaxis=list(title="PC1",gridcolor="#F3F4F6",backgroundcolor="rgba(247,248,252,.9)",showbackground=TRUE),
                        yaxis=list(title="PC2",gridcolor="#F3F4F6",backgroundcolor="rgba(247,248,252,.9)",showbackground=TRUE),
                        zaxis=list(title="PC3",gridcolor="#F3F4F6",backgroundcolor="rgba(247,248,252,.9)",showbackground=TRUE)),
             paper_bgcolor="rgba(0,0,0,0)",
             legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#6B7280",size=10)),
             margin=list(t=0,b=0,l=0,r=0))
  })
  
  output$p_elbow <- renderPlot({
    d <- fd_sp(); req(nrow(d)>=3)
    num_cols <- c("latitude","longitude","rating","jumlah_ulasan","skor_potensi")
    d[num_cols] <- lapply(d[num_cols], as.numeric)
    d <- d[complete.cases(d[num_cols]), ]
    req(nrow(d) >= 3)
    fs <- scale(d[, num_cols])
    fs[!is.finite(fs)] <- 0
    wss <- sapply(2:10, function(k) tryCatch(kmeans(fs,k,nstart=10)$tot.withinss, error=function(e) NA_real_))
    df  <- data.frame(K=2:10, WSS=as.numeric(wss)) %>% filter(!is.na(WSS))
    req(nrow(df) > 0)
    k_val <- as.numeric(input$k_n)
    ggplot(df, aes(K, WSS)) +
      geom_area(fill="#4F46E5", alpha=.06) +
      geom_line(color="#C7D2FE", linewidth=.8) +
      geom_point(aes(color=K==k_val), size=5.5, show.legend=FALSE) +
      scale_color_manual(values=c("FALSE"="#4F46E5","TRUE"="#EF4444")) +
      geom_vline(xintercept=k_val, linetype="dashed", color="#EF4444", linewidth=.6, alpha=.7) +
      scale_x_continuous(breaks=2:10) + labs(x="K", y="Total Within SS") + gg_clean()
  })
  
  output$p_sil <- renderPlot({
    k_val <- as.numeric(input$k_n); if(is.na(k_val)) k_val <- 4
    d <- fd_sp(); req(nrow(d) >= k_val + 1)
    num_cols <- c("latitude","longitude","rating","jumlah_ulasan","skor_potensi")
    d[num_cols] <- lapply(d[num_cols], as.numeric)
    d <- d[complete.cases(d[num_cols]), ]
    req(nrow(d) >= k_val + 1)
    fs <- scale(d[, num_cols])
    fs[!is.finite(fs)] <- 0
    km  <- kmeans(fs, k_val, nstart=25)
    sil <- silhouette(km$cluster, dist(fs))
    sw_vals <- as.numeric(sil[, 3])
    df  <- data.frame(cl=factor(sil[,1]), sw=sw_vals) %>%
      arrange(cl, desc(sw)) %>% mutate(i=row_number())
    av  <- round(mean(sw_vals, na.rm=TRUE), 3)
    ggplot(df, aes(i, sw, fill=cl)) +
      geom_col(width=1, show.legend=FALSE) +
      scale_fill_manual(values=PAL[seq_len(k_val)]) +
      geom_hline(yintercept=av, linetype="dashed", color="#F59E0B", linewidth=.8) +
      annotate("text", x=nrow(df)*.6, y=av+.04,
               label=paste0("Avg = ", av), color="#F59E0B", size=3.2) +
      labs(x="", y="Silhouette Width") + gg_clean() + theme(axis.text.x=element_blank())
  })
  
  output$p_gap <- renderPlot({
    d <- fd_sp(); req(nrow(d) >= 10)
    num_cols <- c("latitude","longitude","rating","jumlah_ulasan","skor_potensi")
    d[num_cols] <- lapply(d[num_cols], as.numeric)
    d <- d[complete.cases(d[num_cols]), ]
    req(nrow(d) >= 10)
    fs <- scale(d[, num_cols])
    fs[!is.finite(fs)] <- 0
    set.seed(42)
    gs <- tryCatch(
      clusGap(fs, FUNcluster=kmeans, K.max=8, B=20, FUN.args=list(nstart=5, iter.max=20)),
      error=function(e) NULL
    )
    req(!is.null(gs))
    gap_v <- as.numeric(gs$Tab[, 3])
    se_v  <- as.numeric(gs$Tab[, 4])
    df <- data.frame(K=1:8, Gap=gap_v, SE=se_v)
    df <- df[is.finite(df$Gap), ]
    req(nrow(df) > 0)
    ko <- maxSE(gap_v, se_v)
    ggplot(df, aes(K, Gap)) +
      geom_ribbon(aes(ymin=Gap-SE, ymax=Gap+SE), fill="#8B5CF6", alpha=.1) +
      geom_line(color="#8B5CF6", linewidth=.8) +
      geom_point(aes(color=K==ko), size=5.5, show.legend=FALSE) +
      scale_color_manual(values=c("FALSE"="#8B5CF6","TRUE"="#EF4444")) +
      geom_vline(xintercept=ko, linetype="dashed", color="#EF4444", linewidth=.6, alpha=.7) +
      annotate("text", x=ko+.25, y=max(df$Gap, na.rm=TRUE)*.96,
               label=paste0("K opt = ", ko), color="#EF4444", size=3.2, hjust=0) +
      scale_x_continuous(breaks=1:8) + labs(x="K", y="Gap Statistic") + gg_clean()
  })
  
  output$p_radar <- renderPlotly({
    res <- ar(); req(!is.null(res))
    vars <- c("rating","jumlah_ulasan","skor_potensi","latitude","longitude")
    # Hitung mean per cluster terlebih dahulu, lalu rescale antar cluster
    prof_raw <- res$data %>% group_by(cluster_kmeans) %>%
      summarise(across(all_of(vars), ~mean(as.numeric(.), na.rm=TRUE), .names="{.col}"), .groups="drop")
    # Safe rescale: jika semua nilai sama, kembalikan 0.5
    safe_rescale <- function(x) {
      x <- as.numeric(x)
      if (length(x) < 2 || all(is.na(x)) || diff(range(x, na.rm=TRUE)) == 0) return(rep(0.5, length(x)))
      rescale(x, to=c(0,1))
    }
    prof <- prof_raw
    for (v in vars) prof[[v]] <- safe_rescale(prof_raw[[v]])
    cats <- c("Rating","Ulasan","Potensi","Lat","Lng","Rating")
    p <- plot_ly(type="scatterpolar",fill="toself")
    for (i in seq_len(nrow(prof))) {
      v <- as.numeric(prof[i,vars]); rgb_v <- col2rgb(PAL[i])
      p <- add_trace(p,r=c(v,v[1]),theta=cats,name=paste0("Cluster ",prof$cluster_kmeans[i]),
                     line=list(color=PAL[i],width=2),
                     fillcolor=paste0("rgba(",rgb_v[1],",",rgb_v[2],",",rgb_v[3],",.1)"))
    }
    p %>% layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,1),gridcolor="#E5E7EB",
                                            linecolor="#E5E7EB",tickfont=list(color="#D1D5DB",size=8)),
                            angularaxis=list(color="#9CA3AF",gridcolor="#E5E7EB")),
                 paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
                 font=list(family="Inter",color="#6B7280",size=11),
                 legend=list(bgcolor="rgba(0,0,0,0)",font=list(color="#6B7280",size=11)),
                 margin=list(t=20,b=20,l=30,r=30))
  })
  
  output$p_box <- renderPlotly({
    res <- ar(); req(!is.null(res))
    d <- res$data; v <- input$box_v
    plot_ly(d,x=~paste0("C",cluster_kmeans),y=~get(v),
            type="box",color=~cluster_kmeans,colors=PAL,showlegend=FALSE,
            marker=list(size=3,opacity=.5),line=list(width=1.5)) %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="Cluster"),
                yaxis=list(gridcolor="#F3F4F6",title=v)))
  })
  
  output$cl_stats <- renderTable({
    res <- ar(); req(!is.null(res))
    res$data %>% group_by(Cluster=paste0("C", cluster_kmeans)) %>%
      summarise(
        N       = n(),
        Rating  = round(mean(as.numeric(rating),        na.rm=TRUE), 2),
        Ulasan  = round(mean(as.numeric(jumlah_ulasan), na.rm=TRUE)),
        Potensi = round(mean(as.numeric(skor_potensi),  na.rm=TRUE), 1),
        .groups = "drop"
      )
  }, striped=FALSE, bordered=FALSE, spacing="xs", width="100%", align="l")
  
  output$km_cards <- renderUI({
    res <- ar()
    if (is.null(res)) return(tags$div(class="insight-box",
                                      tags$div(class="insight-label","Menunggu"),
                                      tags$div(class="insight-text","Klik \"Jalankan Analisis\" untuk memulai clustering.")))
    prof <- res$data %>% group_by(cluster_kmeans) %>%
      summarise(
        r = round(mean(as.numeric(rating),        na.rm=TRUE), 2),
        u = round(mean(as.numeric(jumlah_ulasan), na.rm=TRUE)),
        p = round(mean(as.numeric(skor_potensi),  na.rm=TRUE), 1),
        n = n(), .groups="drop"
      ) %>%
      mutate(r=ifelse(is.nan(r)|is.na(r),0,r),
             u=ifelse(is.nan(u)|is.na(u),0,u),
             p=ifelse(is.nan(p)|is.na(p),0,p)) %>%
      arrange(desc(p))
    make_card <- function(row) {
      if      (row$p>=75&&row$r>=4.2){cls="cl-green"; lbl="Sangat Strategis"; bc="#10B981"; rec="Zona premium — prioritas utama investasi usaha. Rating dan potensi tertinggi."}
      else if (row$p>=65)            {cls="cl-blue";  lbl="Strategis";        bc="#4F46E5"; rec="Zona potensial — ideal untuk usaha baru dengan diferensiasi yang tepat."}
      else if (row$p>=55)            {cls="cl-yellow";lbl="Cukup Strategis";  bc="#D97706"; rec="Zona berkembang — potensi ada, perlu strategi pemasaran yang kuat."}
      else if (row$p>=45)            {cls="cl-orange";lbl="Potensi Sedang";   bc="#EA580C"; rec="Zona selektif — pertimbangkan niche market atau segmentasi spesifik."}
      else                           {cls="cl-red";   lbl="Risiko Tinggi";    bc="#DC2626"; rec="Zona risiko — kompetisi tinggi atau daya beli rendah. Pertimbangkan ulang."}
      rgb_v <- col2rgb(bc)
      tags$div(class=paste("cl-card",cls),
               tags$div(class="cl-title", paste0("Cluster ",row$cluster_kmeans),
                        tags$span(class="cl-badge",
                                  style=paste0("background:rgba(",rgb_v[1],",",rgb_v[2],",",rgb_v[3],",.15);color:",bc,";"),
                                  lbl)),
               tags$div(class="cl-meta", paste0(row$n," lokasi  ·  Rating: ",row$r,"★  ·  Ulasan avg: ",row$u,"  ·  Skor: ",row$p,"/100")),
               tags$div(class="cl-rec", rec))
    }
    do.call(tagList, lapply(seq_len(nrow(prof)), function(i) make_card(prof[i,])))
  })
  
  # ── DBSCAN ──────────────────────────────────────────────
  output$db_scatter <- renderPlotly({
    res <- ar(); req(!is.null(res)); d <- res$data
    d$lbl <- ifelse(d$cluster_dbscan=="0","⚫ Noise",paste0("Cluster ",d$cluster_dbscan))
    plot_ly(d,x=~longitude,y=~latitude,color=~lbl,colors=c("#D1D5DB",PAL),
            type="scatter",mode="markers",
            marker=list(size=7,opacity=.8,line=list(color="rgba(0,0,0,.06)",width=.5)),
            text=~paste0("<b>",nama,"</b><br>",kecamatan,"<br>⭐",rating," | Potensi: ",skor_potensi),
            hoverinfo="text") %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="Longitude"),
                yaxis=list(gridcolor="#F3F4F6",title="Latitude")))
  })
  
  output$db_knn <- renderPlot({
    eps_val <- as.numeric(input$eps); if(is.na(eps_val)) eps_val <- 0.05
    mpts_val <- as.numeric(input$mpts); if(is.na(mpts_val)) mpts_val <- 5
    d <- fd_sp()
    d$latitude  <- as.numeric(d$latitude)
    d$longitude <- as.numeric(d$longitude)
    d <- d[is.finite(d$latitude) & is.finite(d$longitude), ]
    req(nrow(d) > mpts_val)
    cs <- scale(d[, c("latitude","longitude")])
    cs[!is.finite(cs)] <- 0
    kd <- sort(as.numeric(kNNdist(cs, k=mpts_val)))
    kd <- kd[is.finite(kd)]
    req(length(kd) > 0)
    df <- data.frame(I=seq_along(kd), D=kd)
    rng <- diff(range(kd))
    y_ann <- eps_val + ifelse(rng > 0, rng * .06, 0.01)
    ggplot(df, aes(I, D)) +
      geom_area(fill="#8B5CF6", alpha=.07) +
      geom_line(color="#8B5CF6", linewidth=.8) +
      geom_hline(yintercept=eps_val, linetype="dashed", color="#EF4444", linewidth=.8, alpha=.85) +
      annotate("text", x=nrow(df)*.06, y=y_ann,
               label=paste0("eps = ", eps_val), color="#EF4444", size=3.2, hjust=0) +
      labs(x="Data Points (diurutkan)", y=paste0(mpts_val, "-NN Distance")) + gg_clean()
  })
  
  output$db_reach <- renderPlot({
    eps_val <- as.numeric(input$eps); if(is.na(eps_val)) eps_val <- 0.05
    mpts_val <- as.numeric(input$mpts); if(is.na(mpts_val)) mpts_val <- 5
    d <- fd_sp()
    d$latitude  <- as.numeric(d$latitude)
    d$longitude <- as.numeric(d$longitude)
    d <- d[is.finite(d$latitude) & is.finite(d$longitude), ]
    req(nrow(d) > mpts_val)
    cs <- scale(d[, c("latitude","longitude")])
    cs[!is.finite(cs)] <- 0
    db    <- dbscan::dbscan(cs, eps=eps_val, minPts=mpts_val)
    reach <- dbscan::optics(cs, minPts=mpts_val)
    rd_vals <- as.numeric(reach$reachdist)
    cl_vals <- as.factor(db$cluster[reach$order])
    df_r <- data.frame(I=seq_along(rd_vals), RD=rd_vals, CL=cl_vals)
    finite_rd <- df_r$RD[is.finite(df_r$RD)]
    mx <- if (length(finite_rd) > 0) max(finite_rd, na.rm=TRUE) else 1
    df_r$RD[!is.finite(df_r$RD)] <- mx * 1.12
    ggplot(df_r, aes(I, RD, fill=CL)) +
      geom_col(width=1, show.legend=FALSE) +
      scale_fill_manual(values=c("#E5E7EB", PAL)) +
      geom_hline(yintercept=eps_val, linetype="dashed", color="#EF4444", linewidth=.8, alpha=.85) +
      labs(x="Order", y="Reachability Distance") + gg_clean() + theme(axis.text.x=element_blank())
  })
  
  output$db_bar <- renderPlotly({
    res <- ar(); req(!is.null(res))
    d <- res$data %>% group_by(cluster_dbscan) %>% summarise(N=n(),.groups="drop") %>%
      mutate(L=ifelse(cluster_dbscan=="0","Noise",paste0("C",cluster_dbscan)),
             no=cluster_dbscan=="0")
    plot_ly(d,x=~L,y=~N,type="bar",
            marker=list(color=ifelse(d$no,"#E5E7EB","#4F46E5"),opacity=.88),
            text=~N,textposition="outside",textfont=list(color="#9CA3AF",size=12)) %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="Cluster"),
                yaxis=list(gridcolor="#F3F4F6",title="Jumlah")))
  })
  
  output$db_heat <- renderPlotly({
    res <- ar(); req(!is.null(res))
    d <- res$data %>% filter(cluster_dbscan!="0") %>%
      group_by(cluster_dbscan,kategori_grup) %>% summarise(N=n(),.groups="drop")
    plot_ly(d,x=~kategori_grup,y=~paste0("C",cluster_dbscan),z=~N,type="heatmap",
            colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
            text=~N,texttemplate="%{text}",textfont=list(color="#fff",size=11)) %>%
      layout(mk(margin=list(t=5,b=92,l=60,r=12),
                xaxis=list(gridcolor="#F3F4F6",title="",tickangle=-45,tickfont=list(color="#9CA3AF",size=9.5)),
                yaxis=list(gridcolor="#F3F4F6",title="Cluster")))
  })
  
  output$db_noise <- renderUI({
    res <- ar(); req(!is.null(res)); d <- res$data
    nn <- sum(d$cluster_dbscan=="0"); nc <- length(unique(d$cluster_dbscan[d$cluster_dbscan!="0"]))
    pct <- round(nn/nrow(d)*100,1)
    tags$div(
      tags$div(class="stat-card red",
               tags$div(class="stat-label","Noise Points"),
               tags$div(class="stat-value",nn),
               tags$div(class="stat-sub",paste0(pct,"% dari data"))),
      tags$div(class="stat-card brand",
               tags$div(class="stat-label","Cluster Terdeteksi"),
               tags$div(class="stat-value",nc),
               tags$div(class="stat-sub",paste0("eps=",input$eps," · minPts=",input$mpts))),
      tags$div(class="stat-card green",
               tags$div(class="stat-label","Core Points"),
               tags$div(class="stat-value",nrow(d)-nn),
               tags$div(class="stat-sub","Termasuk dalam cluster"))
    )
  })
  
  # ── KOMPARASI ───────────────────────────────────────────
  output$p_conf <- renderPlotly({
    res <- ar(); req(!is.null(res)); d <- res$data
    req("cluster_kmeans"%in%names(d),"cluster_dbscan"%in%names(d))
    ct <- as.data.frame(table(KM=d$cluster_kmeans,DB=d$cluster_dbscan))
    plot_ly(ct,x=~DB,y=~KM,z=~Freq,type="heatmap",
            colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
            text=~Freq,texttemplate="%{text}",textfont=list(color="#fff",size=13)) %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="DBSCAN"),
                yaxis=list(gridcolor="#F3F4F6",title="K-Means")))
  })
  
  output$p_metrics <- renderUI({
    res <- ar(); req(!is.null(res))
    d <- res$data; fs <- res$scaled; km <- res$km
    sil_vals <- tryCatch(as.numeric(silhouette(km$cluster, dist(fs))[, 3]), error=function(e) NA_real_)
    sk  <- round(mean(sil_vals, na.rm=TRUE), 4)
    sk  <- ifelse(is.nan(sk) | is.na(sk), 0, sk)
    nc  <- length(unique(d$cluster_dbscan[d$cluster_dbscan != "0"]))
    nn  <- sum(d$cluster_dbscan == "0")
    pct <- ifelse(nrow(d) > 0, round((nrow(d)-nn)/nrow(d)*100, 1), 0)
    tags$div(
      tags$div(class="stat-card brand",
               tags$div(class="stat-label","K-Means Silhouette"),
               tags$div(class="stat-value", sk),
               tags$div(class="stat-sub", if(sk>=.5)"✅ Struktur kuat" else if(sk>=.25)"⚠️ Sedang" else "❌ Lemah")),
      tags$div(class="stat-card violet",
               tags$div(class="stat-label","DBSCAN Clusters"),
               tags$div(class="stat-value", nc),
               tags$div(class="stat-sub", paste0(nn, " noise · eps=", input$eps))),
      tags$div(class="stat-card green",
               tags$div(class="stat-label","Data Tercluster"),
               tags$div(class="stat-value", paste0(pct, "%")),
               tags$div(class="stat-sub", paste0(nrow(d)-nn, " dari ", nrow(d)))),
      tags$div(class="stat-card amber",
               tags$div(class="stat-label","Inertia K-Means"),
               tags$div(class="stat-value", round(as.numeric(km$tot.withinss), 0)),
               tags$div(class="stat-sub","Within-cluster SS"))
    )
  })
  
  output$p_parallel <- renderPlotly({
    res <- ar(); req(!is.null(res)); d <- res$data
    vp <- c("rating","jumlah_ulasan","skor_potensi","latitude","longitude")
    # Safe rescale per column
    safe_rs <- function(x) {
      x <- as.numeric(x)
      x[!is.finite(x)] <- NA
      if (all(is.na(x))) return(rep(0, length(x)))
      mn <- min(x, na.rm=TRUE); mx <- max(x, na.rm=TRUE)
      if (mx == mn) return(rep(0.5, length(x)))
      (x - mn) / (mx - mn)
    }
    dn <- d
    for (v in vp) dn[[v]] <- safe_rs(d[[v]])
    k_val <- as.numeric(input$k_n); if(is.na(k_val)) k_val <- 4
    k  <- as.integer(k_val)
    cs <- lapply(seq(0, 1, length.out=k), function(i) list(i, PAL[max(1, ceiling(i*k))]))
    plot_ly(type="parcoords",
            line=list(color=as.numeric(d$cluster_kmeans), colorscale=cs, showscale=FALSE),
            dimensions=list(
              list(label="Rating",    values=dn$rating,        range=c(0,1)),
              list(label="Ulasan",    values=dn$jumlah_ulasan, range=c(0,1)),
              list(label="Potensi",   values=dn$skor_potensi,  range=c(0,1)),
              list(label="Latitude",  values=dn$latitude,      range=c(0,1)),
              list(label="Longitude", values=dn$longitude,     range=c(0,1))
            )) %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",
             font=list(family="Inter", color="#6B7280", size=11),
             margin=list(t=32, b=32, l=82, r=82))
  })
  
  # ── EKSPLORASI DATA (1-2-2 Layout) ──────────────────────
  output$ex_total_biz <- renderUI({
    tags$div(class="stat-card brand",
             tags$div(class="stat-label","Total Entitas Bisnis"),
             tags$div(class="stat-value",nrow(data_usaha)),
             tags$div(class="stat-sub","Terverifikasi di Makassar"))
  })
  output$ex_avg_rating <- renderUI({
    tags$div(class="stat-card emerald",
             tags$div(class="stat-label","Rata-rata Rating"),
             tags$div(class="stat-value",round(mean(data_usaha$rating,na.rm=T),1)),
             tags$div(class="stat-sub","Kepuasan Pelanggan"))
  })
  output$ex_top_kec <- renderUI({
    top_k <- names(sort(table(data_usaha$kecamatan),decreasing=T)[1])
    tags$div(class="stat-card violet",
             tags$div(class="stat-label","Kecamatan Teraktif"),
             tags$div(class="stat-value",style="font-size:18px;",top_k),
             tags$div(class="stat-sub","Pusat Ekonomi"))
  })
  output$ex_top_kat <- renderUI({
    top_c <- names(sort(table(data_usaha$kategori_grup),decreasing=T)[1])
    tags$div(class="stat-card yellow",
             tags$div(class="stat-label","Kategori Dominan"),
             tags$div(class="stat-value",style="font-size:18px;",top_c),
             tags$div(class="stat-sub","Peluang Terbesar"))
  })

  output$ex_rating_chart <- renderPlotly({
    plot_ly(data_usaha %>% filter(!is.na(rating)), x=~rating, type="histogram", nbinsx=10,
            marker=list(color="#4F46E5", line=list(color="#fff", width=1))) %>%
      layout(mk(xaxis=list(title="Rating Bintang"), yaxis=list(title="Jumlah Usaha")))
  })

  output$ex_pop_chart <- renderPlotly({
    d <- data_usaha %>% group_by(popularitas) %>% summarise(n=n())
    plot_ly(d, labels=~popularitas, values=~n, type="pie",
            textinfo="label+percent", hole=0.4,
            marker=list(colors=c("#10B981","#3B82F6","#F59E0B","#EF4444"))) %>%
      layout(mk(showlegend=TRUE, legend=list(orientation="h", x=0, y=-0.2)))
  })

  output$ex_kep_chart <- renderPlotly({
    d <- data_usaha %>% group_by(kecamatan) %>% summarise(kep=mean(kepadatan,na.rm=T)) %>% arrange(desc(kep))
    plot_ly(d, x=~kep, y=~reorder(kecamatan,kep), type="bar", orientation="h",
            marker=list(color="#10B981")) %>%
      layout(mk(xaxis=list(title="Kepadatan (jiwa/km²)"), yaxis=list(title="")))
  })

  output$ex_pot_chart <- renderPlotly({
    d <- data_usaha %>% group_by(kecamatan) %>% summarise(score=mean(skor_potensi, na.rm=T)) %>% arrange(desc(score))
    plot_ly(d, x=~reorder(kecamatan,-score), y=~score, type="bar",
            marker=list(color=~score, colorscale="Viridis", showscale=FALSE)) %>%
      layout(mk(xaxis=list(title="", tickangle=-45), yaxis=list(title="Skor Potensi Avg")))
  })
  output$p_corr <- renderPlotly({
    num_cols <- c("rating","jumlah_ulasan","skor_potensi","kepadatan","latitude","longitude")
    d <- fd()[, num_cols]
    d[] <- lapply(d, as.numeric)        # forcer semua jadi numeric
    d <- d[complete.cases(d), ]
    req(nrow(d) >= 2)
    cm <- tryCatch(cor(d, use="complete.obs"), error=function(e) NULL)
    req(!is.null(cm))
    cm[!is.finite(cm)] <- 0
    cn <- c("Rating","Ulasan","Potensi","Kepadatan","Lat","Lng")
    colnames(cm) <- rownames(cm) <- cn
    plot_ly(z=cm, x=cn, y=cn, type="heatmap",
            colorscale=list(c(0,"#EF4444"),c(.5,"#F9FAFB"),c(1,"#4F46E5")), zmin=-1, zmax=1,
            text=round(cm,2), texttemplate="%{text}", textfont=list(color="#111827",size=12)) %>%
      layout(mk(margin=list(t=5,b=65,l=78,r=12),
                xaxis=list(gridcolor="#F3F4F6",title="",tickangle=-30),
                yaxis=list(gridcolor="#F3F4F6",title="")))
  })
  output$p_bubble <- renderPlotly({
    d <- fd() %>% group_by(kecamatan) %>%
      summarise(
        ar  = mean(as.numeric(rating),        na.rm=TRUE),
        ap  = mean(as.numeric(jumlah_ulasan),  na.rm=TRUE),
        apt = mean(as.numeric(skor_potensi),   na.rm=TRUE),
        n   = n(), .groups="drop"
      ) %>%
      mutate(across(c(ar,ap,apt), ~ifelse(is.nan(.)|is.na(.), 0, .)))
    plot_ly(d, x=~ar, y=~ap, size=~n, color=~apt,
            text=~paste0("<b>",kecamatan,"</b><br>N=",n,"<br>Potensi=",round(apt,1)),
            hoverinfo="text", type="scatter", mode="markers+text",
            textposition="top center", textfont=list(color="#9CA3AF",size=10.5),
            marker=list(sizemode="diameter", sizeref=.26, opacity=.82,
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
                        showscale=TRUE, colorbar=list(bgcolor="#fff",bordercolor="#E5E7EB",
                                                     thickness=10,tickfont=list(color="#9CA3AF",size=9)))) %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="Avg Rating"),
                yaxis=list(gridcolor="#F3F4F6",title="Avg Ulasan")))
  })
  output$p_rank <- renderPlotly({
    # Safe rescale: handle single value or all-same case
    safe_rs <- function(x) {
      x <- as.numeric(x)
      x[!is.finite(x)] <- NA
      if (all(is.na(x))) return(rep(0, length(x)))
      mn <- min(x, na.rm=TRUE); mx <- max(x, na.rm=TRUE)
      if (mx == mn) return(rep(0.5, length(x)))
      (x - mn) / (mx - mn)
    }
    d <- fd() %>% group_by(kecamatan) %>%
      summarise(
        rating  = mean(as.numeric(rating),        na.rm=TRUE),
        ulasan  = mean(as.numeric(jumlah_ulasan),  na.rm=TRUE),
        potensi = mean(as.numeric(skor_potensi),   na.rm=TRUE),
        n = n(), .groups="drop"
      ) %>%
      mutate(
        across(c(rating,ulasan,potensi), ~ifelse(is.nan(.)|is.na(.), 0, .)),
        rating  = safe_rs(rating),
        ulasan  = safe_rs(ulasan),
        potensi = safe_rs(potensi)
      ) %>%
      pivot_longer(c(rating,ulasan,potensi), names_to="m", values_to="v")
    plot_ly(d, x=~v, y=~kecamatan, color=~m,
            colors=c("#4F46E5","#10B981","#F59E0B"),
            type="bar", orientation="h") %>%
      layout(mk(margin=list(t=5,b=32,l=88,r=12),
                barmode="group",
                xaxis=list(gridcolor="#F3F4F6",title="Normalized 0–1"),
                yaxis=list(gridcolor="#F3F4F6",title="",tickfont=list(color="#6B7280",size=10.5))))
  })
  
  # ── SIMULASI & CUSTOM ANALYSIS ─────────────────────────
  selected_loc <- reactiveVal(NULL)
  
  output$sim_input_ui <- renderUI({
    if (input$sim_mode == "auto") {
      tagList(
        selectInput("sim_jenis","Jenis Usaha",choices=JENIS_USAHA,selected="cafe"),
        pickerInput("sim_kec","Kecamatan Target",choices=c("Semua",KECS),selected="Semua",
                    options=pickerOptions(liveSearch=TRUE)),
        sliderInput("sim_n","Tampilkan Top N",min=5,max=20,value=10,step=1)
      )
    } else {
      tagList(
        selectInput("sim_jenis","Jenis Usaha",choices=JENIS_USAHA,selected="cafe"),
        tags$div(style="display:flex;gap:10px;",
                 numericInput("cust_lat", "Latitude",  value=-5.14, step=0.001),
                 numericInput("cust_lon", "Longitude", value=119.43, step=0.001)
        ),
        sliderInput("sim_radius", "Radius Analisis (km)", min=0.5, max=5, value=1.5, step=0.5),
        tags$div(class="note-box", style="padding:10px; border-left:4px solid #4F46E5; background:rgba(79,70,229,0.05);",
                 tags$div(style="display:flex; align-items:center; gap:8px;",
                          tags$i(class="fa fa-info-circle", style="color:#4F46E5;"),
                          tags$span(style="font-size:12px; color:#374151; font-weight:500;", 
                                    "Klik langsung pada peta untuk menentukan lokasi Anda secara presisi.")
                 )
        )
      )
    }
  })
  
  observeEvent(input$sim_map_click, {
    if (input$sim_mode == "custom") {
      lat <- round(input$sim_map_click$lat, 5)
      lng <- round(input$sim_map_click$lng, 5)
      selected_loc(list(lat=lat, lng=lng))
      updateNumericInput(session, "cust_lat", value=lat)
      updateNumericInput(session, "cust_lon", value=lng)
      
      # Tampilkan Pin Biru + Radius seketika
      leafletProxy("sim_map") %>%
        clearGroup("custom_marker") %>%
        addCircles(lng=lng, lat=lat, radius=input$sim_radius * 1000, 
                   color="#4F46E5", weight=1, fillOpacity=0.1, group="custom_marker") %>%
        addMarkers(lng=lng, lat=lat, group="custom_marker",
                   label="Lokasi Pilihan Anda")
    }
  })
  
  # Update marker & radius if inputs change
  observeEvent(list(input$cust_lat, input$cust_lon, input$sim_radius), {
    req(input$sim_mode == "custom", input$cust_lat, input$cust_lon)
    leafletProxy("sim_map") %>%
      clearGroup("custom_marker") %>%
      addCircles(lng=input$cust_lon, lat=input$cust_lat, radius=input$sim_radius * 1000, 
                 color="#4F46E5", weight=1, fillOpacity=0.1, group="custom_marker") %>%
      addMarkers(lng=input$cust_lon, lat=input$cust_lat, group="custom_marker",
                 label="Lokasi Pilihan Anda")
  })

  sim_r <- eventReactive(input$run_sim, {
    req(input$sim_jenis, input$sim_budget)
    jenis <- input$sim_jenis; kat <- JENIS_KAT[[jenis]]
    if (is.null(kat)) {
      showNotification("Kategori tidak ditemukan.", type="error")
      return(NULL)
    }
    budget_val <- as.numeric(input$sim_budget)
    if (is.na(budget_val) || budget_val <= 0) {
      showNotification("Silakan masukkan modal yang valid.", type="warning")
      return(NULL)
    }
    if (is.null(kat)) {
      showNotification("Kategori tidak ditemukan.", type="error")
      return(NULL)
    }
    
    if (input$sim_mode == "auto") {
      if (!is.null(input$sim_kec) && input$sim_kec!="Semua") d <- d %>% filter(kecamatan==input$sim_kec)
      
      # Improved vectorized matching
      kat_regex <- paste0("\\b(", paste(kat, collapse="|"), ")\\b")
      
      d_k <- data_usaha_spatial %>% 
        filter(grepl(kat_regex, kategori_grup, ignore.case=TRUE))
      
      kec <- data_usaha_spatial %>% group_by(kecamatan) %>%
        summarise(
          n   = n(),
          ar  = mean(as.numeric(rating),      na.rm=TRUE),
          au  = mean(as.numeric(jumlah_ulasan),na.rm=TRUE),
          ap  = mean(as.numeric(skor_potensi), na.rm=TRUE),
          nk  = sum(grepl(kat_regex, kategori_grup, ignore.case=TRUE), na.rm=TRUE),
          kep = mean(as.numeric(kepadatan),    na.rm=TRUE),
          .groups="drop"
        ) %>%
        mutate(
          ar  = ifelse(is.nan(ar)  | is.na(ar),  0, ar),
          ap  = ifelse(is.nan(ap)  | is.na(ap),  0, ap),
          kep = ifelse(is.nan(kep) | is.na(kep), 0, kep),
          nk  = ifelse(is.na(nk), 0, nk),
          skor = round(
            (ap/100)*40 + (kep/13000)*25 + (ar/5)*20 + pmax(0,(50-nk)/50)*15,
            1
          ),
          label = case_when(
            skor>=75 ~ "Sangat Direkomendasikan",
            skor>=60 ~ "Direkomendasikan",
            skor>=45 ~ "Cukup Baik",
            TRUE     ~ "Kurang"
          )
        ) %>% arrange(desc(skor))
      
      if (nrow(kec) == 0) {
        showNotification("Tidak ada data kecamatan untuk simulasi ini.", type="warning")
        return(NULL)
      }
      
      if (!is.null(input$sim_kec) && input$sim_kec!="Semua") {
        kec <- kec %>% filter(kecamatan==input$sim_kec)
        top <- d %>% arrange(desc(skor_potensi)) %>% head(input$sim_n)
      } else {
        top_kec <- kec$kecamatan[seq_len(min(5,nrow(kec)))]
        top <- data_usaha_spatial %>% filter(kecamatan%in%top_kec) %>%
          arrange(desc(skor_potensi)) %>% head(input$sim_n)
      }
    } else {
      # CUSTOM LOCATION MODE
      clat <- input$cust_lat; clon <- input$cust_lon; crad <- input$sim_radius
      req(clat, clon, crad)
      
      kat_regex <- paste0("\\b(", paste(kat, collapse="|"), ")\\b")
      
      # Filter kompetitor hanya dalam radius
      dist_dk <- sqrt((data_usaha_spatial$latitude - clat)^2 + (data_usaha_spatial$longitude - clon)^2) * 111.32
      d_k <- data_usaha_spatial %>% 
        filter(grepl(kat_regex, kategori_grup, ignore.case=TRUE), dist_dk <= crad)
      
      # Cari kecamatan terdekat & data lokal
      dist_all <- sqrt((data_usaha_spatial$latitude - clat)^2 + (data_usaha_spatial$longitude - clon)^2)
      nearest_idx <- which.min(dist_all)
      target_kec <- data_usaha_spatial$kecamatan[nearest_idx]
      target_kep <- data_usaha_spatial$kepadatan[nearest_idx]
      
      # Hitung skor kustom berdasarkan jumlah kompetitor d_k dalam radius
      nk_count <- nrow(d_k)
      skor_val <- round(pmax(10, 80 - (nk_count * 5) + (target_kep/13000)*20), 1)
      
      top <- data.frame(
        nama           = "Lokasi Anda",
        kecamatan      = target_kec,
        kategori_grup  = input$sim_jenis,
        latitude       = clat,
        longitude      = clon,
        rating         = 0,
        jumlah_ulasan  = 0,
        skor_potensi   = skor_val,
        label_kelayakan = ifelse(skor_val > 70, "Sangat Potensial", "Perlu Pertimbangan"),
        kepadatan      = target_kep,
        foto_url       = "",
        stringsAsFactors = FALSE
      )
      
      kec <- data.frame(kecamatan=target_kec, skor=skor_val, nk=nk_count, ap=skor_val, stringsAsFactors=FALSE)
    }
    
    pb     <- PROFIT[[jenis]]
    modal  <- as.numeric(pb$modal)
    br     <- min(1, budget_val / max(1, modal))
    avg_p  <- mean(as.numeric(kec$ap), na.rm=TRUE)
    avg_p  <- ifelse(is.nan(avg_p) | is.na(avg_p), 50, avg_p)
    e_min  <- round(as.numeric(pb$min) * br * avg_p / 72, 1)
    e_max  <- round(as.numeric(pb$max) * br * avg_p / 72, 1)
    e_min  <- ifelse(is.nan(e_min) | is.na(e_min), 0, e_min)
    e_max  <- ifelse(is.nan(e_max) | is.na(e_max), 0, e_max)
    roi    <- round(modal / max(0.1, (e_min + e_max) / 2), 1)
    
    # Ambil SEMUA kompetitor kategori ini untuk hitung jarak akurat (bukan cuma yg dlm radius)
    d_k_all <- data_usaha_spatial %>% filter(grepl(kat_regex, kategori_grup, ignore.case=TRUE))
    
    # Hitung jarak ke kompetitor terdekat + ambil koordinatnya
    if (nrow(top) > 0 && nrow(d_k_all) > 0) {
      res_dist <- lapply(1:nrow(top), function(i) {
        dist_vec <- sqrt((d_k_all$latitude - top$latitude[i])^2 + (d_k_all$longitude - top$longitude[i])^2) * 111.32
        # Hindari jarak 0 (lokasi itu sendiri)
        idx_valid <- which(dist_vec > 0.002) # Toleransi 2 meter
        if (length(idx_valid) == 0) return(list(d=NA, lat=NA, lon=NA))
        
        idx_min <- idx_valid[which.min(dist_vec[idx_valid])]
        list(d = dist_vec[idx_min], lat = d_k_all$latitude[idx_min], lon = d_k_all$longitude[idx_min])
      })
      top$jarak_km <- sapply(res_dist, `[[`, "d")
      top$near_lat <- sapply(res_dist, `[[`, "lat")
      top$near_lon <- sapply(res_dist, `[[`, "lon")
    } else {
      top$jarak_km <- NA; top$near_lat <- NA; top$near_lon <- NA
    }
    
    list(kec=kec, top=top, d_k=d_k, e_min=e_min, e_max=e_max, roi=roi, pb=pb)
  })
  
  output$sim_loc_info <- renderUI({
    if (input$run_sim == 0) return(tags$p(style="color:#9CA3AF;","Menunggu analisis..."))
    r <- sim_r(); req(r)
    if (input$sim_mode == "auto") {
      tags$p("Mode Rekomendasi otomatis mencari lokasi terbaik berdasarkan data historis.")
    } else {
      tags$div(
        tags$p(tags$b("Kecamatan Terdeteksi: "), r$top$kecamatan[1]),
        tags$p(tags$b("Kepadatan Penduduk: "), format(r$top$kepadatan[1], big.mark=".", decimal.mark=","), " jiwa/km²"),
        tags$p(style="font-size:11px;color:#6B7280;", "Analisis dilakukan pada radius terdekat dari koordinat yang Anda tentukan.")
      )
    }
  })

  output$sim_near_list <- renderUI({
    if (input$run_sim == 0) return(NULL)
    r <- sim_r(); d_k <- r$d_k; d_top <- r$top; req(nrow(d_top)>0)
    
    # Ambil kompetitor terdekat dari lokasi pilihan (baris pertama top)
    target <- d_top[1,]
    dist_v <- sqrt((d_k$latitude - target$latitude)^2 + (d_k$longitude - target$longitude)^2) * 111.32
    d_k$d <- dist_v
    near <- d_k %>% arrange(d) %>% head(5)
    
    if (nrow(near) == 0) return(tags$p(style="color:#9CA3AF;font-size:11px;","Tidak ada kompetitor sejenis dalam radius ini."))
    
    tags$div(style="margin-top:10px;",
             lapply(seq_len(nrow(near)), function(i){
               tags$div(style="display:flex; justify-content:space-between; font-size:12px; margin-bottom:6px; border-bottom:1px dashed #E5E7EB; padding-bottom:4px;",
                        tags$span(style="font-weight:600; color:#374151;", paste0(i, ". ", substr(near$nama[i],1,25))),
                        tags$span(style="color:#EF4444; font-weight:700;", paste0(round(near$d[i],2), " km"))
               )
             })
    )
  })

  output$sim_sum <- renderUI({
    if (input$run_sim == 0) return(NULL)
    r <- sim_r(); req(r)
    kb <- r$kec[1,]
    tags$div(
      tags$div(class="stat-card green",
               tags$div(class="stat-label","Kecamatan Terbaik"),
               tags$div(class="stat-value",style="font-size:20px;",kb$kecamatan),
               tags$div(class="stat-sub",paste0("Skor: ",kb$skor," · Kompetitor: ",kb$nk))),
      tags$div(class="stat-card amber",
               tags$div(class="stat-label","Total Kompetitor Sejenis"),
               tags$div(class="stat-value",nrow(r$d_k)),
               tags$div(class="stat-sub","Di seluruh Makassar"))
    )
  })
  
  output$sim_profit <- renderUI({
    if (input$run_sim == 0) return(tags$p(style="color:#9CA3AF;","Silakan jalankan analisis untuk melihat estimasi."))
    r <- sim_r(); req(r)
    pct <- min(100,round((r$e_max/r$pb$max)*100))
    tags$div(
      tags$div(class="sim-card",
               tags$div(class="stat-label","Estimasi Pendapatan / Bulan"),
               tags$div(class="sim-profit",paste0("Rp ",r$e_min," – ",r$e_max," Jt")),
               tags$div(class="stat-sub",paste0("Modal: Rp ",r$pb$modal," Jt · ROI: ±",r$roi," bulan"))),
      tags$div(class="sim-card",
               tags$div(class="stat-label","Kelayakan Investasi"),
               tags$div(class="profit-bar", tags$div(class="profit-fill",style=paste0("width:",pct,"%"))),
               tags$div(class="stat-sub",paste0(pct,"% dari potensi maksimal kategori ini")))
    )
  })
  
  output$sim_chart <- renderPlotly({
    r <- sim_r(); ks <- r$kec %>% head(8); pb <- r$pb
    ks$est <- round(pb$min+(pb$max-pb$min)*(ks$skor/100),1)
    plot_ly(ks,x=~kecamatan,y=~est,type="bar",
            marker=list(color=~skor,
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#10B981")),
                        showscale=FALSE,opacity=.9),
            text=~paste0("Rp ",est," Jt"),
            textposition="outside",textfont=list(color="#9CA3AF",size=10)) %>%
      layout(mk(margin=list(t=5,b=68,l=52,r=12),
                xaxis=list(gridcolor="#F3F4F6",title="",tickangle=-38),
                yaxis=list(gridcolor="#F3F4F6",title="Est. Profit (Jt)")))
  })
  
  output$sim_dist_stats <- renderUI({
    if (input$run_sim == 0) return(NULL)
    r <- sim_r(); req(r)
    tags$div(
      tags$div(class="stat-card violet",
               tags$div(class="stat-label","Jarak Rata-rata ke Kompetitor"),
               tags$div(class="stat-value", paste0(round(mean(r$top$jarak_km, na.rm=T), 2), " km")),
               tags$div(class="stat-sub","Dari 10 lokasi teratas")),
      tags$div(class="stat-card orange",
               tags$div(class="stat-label","Lokasi Paling Terisolasi"),
               tags$div(class="stat-value", paste0(round(max(r$top$jarak_km, na.rm=T), 2), " km")),
               tags$div(class="stat-sub","Peluang monopoli pasar lebih besar"))
    )
  })

  output$sim_map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=119.43, lat=-5.14, zoom=12) %>%
      addLayersControl(overlayGroups = c("Heatmap Kompetisi", "Kompetitor"), options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Heatmap Kompetisi")
  })
  
  observe({
    r <- sim_r(); req(r)
    d <- r$top; d_k <- r$d_k
    pf <- colorNumeric(c("#EEF2FF","#818CF8","#4F46E5"), domain=d$skor_potensi)
    
    proxy <- leafletProxy("sim_map") %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    # Heatmap Kompetisi (Latar belakang)
    if (nrow(d_k) > 5) {
      proxy <- proxy %>% addHeatmap(data=d_k, lng=~longitude, lat=~latitude,
                                    blur=20, max=0.05, radius=15, group="Heatmap Kompetisi")
    }
    
    # Tambahkan Kompetitor Terdeteksi
    if (nrow(d_k) > 0) {
      proxy <- proxy %>% addCircleMarkers(data=d_k, lng=~longitude, lat=~latitude,
                                          radius=4, color="#9CA3AF", stroke=TRUE, weight=1, fillOpacity=0.7,
                                          label=~nama, 
                                          popup=~paste0("<div style='font-family:Inter;min-width:180px;'>",
                                                       ifelse(!is.na(foto_url) & foto_url != "", paste0("<img src='",foto_url,"' style='width:100%;height:100px;object-fit:cover;border-radius:6px;margin-bottom:8px;'>"), ""),
                                                       "<b style='color:#6B7280;font-size:13px;'>",nama,"</b><br>",
                                                       "<span style='color:#9CA3AF;font-size:11px;'>",kategori_grup,"</span><br><br>",
                                                       "⭐ ",rating," · 💬 ",jumlah_ulasan,"<br>",
                                                       "<span style='font-size:10px;color:#9CA3AF;'>",alamat,"</span></div>"),
                                          group="Kompetitor")
    }
    
    # Tambahkan Garis Merah ke Kompetitor Terdekat
    for(i in 1:nrow(d)) {
      if(!is.na(d$near_lat[i]) && !is.na(d$near_lon[i])) {
        proxy <- proxy %>% addPolylines(lng=c(d$longitude[i], d$near_lon[i]),
                                        lat=c(d$latitude[i], d$near_lat[i]),
                                        color="#EF4444", weight=1.5, dashArray="5, 8", opacity=0.6,
                                        label=paste0("Jarak: ", round(d$jarak_km[i], 2), " km"))
      }
    }
    
    # Tambahkan Rekomendasi Lokasi Utama
    proxy %>% addCircleMarkers(data=d, lng=~longitude, lat=~latitude,
                       color=~pf(skor_potensi), fillColor=~pf(skor_potensi),
                       fillOpacity=.9, opacity=1, radius=~rescale(skor_potensi,to=c(7,15)), weight=2,
                       popup=~paste0("<div style='font-family:Inter;min-width:200px;'>",
                                     ifelse(!is.na(foto_url) & foto_url != "", paste0("<img src='",foto_url,"' style='width:100%;height:120px;object-fit:cover;border-radius:8px;margin-bottom:10px;'>"), ""),
                                     "<b style='color:#4F46E5;font-size:14px;'>",nama,"</b><br>",
                                     "<span style='color:#9CA3AF;font-size:11px;'>",kecamatan," · ",kategori_grup,"</span><br><br>",
                                     "Skor: <b style='color:#4F46E5;'>",skor_potensi,"/100</b><br>",
                                     "📍 Jarak Kompetitor: <b style='color:#EF4444;'>",
                                     ifelse(is.na(jarak_km), "Tidak Terdeteksi", paste0(round(jarak_km,2)," km")), "</b><br>",
                                     "⭐ ",rating," · 💬 ",jumlah_ulasan,"<br>",label_kelayakan,"</div>"),
                       label=~nama)
    
    # Legend hanya jika ada data yang bervariasi
    if (nrow(d) > 0 && diff(range(d$skor_potensi, na.rm=T)) >= 0) {
      proxy %>% addLegend("bottomright", pal=pf, values=d$skor_potensi, title="Skor Potensi", opacity=.85, layerId="sim_legend")
    }
  })
  
  output$sim_rank <- renderUI({
    r <- sim_r(); d <- r$top; req(nrow(d)>0)
    tags$div(style="columns:2;column-gap:18px;",
             lapply(seq_len(nrow(d)),function(i){
               row <- d[i,]; s <- row$skor_potensi
               col <- if(s>=80)"#10B981" else if(s>=65)"#4F46E5" else if(s>=50)"#D97706" else "#EF4444"
               tags$div(class="rank-item",style="break-inside:avoid;",
                        tags$div(class="rank-num",style=paste0("color:",col),paste0("#",i)),
                        tags$div(class="rank-info",
                                 tags$div(class="rank-name",substr(row$nama,1,38)),
                                                                   tags$div(class="rank-meta",paste0(row$kecamatan," · ",row$kategori_grup," · 📍 ",round(row$jarak_km,2)," km"))),

                        tags$div(class="rank-score",style=paste0("color:",col),s))
             })
    )
  })
  
  # ── RANKING ─────────────────────────────────────────────
  rk_d <- reactive({
    d <- data_usaha_spatial
    if(!is.null(input$rk_kat)&&input$rk_kat!="Semua") d <- d %>% filter(kategori_grup==input$rk_kat)
    if(!is.null(input$rk_kec)&&input$rk_kec!="Semua") d <- d %>% filter(kecamatan==input$rk_kec)
    sc <- input$rk_sort%||%"skor_potensi"
    d %>% arrange(desc(get(sc))) %>% head(input$rk_n)
  })
  
  output$rk_list <- renderUI({
    d <- rk_d(); sc <- input$rk_sort%||%"skor_potensi"
    tags$div(style="max-height:520px;overflow-y:auto;",
             lapply(seq_len(nrow(d)),function(i){
               r <- d[i,]; v <- r[[sc]]; mx <- max(d[[sc]],na.rm=T)
               col <- if(v>=mx*.85)"#10B981" else if(v>=mx*.65)"#4F46E5" else "#F59E0B"
               med <- if(i==1)"🥇" else if(i==2)"🥈" else if(i==3)"🥉" else paste0("#",i)
               tags$div(class="rank-item",
                        tags$div(class="rank-num",style=paste0("color:",col,";font-size:", ifelse(i <= 3, "20px", "15px")), med),
                        tags$div(class="rank-info",
                                 tags$div(class="rank-name",substr(r$nama,1,40)),
                                 tags$div(class="rank-meta",paste0(r$kecamatan," · ",r$kategori_grup," · ⭐",r$rating," · 💬",r$jumlah_ulasan))),
                        tags$div(style="text-align:right;flex-shrink:0;",
                                 tags$div(class="rank-score",style=paste0("color:",col),round(v,1)),
                                 tags$div(style="font-size:9.5px;color:#9CA3AF;",r$label_kelayakan))
               )
             })
    )
  })
  
  output$rk_chart <- renderPlotly({
    d <- rk_d(); sc <- input$rk_sort%||%"skor_potensi"
    d$nm <- paste0(substr(d$nama,1,30),ifelse(nchar(d$nama)>30,"…",""))
    plot_ly(d,x=~get(sc),y=~reorder(nm,get(sc)),type="bar",orientation="h",
            marker=list(color=~get(sc),
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
                        showscale=FALSE,opacity=.9),
            text=~paste0(kecamatan," · ⭐",rating),
            textposition="outside",textfont=list(color="#9CA3AF",size=10)) %>%
      layout(mk(margin=list(t=5,b=22,l=225,r=80),
                xaxis=list(gridcolor="#F3F4F6",title=sc),
                yaxis=list(gridcolor="#F3F4F6",title="",tickfont=list(color="#6B7280",size=10))))
  })
  
  # ── KEPADATAN PENDUDUK ───────────────────────────────────
  output$kep_kec_chart <- renderPlotly({
    d <- data.frame(
      kec = names(KEPADATAN_KEC),
      pop = as.numeric(KEPADATAN_KEC)
    ) %>% arrange(desc(pop))
    
    plot_ly(d, x=~pop, y=~reorder(kec, pop), type="bar", orientation="h",
            marker=list(color=~pop, colorscale="Viridis", showscale=FALSE),
            text=~paste0(format(pop, big.mark="."), " jiwa/km²"), hoverinfo="text") %>%
      layout(mk(margin=list(t=5, b=45, l=110, r=90),
                xaxis=list(gridcolor="#F3F4F6", title="Kepadatan (jiwa/km²)"),
                yaxis=list(gridcolor="#F3F4F6", title="", tickfont=list(color="#6B7280", size=10.5))))
  })
  
  output$kep_kec_ui <- renderUI({
    kec_df <- data.frame(
      kecamatan = names(KEPADATAN_KEC),
      kepadatan = as.numeric(KEPADATAN_KEC)
    ) %>% arrange(desc(kepadatan))
    max_k <- max(kec_df$kepadatan)
    tags$div(style="max-height:380px;overflow-y:auto;padding-right:4px;",
             lapply(seq_len(nrow(kec_df)),function(i){
               r <- kec_df[i,]
               pct <- round(r$kepadatan/max_k*100,0)
               col <- if(r$kepadatan>=10000)"#4F46E5" else if(r$kepadatan>=7000)"#818CF8" else "#C7D2FE"
               tags$div(class="kep-bar-row",
                        tags$div(class="kep-kec",r$kecamatan),
                        tags$div(class="kep-bar-wrap",
                                 tags$div(class="kep-bar-fill",style=paste0("width:",pct,"%;background:",col,";"))),
                        tags$div(class="kep-val",paste0(format(r$kepadatan,big.mark=".", decimal.mark=",")))
               )
             })
    )
  })
  
  output$kep_scatter <- renderPlotly({
    d <- data_usaha_spatial %>% group_by(kecamatan) %>%
      summarise(
        avg_pot = mean(as.numeric(skor_potensi), na.rm=TRUE),
        kep     = mean(as.numeric(kepadatan),    na.rm=TRUE),
        n       = n(), .groups="drop"
      ) %>%
      mutate(
        avg_pot = ifelse(is.nan(avg_pot)|is.na(avg_pot), 0, avg_pot),
        kep     = ifelse(is.nan(kep)|is.na(kep), 0, kep)
      )
    plot_ly(d, x=~kep, y=~avg_pot, size=~n,
            text=~paste0("<b>",kecamatan,"</b><br>Kepadatan: ",
                         format(round(kep), big.mark=".", decimal.mark=",")," org/km²<br>Avg Potensi: ",round(avg_pot,1)),
            hoverinfo="text", type="scatter", mode="markers+text",
            textposition="top center", textfont=list(color="#6B7280",size=10.5),
            marker=list(sizemode="diameter", sizeref=.18, opacity=.85,
                        color=~avg_pot,
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
                        showscale=TRUE, colorbar=list(bgcolor="#fff", bordercolor="#E5E7EB",
                                                     thickness=10, title="Potensi",
                                                     tickfont=list(color="#9CA3AF",size=9)))) %>%
      layout(mk(xaxis=list(gridcolor="#F3F4F6",title="Kepadatan Penduduk (org/km²)"),
                yaxis=list(gridcolor="#F3F4F6",title="Avg Skor Potensi")))
  })
  
  output$kep_bar <- renderPlotly({
    d <- data_usaha_spatial %>% group_by(kecamatan) %>%
      summarise(
        avg_pot = mean(as.numeric(skor_potensi), na.rm=TRUE),
        kep     = mean(as.numeric(kepadatan),    na.rm=TRUE),
        n       = n(), .groups="drop"
      ) %>%
      mutate(
        avg_pot = ifelse(is.nan(avg_pot)|is.na(avg_pot), 0, avg_pot),
        kep     = ifelse(is.nan(kep)|is.na(kep), 0, kep),
        skor_w  = round((avg_pot * 0.6) + ((kep/13000*100) * 0.4), 1)
      ) %>% arrange(desc(skor_w))
    plot_ly(d,x=~reorder(kecamatan,-skor_w),y=~skor_w,type="bar",
            marker=list(color=~skor_w,
                        colorscale=list(c(0,"#EEF2FF"),c(.5,"#818CF8"),c(1,"#4F46E5")),
                        showscale=FALSE,opacity=.9),
            text=~round(skor_w,1),
            textposition="outside",textfont=list(color="#9CA3AF",size=10)) %>%
      layout(mk(margin=list(t=5,b=72,l=44,r=12),
                xaxis=list(gridcolor="#F3F4F6",title="",tickangle=-38,tickfont=list(color="#6B7280",size=10.5)),
                yaxis=list(gridcolor="#F3F4F6",title="Skor Berbobot")))
  })
  
  # ── AI CHAT ─────────────────────────────────────────────
  chat_disp <- reactiveVal(list())
  chat_api  <- reactiveVal(list())
  
  observeEvent(input$btn_clear, { chat_disp(list()); chat_api(list()) })
  
  # Quick questions
  qq_map <- list(
    qq1="Kecamatan mana yang paling cocok untuk membuka coffee shop di Makassar? Analisis data lengkap.",
    qq2="Berapa estimasi modal dan berapa bulan bisa balik modal untuk laundry kiloan di Makassar?",
    qq3="Kategori usaha apa yang paling potensial dan masih ada celah pasar di Makassar?",
    qq4="Bagaimana data kepadatan penduduk BPS 2024 mempengaruhi potensi bisnis per kecamatan?",
    qq5="Berikan 5 tips terpenting memilih lokasi usaha di Makassar berdasarkan data riil.",
    qq6="Analisis peluang dan persaingan untuk usaha baru di area Panakkukang.",
    qq7="Apa prediksi tren bisnis yang akan naik di Makassar di 2025-2026?",
    qq8="Kapan sebaiknya menggunakan K-Means dan kapan menggunakan DBSCAN untuk analisis ini?"
  )
  if (length(qq_map) > 0) {
    for (btn_id in names(qq_map)) {
      local({
        bid <- btn_id; txt <- qq_map[[bid]]
        observeEvent(input[[bid]], { updateTextAreaInput(session,"chat_input",value=txt) })
      })
    }
  }
  
  observeEvent(input$btn_send, {
    msg <- trimws(input$chat_input)
    if (nchar(msg)==0) return()
    updateTextAreaInput(session,"chat_input",value="")
    
    disp <- chat_disp()
    disp <- c(disp, list(list(role="user",content=msg)))
    chat_disp(c(disp, list(list(role="thinking",content=""))))
    session$sendCustomMessage("scrollChat",list())
    
    api <- chat_api()
    api <- c(api, list(list(role="user",content=msg)))
    sys_p <- build_sys_prompt(data_usaha)
    reply <- groq_chat(api, sys=sys_p)
    api <- c(api, list(list(role="assistant",content=reply)))
    chat_api(api)
    chat_disp(c(disp, list(list(role="assistant",content=reply))))
    session$sendCustomMessage("scrollChat",list())
  })
  
  output$chat_ui <- renderUI({
    hist <- chat_disp()
    if (length(hist) == 0) {
      return(tags$div(class="chat-welcome",
               tags$div(class="chat-welcome-avatar",
                        tags$img(src="idcu.png", style="width:100%;height:100%;object-fit:contain;")),
               tags$div(class="chat-welcome-title", "Selamat Datang!"),
               tags$div(class="chat-welcome-desc", 
                        "Saya AI Advisor dari Uscu Technology. Tanya saya tentang lokasi usaha, analisis data bisnis, atau strategi di Makassar."),
               tags$div(class="chat-welcome-hint",
                        tags$span(class="hint-chip", "📍 Rekomendasi lokasi"),
                        tags$span(class="hint-chip", "💰 Estimasi modal"),
                        tags$span(class="hint-chip", "📊 Analisis data"),
                        tags$span(class="hint-chip", "🎯 Strategi bisnis"))
             ))
    }
    
    tagList(lapply(hist, function(m) {
      if (m$role == "user") {
        tags$div(class="msg-row-user",
                 tags$div(class="msg-bubble-user", m$content))
      } else if (m$role == "thinking") {
        tags$div(class="msg-row-thinking",
                 tags$div(class="msg-ai-avatar",
                          tags$img(src="idcu.png", style="width:100%;height:100%;object-fit:contain;")),
                 tags$div(class="msg-thinking-bubble",
                          tags$div(class="dots", tags$span(), tags$span(), tags$span()),
                          tags$div(class="msg-thinking-text", "Sedang menganalisis data...")))
      } else {
        txt <- m$content
        txt <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", txt)
        txt <- gsub("\n", "<br>", txt)
        tags$div(class="msg-row-ai",
                 tags$div(class="msg-ai-avatar",
                          tags$img(src="idcu.png", style="width:100%;height:100%;object-fit:contain;")),
                 tags$div(class="msg-bubble-ai",
                          tags$div(class="msg-ai-name", "✨ AI Advisor · Uscu Technology"),
                          tags$div(class="msg-bubble-ai-inner", HTML(txt)))
        )
      }
    }))
  })
  
  output$ai_stats <- renderUI({
    d <- fd()
    n_pesan <- length(chat_api())
    tags$div(
      tags$div(class="stat-card brand",
               tags$div(class="stat-label","Dataset Aktif"),
               tags$div(class="stat-value",style="font-size:22px;",nrow(d)),
               tags$div(class="stat-sub",paste0(length(unique(d$kecamatan))," kecamatan · ",
                                                length(unique(d$kategori_grup))," kategori"))),
      tags$div(class="stat-card violet",
               tags$div(class="stat-label","Pesan dalam Sesi"),
               tags$div(class="stat-value",style="font-size:22px;",n_pesan),
               tags$div(class="stat-sub","AI mengingat seluruh konteks"))
    )
  })
  
  # ── DATA LAB ────────────────────────────────────────────
  output$data_tbl <- renderDT({
    d <- rd()
    bc <- c("nama","kecamatan","kategori_grup","rating","jumlah_ulasan",
            "skor_potensi","label_kelayakan","popularitas","kepadatan","telepon")
    cc <- c("latitude","longitude","alamat")
    cl <- c()
    if("cluster_kmeans"%in%names(d)) cl <- c(cl,"cluster_kmeans")
    if("cluster_dbscan"%in%names(d)) cl <- c(cl,"cluster_dbscan")
    cols <- switch(input$tbl_cols, "all"=c(bc,cc,cl), "basic"=bc, "coord"=c(bc[1:2],cc), "cluster"=c(bc[1:2],cl))
    cols <- cols[cols%in%names(d)]
    dt <- datatable(d[,cols], rownames=FALSE, filter="top",
                    options=list(pageLength=15, scrollX=TRUE,
                                 language=list(search="Cari:",zeroRecords="Tidak ada data",
                                               lengthMenu="Tampilkan _MENU_ baris")),
                    class="display compact hover")
    if("rating"%in%cols)
      dt <- dt %>% formatStyle("rating",
                               background=styleColorBar(range(d$rating,na.rm=T),"rgba(245,158,11,.15)"),
                               backgroundSize="100% 80%",backgroundRepeat="no-repeat",backgroundPosition="center")
    if("skor_potensi"%in%cols)
      dt <- dt %>% formatStyle("skor_potensi",
                               background=styleColorBar(range(d$skor_potensi,na.rm=T),"rgba(79,70,229,.15)"),
                               backgroundSize="100% 80%",backgroundRepeat="no-repeat",backgroundPosition="center")
    if("label_kelayakan"%in%cols)
      dt <- dt %>% formatStyle("label_kelayakan",
                               color=styleEqual(c("Sangat Layak","Layak","Cukup Layak","Kurang Layak"),
                                                c("#059669","#4F46E5","#D97706","#DC2626")),
                               fontWeight="600")
    dt
  })
  
  # ── REPORT GENERATION ───────────────────────────────────
  observeEvent(input$show_report_modal, {
    showModal(modalDialog(
      title = tags$div(style="display:flex;align-items:center;gap:10px;",
                       tags$i(class="fa fa-file-pdf", style="color:#8B5CF6;"),
                       "Konfigurasi Laporan Presentasi"),
      tags$p("Masukkan nama anggota kelompok (5 orang) untuk dicantumkan dalam laporan akademik:"),
      fluidRow(
        column(6, textInput("member1", "Anggota 1 (Ketua)", "Nama Lengkap 1")),
        column(6, textInput("member2", "Anggota 2", "Nama Lengkap 2"))
      ),
      fluidRow(
        column(6, textInput("member3", "Anggota 3", "Nama Lengkap 3")),
        column(6, textInput("member4", "Anggota 4", "Nama Lengkap 4"))
      ),
      fluidRow(
        column(12, textInput("member5", "Anggota 5", "Nama Lengkap 5"))
      ),
      tags$hr(),
      tags$div(style="font-size:12px; color:#6B7280; background:#F9FAFB; padding:10px; border-radius:6px;",
               tags$b("Isi Laporan:"),
               tags$ul(
                 tags$li("Statistik Deskriptif Usaha Makassar"),
                 tags$li("Analisis Kepadatan Penduduk BPS 2024"),
                 tags$li("Rekomendasi Strategis AI Advisor"),
                 tags$li("Data Kelayakan Bisnis per Kecamatan")
               )
      ),
      footer = tagList(
        modalButton("Batal"),
        downloadButton("download_pdf_report", "🚀 Generate & Download PDF", 
                       class="btn btn-primary", style="background:#8B5CF6; border:none;")
      ),
      size = "m", easyClose = TRUE
    ))
  })

  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Bisnis_Makassar_", Sys.Date(), ".html") # PDF-ready HTML
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        members = c(input$member1, input$member2, input$member3, input$member4, input$member5),
        data = fd(),
        kec_summary = data_usaha %>% group_by(kecamatan) %>% 
                      summarise(n=n(), pot=mean(skor_potensi, na.rm=T), .groups="drop") %>% 
                      arrange(desc(pot))
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )

  output$dl1 <- downloadHandler(
    filename=function() paste0("makassar_full_",Sys.Date(),".csv"),
    content=function(f) write.csv(data_usaha,f,row.names=FALSE))
  output$dl2 <- downloadHandler(
    filename=function() paste0("makassar_filtered_",Sys.Date(),".csv"),
    content=function(f) write.csv(fd(),f,row.names=FALSE))
}

shinyApp(ui=ui, server=server)