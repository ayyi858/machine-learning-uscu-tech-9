# 🚀 Uscu Technology — Makassar Business Dashboard
### **Decision Support System Berbasis Machine Learning & AI untuk Analisis Lokasi Usaha**

![Logo Uscu Technology](www/idcu.png)

## 📋 Deskripsi Proyek
**Uscu Technology** adalah platform *Market Intelligence* canggih yang dirancang untuk membantu pengusaha, investor, dan mahasiswa dalam menganalisis potensi bisnis di Kota Makassar secara presisi. Platform ini mengintegrasikan data geografis riil, statistik kependudukan BPS 2024, dan algoritma cerdas untuk memberikan rekomendasi lokasi usaha yang optimal.

## ✨ Fitur Utama
*   **📊 Dashboard Analitik**: Visualisasi data real-time jumlah usaha, rata-rata rating, dan popularitas per wilayah.
*   **📍 Peta Interaktif (Leaflet)**: Heatmap kepadatan bisnis dan sebaran lokasi kompetitor dengan detail foto usaha.
*   **🤖 AI Chat Advisor**: Konsultan bisnis berbasis AI (Groq LLaMA 3.3) yang memahami konteks data lokal Makassar.
*   **🔍 Machine Learning Clustering**:
    *   **K-Means**: Segmentasi wilayah berdasarkan potensi ekonomi.
    *   **DBSCAN**: Deteksi area jenuh (Oversaturated) vs area peluang (Blue Ocean).
*   **🏠 Simulasi Usaha & ROI**: Estimasi modal, profit bulanan, dan masa balik modal (Break Even Point) untuk 10 jenis usaha populer.
*   **📈 Integrasi Data BPS 2024**: Analisis kepadatan penduduk per kecamatan terbaru untuk akurasi target pasar.
*   **📄 Laporan Akademik**: Fitur ekspor laporan presentasi otomatis dalam format profesional.

## 🛠️ Tech Stack
*   **Language**: [R](https://www.r-project.org/)
*   **Framework**: [Shiny](https://shiny.posit.co/)
*   **Visualization**: [Plotly](https://plotly.com/r/), [ggplot2](https://ggplot2.tidyverse.org/)
*   **Mapping**: [Leaflet](https://rstudio.github.io/leaflet/)
*   **Machine Learning**: `cluster`, `dbscan`, `factoextra`
*   **AI Engine**: [Groq API](https://groq.com/) (LLaMA 3.3)
*   **UI/UX**: `shinydashboard`, `shinyWidgets`, Custom CSS (Inter Font)

## 🚀 Cara Menjalankan (Local)
1.  **Clone Repositori**:
    ```bash
    git clone https://github.com/ayyi858/machine-learning-uscu-tech-9.git
    ```
2.  **Konfigurasi Environment**:
    Buat file `.Renviron` di folder root dan tambahkan API Key Anda:
    ```env
    GROQ_API_KEY=your_api_key_here
    ```
3.  **Install Dependencies**:
    Buka RStudio dan jalankan:
    ```R
    source("install_packages.R")
    ```
4.  **Run App**:
    ```R
    shiny::runApp()
    ```

## 👥 Tim Pengembang (Kelompok 9)
Laporan dan platform ini dikembangkan untuk kebutuhan presentasi akademik:
1. **MUAMMAR** - Ketua Tim / Lead Developer
2. **ARYA PRAMUKTI BASRI** - Data Scientist
3. **AHMAD SYARIF HIDAYATULLAH** - UI/UX Designer
4. **RAHMAT FADHILA** - Business Analyst
5. **MUHAMMAD IKRAR** - Research & Documentation

---
**Uscu Technology** — *Inovasi Berbasis Data untuk Masa Depan Bisnis Makassar.*
