===================================================
  LOKASI USAHA MAKASSAR — Decision Support System
  v2.0 | CSS Terpisah | AI Chat Lengkap
===================================================

STRUKTUR FOLDER (WAJIB):
  MyShinyApp/
  ├── app.R                          ← aplikasi utama
  ├── data_usaha_makassar_clean.csv  ← dataset (6.394 lokasi)
  ├── install_packages.R             ← install package dulu
  └── www/
      └── global_styles.css          ← CSS TERPISAH (buat folder www!)

⚠️  PENTING: File global_styles.css HARUS ada di dalam
    subfolder bernama "www/" di direktori yang sama dengan app.R

CARA SETUP:
  1. Buat folder, misal: C:/Users/Nama/MyShinyApp/
  2. Taruh app.R dan data_usaha_makassar_clean.csv di folder itu
  3. Buat subfolder: C:/Users/Nama/MyShinyApp/www/
  4. Taruh global_styles.css di dalam www/
  5. Buka RStudio → File → Open File → pilih app.R
  6. Jalankan install_packages.R sekali (Tools → Source File)
  7. Klik "Run App" atau: shiny::runApp("app.R")

FITUR LENGKAP:
  ✅ Command Center      - 8 KPI + 6 chart interaktif
  ✅ Peta Interaktif     - Leaflet, heatmap, cluster, foto usaha
  ✅ K-Means Pro         - PCA 2D/3D, Elbow, Silhouette, Gap Stat
  ✅ DBSCAN Spatial      - k-NN, Reachability, Noise analysis
  ✅ Simulasi Usaha      - 10 jenis, estimasi profit, ROI, peta
  ✅ Ranking Lokasi      - Top N terbaik, filter, visualisasi
  ✅ AI Chat Advisor     - Groq LLaMA 3.3 + MEMORY percakapan
  ✅ Komparasi Algo      - Confusion matrix + metrik kualitas
  ✅ Eksplorasi Data     - Multi-var, correlation, bubble chart
  ✅ Data Lab            - Export CSV, tabel interaktif DT

AI CHAT (Groq):
  - Model: llama-3.3-70b-versatile
  - Tekan ENTER untuk kirim pesan
  - Shift+Enter untuk baris baru
  - AI mengingat SELURUH percakapan dalam satu sesi
  - Sudah dilengkapi knowledge dataset Makassar
  - 8 tombol quick question tersedia
