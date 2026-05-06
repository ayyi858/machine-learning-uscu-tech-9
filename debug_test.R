setwd('e:/CODING PROGRAMING/R STUDIO/ucu tech')
d <- read.csv('data_usaha_makassar_clean.csv', stringsAsFactors=FALSE)
names(d)[names(d)=='Latitude']  <- 'latitude'
names(d)[names(d)=='Longitude'] <- 'longitude'

cat("=== ALL COLUMN CLASSES ===\n")
for(col in names(d)) {
  cat(col, "->", class(d[[col]]), "| is.list:", is.list(d[[col]]), "\n")
}

cat("\n=== CHECK FOR LIST COLUMNS ===\n")
list_cols <- names(d)[sapply(d, is.list)]
cat("List columns:", paste(list_cols, collapse=", "), "\n")

cat("\n=== CONVERT TO NUMERIC ===\n")
d$rating        <- as.numeric(d$rating)
d$skor_potensi  <- as.numeric(d$skor_potensi)
d$jumlah_ulasan <- as.numeric(d$jumlah_ulasan)
d$kepadatan     <- as.numeric(d$kepadatan)
d$latitude      <- as.numeric(d$latitude)
d$longitude     <- as.numeric(d$longitude)

cat("After conversion, list cols:", paste(names(d)[sapply(d, is.list)], collapse=", "), "\n")

cat("\n=== TEST FILTER ===\n")
tryCatch({
  idx <- !is.na(d$latitude) & d$latitude >= -5.25
  cat("Filter OK, TRUE count:", sum(idx), "\n")
}, error = function(e) cat("FILTER ERROR:", conditionMessage(e), "\n"))

cat("\n=== TEST d[idx, ] ===\n")
tryCatch({
  idx <- !is.na(d$latitude) & d$latitude >= -5.25 & d$latitude <= -5.05 &
         !is.na(d$longitude) & d$longitude >= 119.35 & d$longitude <= 119.55
  dsp <- d[idx, ]
  cat("Subset OK, nrow:", nrow(dsp), "\n")
  cat("dsp list cols:", paste(names(dsp)[sapply(dsp, is.list)], collapse=", "), "\n")
}, error = function(e) cat("SUBSET ERROR:", conditionMessage(e), "\n"))

cat("\n=== TEST scale ===\n")
tryCatch({
  num_cols <- c('latitude','longitude','rating','jumlah_ulasan','skor_potensi')
  dsp2 <- dsp[, num_cols]
  for(col in num_cols) cat(col, class(dsp2[[col]]), "\n")
  dsp2[] <- lapply(dsp2, function(x) as.numeric(unlist(x)))
  fs <- scale(as.matrix(dsp2))
  cat("scale OK, dim:", dim(fs), "\n")
}, error = function(e) cat("SCALE ERROR:", conditionMessage(e), "\n"))
