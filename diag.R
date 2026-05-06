d <- read.csv("data_usaha_makassar_clean.csv")
print(nrow(d))
print(summary(d$Latitude))
print(summary(d$Longitude))
d_sp <- d[!is.na(d$Latitude) & !is.na(d$Longitude) &
          d$Latitude >= -5.25 & d$Latitude <= -5.05 &
          d$Longitude >= 119.35 & d$Longitude <= 119.55, ]
print(nrow(d_sp))
