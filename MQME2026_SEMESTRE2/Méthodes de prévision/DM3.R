library(readxl)
install.packages("openxlsx")   # une seule fois
library(openxlsx)

data = read_excel("/home/layedev/Documents/miashs2026/MQME2026_SEMESTRE2/Méthodes de prévision/serie_000641413_18022026.xlsx")
View(data)
colnames(data) = c("periode", "prix")
data$prix <- as.numeric(data$prix)

View(data)
data = rbind(c("2025-12", 37.92), data)
data$prix <- as.numeric(data$prix)
View(data)
summary(data)

write.xlsx(data, "/home/layedev/Documents/miashs2026/MQME2026_SEMESTRE2/Méthodes de prévision/DM3_ventes.xlsx")

