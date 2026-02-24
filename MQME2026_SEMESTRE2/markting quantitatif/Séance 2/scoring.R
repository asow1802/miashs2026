data = read.csv("/home/layedev/Documents/miashs2026/MQME2026_SEMESTRE2/markting quantitatif/Séance 2/last_event.csv", header = TRUE)

data_clean = data

data_clean$id_client = NULL
data_clean$event_flag = NULL
data_clean$X = NULL
data_clean$event_month = NULL
data_clean$event_week = NULL
data_clean$participe = NULL
quantitatif = data_clean[,sapply(data_clean, is.numeric)]
qualitatif = sapply(data_clean, is.character)
head(data_clean)

library(caret)
splitIndex = creatDataPartition(data_clean$target, p=0.7, list = FALSE)
train_data = data_clean[splitIndex, ]
test_data = data_clean[-splitIndex, ]

# modèle de regression logistique sur les données d'entreinement
model = glm(target ~ ., data = train_data)