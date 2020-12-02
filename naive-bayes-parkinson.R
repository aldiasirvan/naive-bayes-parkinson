library(naivebayes)
library(psych)
library(caret)

dt <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data", sep = ",", header = TRUE)
head(dt) #tampilkan 5 data

str(dt) #tipe data
summary(dt) #ringkasan data

kor <-as.dist(round(cor(dt[,2:24],method = "spearman"),2))
kor

dt <- dt[,-c(1,3,6:9,11:17,24)] #menghilangkan variabel di atas
str(dt)

#Mengambil data yang akan dibuat interval
dtin <- dt[,-c(5)] #menghilangkan variabel status

#Membuat interval
p <- lapply(dtin, FUN = function(a){cut(a,breaks=c(min(a),quantile(a,0.25),quantile(a,0.50),quantile(a,0.75),max(a)),labels = c("rendah","sedang","tinggi","sangat-tinggi"))})

#Merubah list ke data frame dan menggabungkan dengan status
dtfix <- data.frame(p[1:9],as.factor(dt$status))
levels(dtfix$as.factor.dt.status.) = c("Sehat","Parkinson") #merubah levels status menjadi sehat dan parkinson
colnames(dtfix)[10] <- "Status" #ubah nama variabel status
str(dtfix)

pairs.panels(dtfix)

set.seed(911)
sampel <- sample(2, nrow(dtfix), replace = T, prob = c(0.8,0.2))
trainingdat <- dtfix[sampel==1, ]
testingdat <- dtfix[sampel==2, ]
print(paste("Jumlah Train Data: ", nrow(trainingdat), "| Jumlah Test Data: ", nrow(testingdat)))

modelnaiv <- naive_bayes(Status~.,data=trainingdat)
modelnaiv

#Visualisasi Model
plot(modelnaiv)

summary(modelnaiv)

prediksi <- predict(modelnaiv, testingdat)
confusionMatrix(table(prediksi,testingdat$Status))

presisi <- 5/(5+2)
recall <- 5/(5+6)
f1 <- 2*(recall*presisi)/(recall+presisi)