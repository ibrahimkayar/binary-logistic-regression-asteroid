df <- read.csv("nasa_asteroids_2016_2025.csv", stringsAsFactors = FALSE)

head(df)
str(df)
names(df)
dim(df)
summary(df)

#burada target de??i??kenimi karakterden ??evirdim. 0/1 format??na ??evirdim
df$is_potentially_hazardous <- ifelse(df$is_potentially_hazardous == "True", 1, 0)

table(df$is_potentially_hazardous)
prop.table(table(df$is_potentially_hazardous))

#burada gereksiz s??tunlar?? sildim.id, name, date ve orbiting_body modelde kullanilmayacak
df <- df[, !(names(df) %in% c("id", "name", "date", "orbiting_body"))]
names(df)

#eksik de??er kontrol ettim
colSums(is.na(df))  #hi?? eksik de??er yok. M??kemmel

#target da????l??m??
library(ggplot2)

ggplot(df, aes(x = factor(is_potentially_hazardous))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Hazardous (0=No, 1=Yes)", y = "Count",
       title = "Distribution of Hazardous Asteroids")

#n??merik de??i??kenleri inceleme
summary(df)
library(dplyr)

df %>%
  group_by(is_potentially_hazardous) %>%
  summarise(
    avg_velocity = mean(relative_velocity_kmph),
    avg_distance = mean(miss_distance_km),
    avg_diameter = mean(estimated_diameter_max_km)
  )

#diameter vs Hazard
ggplot(df, aes(x = factor(is_potentially_hazardous), y = estimated_diameter_max_km)) +
  geom_boxplot() +
  labs(title = "Diameter vs Hazard Status")
#velocity vs hazard
ggplot(df, aes(x = factor(is_potentially_hazardous), y = relative_velocity_kmph)) +
  geom_boxplot() +
  labs(title = "Velocity vs Hazard Status")


#  VERIYI HAZIRLAMA


# multicollinearity azaltmak icin diameter degiskenlerinden birini siliyorum
# min ve max diameter neredeyse ayni bilgiyi tasiyor

df <- df[, !(names(df) %in% c("estimated_diameter_min_km"))]



# TRAIN - TEST SPLIT


# sonuclarin tekrar uretilebilir olmasi icin seed belirliyorum
set.seed(42)

# verinin %80'i train, %20'si test olacak sekilde index secimi yapiyorum
train_index <- sample(1:nrow(df), 0.8 * nrow(df))

# train ve test veri setlerini olusturuyorum
train <- df[train_index, ]
test  <- df[-train_index, ]



#    LOGISTIC REGRESSION MODELI 

# binary logistic regression modelini glm ile kuruyorum
# family = binomial oldugu icin lojistik regresyon kullaniliyor
model <- glm(is_potentially_hazardous ~ .,
             data = train,
             family = binomial)

# model ozetini goruyorum
summary(model)

# odds ratio degerlerini hesapliyorum
# katsayilarin ussunu alarak odds ratio elde edilir
exp(coef(model))



#    TAHMIN

# test verisi icin tehlikeli olma olasiliklarini hesapliyorum
prob <- predict(model, test, type = "response")

# varsayilan threshold = 0.5 ile sinif tahmini yapiyorum
pred_05 <- ifelse(prob > 0.5, 1, 0)

# alternatif olarak class imbalance nedeniyle daha dusuk threshold denedim
pred_02 <- ifelse(prob > 0.2, 1, 0)


#   CONFUSION MATRIX


# 0.5 threshold icin confusion matrix
table(Predicted = pred_05, Actual = test$is_potentially_hazardous)

# caret paketi ile daha detayli performans ozeti
library(caret)

# positive = "1" diyerek pozitif sinifi "hazardous asteroid" olarak belirtiyorum
confusionMatrix(as.factor(pred_05),
                as.factor(test$is_potentially_hazardous),
                positive = "1")

# 0.2 threshold icin de bakmak istersek
confusionMatrix(as.factor(pred_02),
                as.factor(test$is_potentially_hazardous),
                positive = "1")



#   ROC CURVE ve AUC

library(pROC)

# ROC objesi olusturuyorum
roc_obj <- roc(test$is_potentially_hazardous, prob)

# ROC egrisini ciziyorum
plot(roc_obj)

# AUC degerini hesapliyorum
auc(roc_obj)

