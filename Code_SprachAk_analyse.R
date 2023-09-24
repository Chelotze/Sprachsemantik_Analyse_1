
library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(base)
library(stats)
library(dplyr)
library(nnet)
install.packages("glmnet")
install.packages("corrplot")
library(corrplot)
library(Matrix)
library(glmnet)
library(stats)
library(pROC)
library(nnet)


total <- read.csv("../FeaturesTotal__")

FeaturesTotal_$NTR <- NULL #entfernen unnötiger Spalten
FeaturesTotal_$LEI <- NULL #entfernen unnötiger Spalten
FeaturesTotal_$GLE <- NULL #entfernen unnötiger Spalten


total <- FeaturesTotal_ #DataFrame in 'total' laden


total$LEI.STP <- as.factor(total$LEI.STP) #Konvertierung der Zielvariable in einen Faktor
total$LEI.STP <- relevel(total$LEI.STP, ref = "2") #Neuanordnung der Levels der Zielvariablen


original_total <- total #Sicherungskopie87z65t99er
total <- total %>%
  mutate(across(2:156, ~as.numeric(as.character(.)))) #konvertierung in numerische Daten


str(total[, 2:156]) #überprüfen


total <- total %>% mutate_at(c(2:155), ~(scale(.) %>% as.vector)) #Z-Standarisierung


summary(total) # Deskriptive Statistiken
sapply(total, function(col) sum(is.na(col))) # Überprüfen auf fehlende Werte




sapply(total[, 2:155], mean)  # Überprüfen des Mittelwerts jeder Spalte
sapply(total[, 2:155], sd)    # Überprüfen der Standardabweichung jeder Spalte

total$ID <- NULL #entfernen unnötiger Spalten
total$tags.file_name <- NULL
total$Speaking_Rate <- NULL


#Datenaufteilung: 

set.seed(123) #Für Reproduzierbarkeit
index <- createDataPartition(
  total$LEI.STP, p = 0.7, 
  list = FALSE
)

train_data <- total[index , ] #Trainingsdaten
test_data <- total[-index , ] #Testdaten




#RFE mit Random Forest:

control <- rfeControl(
  functions = rfFuncs , 
  method = "cv" , 
  number = 10 
) #10-fache Kreuzvalidierung


#Durchführen der rekursiven Feature-Eliminierung:

results <- rfe(
  train_data[, -which(names(train_data) == "LEI.STP")],
  train_data$LEI.STP,
  sizes=c(1:ncol(train_data)-1),
  rfeControl=control
  )



print(results)
results$optVariables


LEI.var <- total


# Die fünf besten Features basierend auf vorherigen Daten:
top5_features <- c(
  "Spectral_Centroid.min",
  "Spectral_Bandwith.min",
  "mfcc2_max",
  "Zero_Crossing_Rate.min",
  "mfcc6_mean"
)

# Lade die gewünschten Spalten aus dem Dataframe 'total' in die Variable 'LEI.top5'
LEI.top5 <- total[, c("LEI.STP", top5_features)]
LEI.mostAcc <- total[ , c ("LEI.STP" , mostAcc_features)]

mostAcc_features<- c("Spectral_Centroid.min" , "Spectral_Bandwith.min" ,
                     "mfcc2_max" , "Zero_Crossing_Rate.min", "mfcc6_mean", "Spectral_Bandwith.stdev",
                     "Spectral_Flatness.mean", "Spectral_Contrast_min.6", "Spectral_Rolloff.mean",
                     "mfcc9_dev", "mfcc11_mean" ,"mfcc1_min", "Spectral_Bandwith.mean", "Spectral_Contrast_min.2" , "Zero_Crossing_Rate.mean", "Spectral_Contrast_min.4", "Spectral_Contrast_min.5", "Spectral_Contrast_mean.6" ,   "Spectral_Centroid.mean",  
                     "Harmonic.11_dev",         
                     "Harmonic.1_dev",          
                     "Pitch.mean",              
                     "mfcc6_dev",               
                     "Pitch.max",               
                     "Zero_Crossing_Rate.stdev",
                     "Harmonic.11_mean",        
                     "Spectral_Contrast_dev.6", 
                     "mfcc5_dev",               
                     "mfcc12_dev",              
                     "Spectral_Flatness.stdev", 
                     "mfcc7_max",               
                     "Spectral_Contrast_min.1", 
                     "Harmonic.10_dev",         
                     "Harmonic.9_dev",          
                     "Harmonic.12_mean",        
                     "Harmonic.8_mean",         
                     "mfcc12_mean",             
                     "Harmonic.10_mean",        
                     "Harmonic.12_dev",         
                     "Spectral_Contrast_min.3", 
                     "Spectral_Contrast_mean.4",
                     "Harmonic.12_min",         
                     "mfcc7_mean",              
                     "Spectral_Contrast_mean.2",
                     "Harmonic.1_max",          
                     "mfcc7_dev",               
                     "Harmonic.11_min",         
                     "Spectral_Flatness.min",   
                     "Spectral_Contrast_mean.5",
                     "mfcc9_min",               
                     "Harmonic.9_mean",         
                     "Pitch.min",               
                     "mfcc10_dev",              
                     "Spectral_Contrast_max.1", 
                     "mfcc8_mean",              
                     "Harmonic.9_min",          
                     "mfcc9_mean",              
                     "Harmonic.10_min",         
                     "Harmonic.7_mean",         
                     "Harmonic.7_dev",          
                     "mfcc13_max",              
                     "Harmonic.1_mean",         
                     "Spectral_Contrast_dev.5", 
                     "mfcc3_dev",               
                     "mfcc2_mean",              
                     "Harmonic.4_mean",         
                     "mfcc3_max",               
                     "Spectral_Contrast_mean.1",
                     "mfcc1_dev",               
                     "Onset_Strength.min",      
                     "Spectral_Contrast_mean.3",
                     "Harmonic.3_min",          
                     "Spectral_Rolloff.min",    
                     "Harmonic.6_mean",         
                     "mfcc5_mean",              
                     "Harmonic.6_min",          
                     "Harmonic.8_dev",          
                     "mfcc8_max",               
                     "Harmonic.5_min",          
                     "Zero_Crossing_Rate.max",  
                     "Spectral_Centroid.max",   
                     "mfcc5_max",               
                     "Harmonic.8_min",          
                     "Spectral_Rolloff.stdev",  
                     "mfcc3_mean",              
                     "mfcc13_mean",             
                     "mfcc10_max",              
                     "mfcc1_mean",              
                     "Harmonic.5_mean",         
                     "mfcc11_max",              
                     "Harmonic.1_min",          
                     "mfcc12_max",              
                     "Harmonic.7_min",          
                     "Harmonic.11_max",         
                     "Harmonic.3_max",          
                     "mfcc10_min",              
                     "mfcc13_dev")



# Erstellen von neuen Trainings- und Testsätzen mit den ausgewählten Features
train_data_selected <- train_data[, c("LEI.STP", mostAcc_features)]
test_data_selected <- test_data[, c("LEI.STP", mostAcc_features)]

x_train <- as.matrix(train_data_selected[, -which(names(train_data_selected) == "LEI.STP")])
y_train <- train_data_selected$LEI.STP

set.seed(123)  # für Reproduzierbarkeit
modell_glmnet <- cv.glmnet(x_train, y_train, family = "multinomial")

print(modell_glmnet)
plot(modell_glmnet)



best_lambda <- modell_glmnet$lambda.min
coefs <- coef(modell_glmnet, s = best_lambda)
print(coefs)



# Koeffizienten in einen data.frame konvertieren
coefs_df <- as.data.frame(as.matrix(coefs$`2`))
coefs_df$Feature <- rownames(coefs_df)
colnames(coefs_df) <- c("Coefficient", "Feature")

# Sortiere nach den absoluten Werten der Koeffizienten
sorted_features<- coefs_df[order(-abs(coefs_df$Coefficient)), ]

print(sorted_features)


# Koeffizienten in einen data.frame konvertieren
coefs_df1<- as.data.frame(as.matrix(coefs$`2`))
coefs_df$Feature <- rownames(coefs_df)
colnames(coefs_df) <- c("Coefficient", "Feature")

# Sortiere nach den absoluten Werten der Koeffizienten
sorted_features <- coefs_df[order(-abs(coefs_df$Coefficient)), ]

print(sorted_features)


# Koeffizienten in einen data.frame konvertieren
coefs_d1 <- as.data.frame(as.matrix(coefs$`1`))
coefs_d1$Feature <- rownames(coefs_d1)
colnames(coefs_d1) <- c("Coefficient", "Feature")

# Sortiere nach den absoluten Werten der Koeffizienten
sorted_features1<- coefs_d1[order(-abs(coefs_d1$Coefficient)), ]

print(sorted_features1)


# Koeffizienten in einen data.frame konvertieren
coefs_d3 <- as.data.frame(as.matrix(coefs$`3`))
coefs_d3$Feature <- rownames(coefs_d3)
colnames(coefs_d3) <- c("Coefficient", "Feature")

# Sortiere nach den absoluten Werten der Koeffizienten
sorted_features3<- coefs_d3[order(-abs(coefs_d3$Coefficient)), ]

print(sorted_features3)


install.packages("DT")
library(DT)

datatable(sorted_features, extensions = 'Buttons', options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
))


datatable(confusion_matrix, extensions = 'Buttons', options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
))





print(significant_features)


# Liste aller Koeffizientenmatrizen
coef_list <- list(coefs$`1`, coefs$`2`, coefs$`3`)

# Leerer Vektor für signifikante Features
significant_features <- c()

# Durchlaufen Sie die Liste und fügen Sie signifikante Features hinzu
for (coef_matrix in coef_list) {
  significant_features <- c(significant_features, rownames(coef_matrix)[coef_matrix[, 1] != 0])
}

# Entfernen von Duplikaten
significant_features <- unique(significant_features)

significant_features <- significant_features[significant_features != "(Intercept)"]


test_data_selected <- test_data [, c ("LEI.STP" , significant_features)]


dim(x_train)
dim(x_test)

print(x_train)
print(x_test)





x_test <- as.matrix(test_data_selected[, -which(names(test_data_selected) == "LEI.STP")])
y_test <- test_data_selected$LEI.STP

predictions <- predict(modell_glmnet, newx = x_test, s = best_lambda, type = "class")

confusion_matrix <- table(Predicted = predictions, Actual = y_test)
print(confusion_matrix)

print(sorted_features1)



# Spaltennamen von x_test erhalten
cols_to_keep <- colnames(x_test)

# x_train auf die Merkmale von x_test reduzieren
x_train_reduced <- x_train[, cols_to_keep]


# Neu trainieren des glmnet-Modells mit den reduzierten Daten
modell_glmnet_reduced <- cv.glmnet(x_train_reduced, y_train, family = "multinomial")

# Den besten Lambda-Wert aus dem neu trainierten Modell holen
best_lambda_reduced <- modell_glmnet_reduced$lambda.min

# Vorhersagen mit dem neuen Modell und x_test treffen
predictions_try <- predict(modell_glmnet_reduced, newx = x_test, s = best_lambda, type = "class")


confusion_matrix <- table(predictions_try, y_test)
print(confusion_matrix)





#glmnet war nicht aussagekräftig bei Datensatz, deswegen folgend neu mit Kreuzvalidierung

set.seed(123) # für Reproduzierbarkeit

#Trainings und Testsätze
train_index <- sample(1:nrow(LEI.mostAcc), 0.8*nrow(LEI.mostAcc))
train_dataK <- LEI.mostAcc[train_index, ]
test_dataK <- LEI.mostAcc[-train_index, ]


#k-fold Validierung + glmnet 
x_trainK <- as.matrix(train_dataK[, -which(names(train_dataK) == "LEI.STP")]) 
y_trainK <- train_dataK$LEI.STP

cv_fit <- cv.glmnet(x_trainK, y_trainK, family="multinomial", type.multinomial = "grouped", nfolds=10)
best_lambda <- cv_fit$lambda.min



#Modelltraining mit bester Lambda-Wert
modell_glmnet <- glmnet(x_trainK, y_trainK, family="multinomial", type.multinomial = "grouped", lambda=best_lambda)


#Modellbewertung
x_testK <- as.matrix(test_dataK[, -which(names(test_dataK) == "LEI.STP")])
y_testK <- test_dataK$LEI.STP

predictions <- predict(modell_glmnet, newx = x_testK, s = best_lambda, type = "class")

# Konfusionsmatrix
confusion_matrix <- table(predictions, y_testK)
print(confusion_matrix)


str(coef(modell_glmnet))



# Für Klasse NTR (Referenzklasse):
coefs_NTR <- as.matrix(coef(modell_glmnet)[["2"]])
print("Koeffizienten für Klasse NTR:")
sorted_NTR<- coefs_df[order(-abs(coefs_df$Coefficient)), ]
print(sorted_NTR)

# Für Klasse GLE:
coefs_GLE <- as.matrix(coef(modell_glmnet)[["1"]])
print("Koeffizienten für Klasse GLE:")
print(coefs_GLE)
sorted_GLE<- coefs_df[order(-abs(coefs_df$Coefficient)), ]
print(sorted_GLE)

# Für Klasse LEI:
coefs_LEI <- as.matrix(coef(modell_glmnet)[["3"]])
print("Koeffizienten für Klasse LEI:")
print(coefs_LEI)
sorted_LEI<- coefs_df[order(-abs(coefs_df$Coefficient)), ]
print(sorted_LEI)




# Koeffizienten für jede Klasse extrahieren
coefs_NTR <- as.matrix(coef(modell_glmnet)[["2"]])
coefs_GLE <- as.matrix(coef(modell_glmnet)[["1"]])
coefs_LEI <- as.matrix(coef(modell_glmnet)[["3"]])

# Nur Koeffizienten behalten, die nicht null sind
coefs_NTR <- coefs_NTR[coefs_NTR != 0, , drop = FALSE]
coefs_GLE <- coefs_GLE[coefs_GLE != 0, , drop = FALSE]
coefs_LEI <- coefs_LEI[coefs_LEI != 0, , drop = FALSE]

# Alle Koeffizienten in eine gemeinsame Liste packen
all_coefs <- list(NTR = coefs_NTR, GLE = coefs_GLE, LEI = coefs_LEI)

# Die Liste ausgeben
print(all_coefs)



# Merkmalsnamen für jede Klasse extrahieren
features_NTR <- rownames(coefs_NTR)
features_GLE <- rownames(coefs_GLE)
features_LEI <- rownames(coefs_LEI)

# Alle Merkmale zusammenführen und Duplikate entfernen
unique_features <- unique(c(features_NTR, features_GLE, features_LEI))

# Die einzigartigen Merkmale ausgeben
print(unique_features)
df_unique_features <- data.frame(Features = unique_features)

datatable(coefs_LEI, extensions = 'Buttons', options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
))


plot(fit)
plot(cv_fit)


fit <- modell_glmnet
class(fit)
length(fit$lambda)
is.unsorted(fit$lambda)



# nochmal, weil der plot nicht angezeigt werden konnte

fit <- glmnet(x_trainK, y_trainK, family = "multinomial", type.multinomial = "grouped")
plot(fit, xvar="lambda", label=TRUE)
cv_fit <- cv.glmnet(x_trainK, y_trainK, family = "multinomial", type.multinomial = "grouped")
plot(cv_fit)

# optimales lambda 
best_lambda <- cv_fit$lambda.min


#neu trainieren mit optimalem lambda

modell_optimal <- glmnet(x = x_trainK, y = y_trainK, family = "multinomial", lambda = best_lambda)
predictions_optimal <- predict(modell_optimal, newx = x_testK, s = best_lambda, type = "class")

confusion_matrix_optimal <- table(predictions_optimal, y_testK)
print(confusion_matrix_optimal)

print(confusion_matrix)
test_data_opt <- test_dataK[, c("LEI.STP", unique_features)]


# Leere Strings aus dem Vektor entfernen
unique_features <- unique_features[unique_features != ""]

# Testen, ob alle Features in test_dataK vorhanden sind
if (all(unique_features %in% names(test_dataK))) {
  test_data_opt <- test_dataK[, c("LEI.STP", unique_features)]
} else {
  missing_features <- unique_features[!unique_features %in% names(test_dataK)]
  print(paste("Folgende Features fehlen in test_dataK:", paste(missing_features, collapse = ", ")))
}


print(coefs_GLE)
print(coefs_LEI)
print(coefs_NTR)


# Spaltennamen von x_test erhalten
cols_to_keep <- colnames(x_testK)

# x_train auf die Merkmale von x_test reduzieren
x_train_opt <- x_trainK[, cols_to_keep]
modell_optimal <- glmnet(x = x_trainK, y = y_trainK, family = "multinomial", lambda = best_lambda)
predictions_optimal <- predict(modell_optimal, newx = x_testK, s = best_lambda, type = "class")

confusion_matrix_optimal_red <- table(predictions_optimal, y_testK)
print(confusion_matrix_optimal_red)



library(dplyr)

# Nehmen wir an, unique_features ist ein Charakter-Vektor mit den Feature-Namen
selected_features_opt <- c(unique_features, "LEI.STP")

# Extrahieren der gewählten Features aus den Trainings- und Testdaten
train_data_opt <- train_dataK %>%
  select(all_of(selected_features_opt))

test_data_opt <- test_dataK %>%
  select(all_of(selected_features_opt))


#EVALUIEREN
predictions_try <- as.factor(predictions_try)
levels(predictions_try) <- levels(y_test)

cm1 <- confusionMatrix(predictions_try, y_test)
print(cm1)


# Zugriff auf Statistiken:
cm$byClass


#MLR
modell_mlr <- multinom(LEI.STP ~ ., data=train_data_opt)
predictions_mlr <- predict(modell_mlr, newdata = test_data_opt)


cm_mlr <- confusionMatrix(predictions_mlr, test_data_opt$LEI.STP)
print(cm_mlr)

summary(modell_mlr)

plot(modell_mlr)


library(ggplot2)

# Extrahieren der Koeffizienten und Umwandeln in ein Dataframe
coefs_mlr <- as.data.frame(coef(modell_mlr))
coefs_mlr$feature <- rownames(coefs_mlr)

# Schmelzen des Dataframes für ggplot
library(reshape2)
coefs_mlr_melted <- melt(coefs_mlr, id.vars="feature")

# Plot MLR
ggplot(coefs_mlr_melted, aes(x=feature, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  labs(y="Coefficient Value", x="", fill="Class") +
  theme_light() +
  theme(legend.position="bottom")



library(ggplot2)
library(dplyr)

# Überprüfen Sie die Struktur der Daten
str(train_dataK)

# Grundlegende Statistiken
summary(train_dataK)

# Überprüfen Sie das Klassenungleichgewicht
table(train_dataK$LEI.STP)

# Visualisierung des Klassenungleichgewichts
ggplot(train_dataK, aes(x = LEI.STP)) + 
  geom_bar(fill = "skyblue") + 
  labs(title = "Distribution of Classes", x = "Class", y = "Count")


ggplot(train_dataK, aes(x=Spectral_Centroid.min)) + 
  geom_histogram(binwidth=0.1, fill="steelblue") + 
  ggtitle("Verteilung von Spectral_Centroid.min")

ggplot(train_dataK, aes(x=LEI.STP, y=mfcc6_mean)) + 
  geom_boxplot() + 
  ggtitle("Boxplot von Zero_Crossing_Rate.min für jede Klasse von LEI.STP")


cor_matrix <- cor(select(train_data_opt, -LEI.STP))
corrplot(cor_matrix, method="color")

# Paarweise Korrelationen - um Beziehungen zwischen Merkmalen zu erkennen
cor_matrix <- cor(train_data_opt[, -which(names(train_data_opt) == "LEI.STP")])
heatmap(cor_matrix)

# Weitere Visualisierungen können erstellt werden, z.B. Histogramme, Boxplots usw.
# um die Verteilung und Beziehungen der Merkmale zu sehen.






# Oversampling
install.packages("smotefamily")
library(smotefamily)




library(smotefamily)

# Daten für SMOTE vorbereiten
X_train <- train_data_opt[, !names(train_data_opt) %in% "LEI.STP"]
Y_train <- train_data_opt$LEI.STP

# SMOTE ausführen
synthetic_data <- SMOTE(X = X_train, target = Y_train, K = 5, dup_size = 100)

# Extrahieren Sie die synthetischen Daten aus synthetic_data
X_synthetic <- synthetic_data$syn_data[, -ncol(synthetic_data$syn_data)] # Alle Spalten außer "class"
Y_synthetic <- synthetic_data$syn_data$class

X <- train_data_opt[, !(names(train_data_opt) %in% "LEI.STP")]  # Alle Spalten außer "LEI.STP"
Y <- train_data_opt$LEI.STP  # Nur die Spalte "LEI.STP"



# Kombinieren Sie die ursprünglichen und synthetischen Daten
X_combined <- rbind(X, X_synthetic)
Y_combined <- c(Y, Y_synthetic)



length(Y)



#Prüfung auf Overfitting

library(caret)

set.seed(123) # Für Reproduzierbarkeit

splitIndex_combined <- createDataPartition(Y_combined, p = 0.8, list = FALSE)

syn_train_data <- data.frame(X_combined[splitIndex, ], Y = Y_combined[splitIndex])
validate_data <- data.frame(X_combined[-splitIndex, ], Y = Y_combined[-splitIndex])


#Modelltraining
modell_mlr <- multinom(Y ~ ., data = syn_train_data)


# Vorhersagen auf den Validierungsdaten
predictions_mlr <- predict(modell_mlr, newdata = validate_data)

# Erstellen Sie die Konfusionsmatrix
cm_syn <- table(predictions_mlr, validate_data$Y)
print(cm_syn)

library(caret)

cm_syn2 <- confusionMatrix(predictions_mlr, validate_data$Y)
print(cm_syn2)




validate_data$Y <- as.factor(validate_data$Y)

barplot(table(train_data_opt$LEI.STP), main="Verteilung der Klassen in den Originaldaten", col="lightblue", ylab="Anzahl")

library(corrplot)

cor_matrix <- cor(syn_train_data[, names(syn_train_data) != "Y"]) # Exklusive der Zielvariable
corrplot(cor_matrix, method = "circle")

heatmap(cor_matrix, main="Paarweise Korrelation", col=topo.colors(12), scale="row")

print(unique(predictions_factor))
print(unique(validate_Y_factor))


library(ggplot2)
library(reshape2)

# 1. Koeffizienten aus Ihrem Modell extrahieren
coefs <- as.data.frame(coef(modell_mlr))
coefs$feature <- rownames(coefs)

# 2. Dataframe für die ggplot2-Visualisierung vorbereiten
coefs_melted <- melt(coefs, id.vars="feature")

# 3. Den Plot erstellen
ggplot(coefs_melted, aes(x=feature, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  labs(y="Coefficient Value", x="", fill="Class") +
  theme_light() +
  theme(legend.position="bottom")




library(ggplot2)

# Koeffizienten für eine bestimmte Klasse, z.B. die erste Klasse
coefficients_class3 <- coef_matrix[2,]

# Datenrahmen erstellen
df_coef <- data.frame(
  Variable = names(coefficients_class3),
  Coefficient = coefficients_class3
)

# Plot
ggplot(df_coef, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Koeffizienten für LEI") +
  xlab("Variable") +
  ylab("Koeffizientenwert")


# Koeffizientenmatrix aus dem multinom-Modell
coef_matrix <- coef(modell_mlr)

# Liste, um Dataframes der Koeffizienten für jede Klasse zu speichern
coefs_list <- list()

# Koeffizienten für jede Klasse extrahieren und in der Liste speichern
for (i in 1:nrow(coef_matrix)) {
  class_label <- rownames(coef_matrix)[i]
  coefs_list[[class_label]] <- data.frame(
    Variable = colnames(coef_matrix),
    Coefficient = coef_matrix[i, ]
  )
}

# Die Koeffizienten für die erste nicht-Basis-Klasse anzeigen
print(coefs_list[[2]])



validate_data <- data.frame(X[-splitIndex,], Y = Y[-splitIndex])


#glmnet

library(glmnet)

# Daten in Matrix umwandeln
X_matrix <- as.matrix(X_combined)

# Das glmnet Modell
set.seed(123) # Für Reproduzierbarkeit
cv_fit <- cv.glmnet(X_matrix, Y_combined, family="multinomial", type.multinomial="grouped")

# Das beste Modell aufgrund des kleinsten Fehlers verwenden
best_lambda <- cv_fit$lambda.min
final_model <- glmnet(X_matrix, Y_combined, family="multinomial", type.multinomial="grouped", lambda=best_lambda)

# Vorhersagen auf Validierungsdaten
validate_matrix <- as.matrix(validate_data[, -1]) # Dies sollte bereits geteilt und vorbereitet worden sein
predicted_probs <- predict(final_model, s=best_lambda, newx=validate_matrix, type="response")

# Da die Vorhersagen als Wahrscheinlichkeiten zurückgegeben werden, wählen Sie die Klasse mit der höchsten Wahrscheinlichkeit
predicted_classes <- colnames(predicted_probs)[apply(predicted_probs, 1, which.max)]

# Verwenden Sie nun die `confusionMatrix` Funktion, um die Vorhersagen zu bewerten:
confusionMatrix(predicted_classes, validate_data$Y)


validate_matrix <- as.matrix(sapply(validate_data, as.numeric))
str(validate_matrix)

