
library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(base)
library(stats)
library(dplyr)
library(nnet)
install.packages("glmnet")
library(Matrix)
library(glmnet)



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
