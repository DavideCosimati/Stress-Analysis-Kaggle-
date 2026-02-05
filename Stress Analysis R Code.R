Stress22=read.csv("stress_detection_data.csv", header=TRUE)
Stress_senza_na=na.omit(Stress22)
#osserviamo che il dataset non ha nessun valore mancante
#possiamo continuare ad usare Stress scartando le variabili meno rilevanti per la nostra analisi
Stress=Stress22[ ,-c(7,8,12,15,16,17,18,20)]
names(Stress)
str(Stress)
attach(Stress)

#ANALISI DESCRITTIVA
library(ggplot2)
#VAR QUALITATIVE
freq_ass_gender=table(Gender)
freq_ass_gender
freq_rel_gender=prop.table(freq_ass_gender)
freq_rel_gender
freq_per_gender=prop.table(freq_ass_gender)*100
freq_per_gender
moda_gender=which.max(table(Gender))
moda_gender
ggplot_gender=ggplot(Stress, aes(x = Gender)) +
  geom_bar(aes(fill = Gender)) +
  labs(
    x = "Gender",
    y = "Frequenza assoluta",
    title = "Gender barplot"
  ) 
ggplot_gender



freq_ass_occupation=table(Occupation)
freq_ass_occupation
freq_rel_occupation=prop.table(freq_ass_occupation)
freq_rel_occupation
freq_per_occupation=prop.table(freq_ass_occupation)*100
freq_per_occupation
moda_occupation=which.max(table(Occupation))
moda_occupation
barplot_occupation=barplot(table(Occupation), xlab="occupation", ylab="Freq.assoluta",main="Occupation barplot",col=4,las=2,cex.names=0.25)


freq_ass_marital_status=table(Marital_Status)
freq_ass_marital_status
freq_rel_marital_status=prop.table(freq_ass_marital_status)
freq_rel_marital_status
freq_per_marital_status=prop.table(freq_ass_marital_status)*100
freq_per_marital_status
moda_marital_status=which.max(table(Marital_Status))
moda_marital_status
ggplot_marital_status=ggplot(Stress, aes(x = Marital_Status)) +
  geom_bar(aes(fill = Marital_Status))+
  labs(
    x = "Marital status",
    y = "Frequenza assoluta",
    title = "Marital Status barplot")
ggplot_marital_status

freq_ass_smoking_habit=table(Smoking_Habit)
freq_ass_smoking_habit
freq_rel_smoking_habit=prop.table(freq_ass_smoking_habit)
freq_rel_smoking_habit
freq_per_smoking_habit=prop.table(freq_ass_smoking_habit)*100
freq_per_smoking_habit
moda_smoking_habit=which.max(table(Smoking_Habit))
moda_smoking_habit
ggplot_smoking_habit=ggplot(Stress, aes(x = Smoking_Habit)) +
  geom_bar(aes(fill = Smoking_Habit))+
  labs(
    x = "Smoking habit",
    y = "Frequenza assoluta",
    title = "Smoking habit barplot")
ggplot_smoking_habit

liv_stress= factor(Stress_Detection, levels = c("Low", "Medium", "High"), ordered = TRUE)
levels(liv_stress)
freq_ass_Stress_Detection=table(liv_stress)
freq_ass_Stress_Detection
freq_rel_Stress_Detection=prop.table(freq_ass_Stress_Detection)
freq_rel_Stress_Detection
freq_per_Stress_Detection=prop.table(freq_ass_Stress_Detection)*100
freq_per_Stress_Detection
moda_Stress_Detection=which.max(table(liv_stress))
moda_Stress_Detection
quantile_Stress_Detection= quantile(as.integer(liv_stress),type=1)
quantile_Stress_Detection
ggplot_Stress_Detection=ggplot(Stress, aes(x = liv_stress)) +
  geom_bar(aes(fill = liv_stress))+
  labs(
    x = "stress detection",
    y = "Frequenza assoluta",
    title = "stress detection barplot")
ggplot_Stress_Detection

#VAR QUANTTATIVE
summary_age=summary(Age)
summary_age
var_age=var(Age)
sd_age=sd(Age)
sd_age
Istog_age = hist(Age,seq(from=18,to=60,by=6),col="yellow", xlab ="age",main ="Age histogram")
abline(v=mean(Age), col="coral", lwd=4, lty=1)
abline(v=median(Age), col="black", lwd=4, lty=2)
freq_ass_age= Istog_age$counts
freq_ass_age
freq_rel_age=freq_ass_age/sum(freq_ass_age)
freq_per_age=freq_rel_age*100
freq_per_age
boxplot_age=boxplot(Age, ylab= "Age",main="boxplot age",col="yellow")

summary_sleep_duration=summary(Sleep_Duration)
summary_sleep_duration
var_sleep_duration=var(Sleep_Duration)
sd_sleep_duration=sd(Sleep_Duration)
sd_sleep_duration
Istog_sleep_duration = hist(Sleep_Duration,col="blue", xlab ="sleep duration",main ="sleep duration histogram")
abline(v=mean(Sleep_Duration), col="coral", lwd=4, lty=1)
abline(v=median(Sleep_Duration), col="black", lwd=4, lty=2)
boxplot_sleep_duration=boxplot(Sleep_Duration, ylab= "sleep duration",main="boxplot sleep duration",col="blue")
test_sleep_duration=t.test(Sleep_Duration, y = NULL, alternative = "two.sided", mu = 0, conf.level = 0.95)
test_sleep_duration #t.test() solo per estrarre l'IC, ignorando il resto- ovvio che la durata è diversa da 0

summary_sleep_quality=summary(Sleep_Quality)
summary_sleep_quality
var_sleep_quality=var(Sleep_Quality)
sd_sleep_quality=sd(Sleep_Quality)
sd_sleep_quality
Istog_sleep_quality = hist(Sleep_Quality,col="purple", xlab ="sleep quality",main ="sleep quality histogram")
abline(v=mean(Sleep_Quality), col="coral", lwd=4, lty=1)
abline(v=median(Sleep_Quality), col="black", lwd=4, lty=2)
boxplot_sleep_quality=boxplot(Sleep_Quality, ylab= "sleep quality",main="boxplot sleep quality",col="purple")
test_sleep_quality=t.test(Sleep_Quality, y = NULL, alternative = "two.sided", mu = 0, conf.level = 0.95)
test_sleep_quality

summary_physical_activity=summary(Physical_Activity)
summary_physical_activity
var_physical_activity=var(Physical_Activity)
sd_physical_activity=sd(Physical_Activity)
sd_physical_activity
Istog_physical_activity = hist(Physical_Activity,col="pink", xlab ="physical activity",main ="physical activity histogram")
abline(v=mean(Physical_Activity), col="coral", lwd=4, lty=1)
abline(v=median(Physical_Activity), col="black", lwd=4, lty=2)
boxplot_physical_activity=boxplot(Physical_Activity, ylab= "physical activity",main="boxplot physical activity",col="pink")
test_physical_activity=t.test(Physical_Activity, y = NULL, alternative = "two.sided", mu = 0, conf.level = 0.95)
test_physical_activity

summary_screen_time=summary(Screen_Time)
summary_screen_time
var_screen_time=var(Screen_Time)
sd_screen_time=sd(Screen_Time)
sd_screen_time
Istog_screen_time = hist(Screen_Time,col="red", xlab ="screen time",main ="Screen time histogram")
abline(v=mean(Screen_Time), col="coral", lwd=4, lty=1)
abline(v=median(Screen_Time), col="black", lwd=4, lty=2)
boxplot_screen_time=boxplot(Screen_Time, ylab= "screen time",main="boxplot screen time",col="red")
test_screen_time=t.test(Screen_Time, y = NULL, alternative = "two.sided", mu = 0, conf.level = 0.95)
test_screen_time

summary_caffeine_intake=summary(Caffeine_Intake)
summary_caffeine_intake
var_caffeine_intake=var(Caffeine_Intake)
sd_caffeine_intake=sd(Caffeine_Intake)
sd_caffeine_intake
barplot(table(Caffeine_Intake), col = "orange", xlab = "Livello di Caffeina", ylab = "Frequenza", main = "Distribuzione dell'assunzione di caffeina")
freq_ass_caffeine_intake=table(Caffeine_Intake)
freq_ass_caffeine_intake
boxplot_caffeine_intake=boxplot(Caffeine_Intake, ylab= "Caffeine intake",main="boxplot caffeine intake",col="orange")


summary_work_hours=summary(Work_Hours)
summary_work_hours
var_work_hours=var(Work_Hours)
sd_work_hours=sd(Work_Hours)
sd_work_hours
Istog_work_hours= hist(Work_Hours,breaks=8,col="lightblue", xlab ="work hours",main ="work hours histogram")
abline(v=mean(Work_Hours), col="coral", lwd=4, lty=1)
abline(v=median(Work_Hours), col="black", lwd=4, lty=2)
freq_ass_work_hours= Istog_work_hours$counts
freq_ass_work_hours
freq_rel_work_hours=freq_ass_work_hours/sum(freq_ass_work_hours)
freq_per_work_hours=freq_rel_work_hours*100
freq_per_work_hours
boxplot_work_hours=boxplot(Work_Hours, ylab= "Work hours",main="boxplot work hours",col="lightblue")


summary_blood_pressure=summary(Blood_Pressure)
summary_blood_pressure
var_blood_pressure=var(Blood_Pressure)
sd_blood_pressure=sd(Blood_Pressure)
sd_blood_pressure
Istog_blood_pressure= hist(Blood_Pressure,breaks=12,col="green", xlab ="blood pressure",main ="blood pressure histogram")
abline(v=mean(Blood_Pressure), col="coral", lwd=4, lty=1)
abline(v=median(Blood_Pressure), col="black", lwd=4, lty=2)
freq_ass_blood_pressure= Istog_blood_pressure$counts
freq_ass_blood_pressure
freq_rel_blood_pressure=freq_ass_blood_pressure/sum(freq_ass_blood_pressure)
freq_per_blood_pressure=freq_rel_blood_pressure*100
freq_per_blood_pressure
boxplot_blood_pressure=boxplot(Blood_Pressure, ylab= "blood pressure",main="boxplot blood pressure",col="green")


summary_blood_sugar_level=summary(Blood_Sugar_Level)
summary_blood_sugar_level
var_blood_sugar_level=var(Blood_Sugar_Level)
sd_blood_sugar_level=sd(Blood_Sugar_Level)
sd_blood_sugar_level
Istog_blood_sugar_level= hist(Blood_Sugar_Level,breaks=14,col="darkgreen", xlab ="blood sugar level",main ="blood sugar level histogram")
abline(v=mean(Blood_Sugar_Level), col="coral", lwd=4, lty=1)
abline(v=median(Blood_Sugar_Level), col="black", lwd=4, lty=2)
freq_ass_blood_sugar_level= Istog_blood_sugar_level$counts
freq_ass_blood_sugar_level
freq_rel_blood_sugar_level=freq_ass_blood_sugar_level/sum(freq_ass_blood_sugar_level)
freq_per_blood_sugar_level=freq_rel_blood_sugar_level*100
freq_per_blood_sugar_level
boxplot_blood_sugar_level=boxplot(Blood_Sugar_Level, ylab= "blood sugar level",main="boxplot blood sugar level",col="darkgreen")



#relazione tra 2 qualitative  
#distribuzione doppia di frequenze assolute 
stress_genere = table(liv_stress, Gender)
stress_genere
prop.table(stress_genere)
prop.table(stress_genere)*100
chi=chisq.test(liv_stress, Gender)
print(chi)
chi$expected 
(chi$residuals)^2
ggplot(Stress, aes(liv_stress)) + 
  geom_bar(aes(fill = Gender))+
  labs(
    x = "liv stress",
    y = "Frequenza assoluta",
    title = "Barplot del genere rispetto al livello di stress")


stress_statocivile = table(liv_stress, Marital_Status)
stress_statocivile
prop.table(stress_statocivile)
prop.table(stress_statocivile)*100
chi2=chisq.test(liv_stress, Marital_Status)
chi2
chi2$expected
(chi2$residuals)^2
ggplot(Stress, aes(liv_stress)) + 
  geom_bar(aes(fill = Marital_Status))+
  labs(
    x = "liv stress",
    y = "Frequenza assoluta",
    title = "Barplot dello stato civile rispetto al livello di stress")


stress_fumo = table(liv_stress, Smoking_Habit)
stress_fumo
prop.table(stress_fumo)
prop.table(stress_fumo)*100
chi3=chisq.test(liv_stress, Smoking_Habit)
chi3
chi3$expected 
(chi3$residuals)^2
ggplot(Stress, aes(liv_stress)) + 
  geom_bar(aes(fill = Smoking_Habit))+
  labs(
    x = "liv stress",
    y = "Frequenza assoluta",
    title = "Barplot dell'abitudine al fumo rispetto al livello di stress")

#RELAZIONE QUANTITATIVA-QUALITATIVA

medieD_S=tapply(Sleep_Duration, liv_stress, mean)
medieD_S
barplot((medieD_S),main= "Barplot della durata del sonno media rispetto al livello di stress", col="lightgreen")
ggplot(Stress, aes(x = liv_stress, y = Sleep_Duration)) +
  geom_boxplot(fill = "violet") +
  labs(title = "boxplot della durata media di ore di sonno rispetto al livello di stress", y = "ore", x = "livello di stress")
model_anova=anova(lm(formula = Sleep_Duration ~ liv_stress))
model_anova


medieQ_S=tapply(Sleep_Quality, liv_stress, mean)
medieQ_S
barplot((medieQ_S),main= "Barplot della qualità del sonno media rispetto al livello di stress", col="coral")
ggplot(Stress, aes(x = liv_stress, y = Sleep_Quality)) +
  geom_boxplot(fill = "gold") +
  labs(title = "boxplot della qualità del sonno rispetto al livello di stress",
       y = "qualità", x = "livello di stress")
model_anova2=anova(lm(formula = Sleep_Quality ~ liv_stress))
model_anova2



medieW_S=tapply(Work_Hours, liv_stress, mean)
medieW_S
barplot((medieW_S),main= "Barplot delle ore lavorative giornaliere medie rispetto al livello di stress", col="pink")
ggplot(Stress, aes(x = liv_stress, y = Work_Hours)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "boxplot delle ore di lavoro rispetto al livello di stress",
       y = "ore", x = "livello di stress")
model_anova3=anova(lm(formula = Work_Hours ~ liv_stress))
model_anova3

medieP_S=tapply(Physical_Activity, liv_stress, mean)
medieP_S
barplot((medieP_S),main= "Barplot dell'attività fisica media rispetto al livello di stress", col="brown")
ggplot(Stress, aes(x = liv_stress, y = Physical_Activity)) +
  geom_boxplot(fill = "darkorange") +
  labs(title = "boxplot dell'attività fisica rispetto al livello di stress",
       y = "attività fisica", x = "livello di stress")
model_anova4=anova(lm(formula = Physical_Activity ~ liv_stress))
model_anova4

medieS_C=tapply(Caffeine_Intake,Smoking_Habit, mean)
medieS_C
barplot((medieS_C),main= "Barplot dell'assunzione di caffeina media giornaliera rispetto all'abitudine al fumo", col="darkgreen")
ggplot(Stress, aes(y = Caffeine_Intake, x= Smoking_Habit)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "boxplot del liv di caffeina assunto rispetto all'abitudine al fumo ",
       x = "fumo", y = "liv di caffeina assunta")
model_anova5=anova(lm(formula = Caffeine_Intake ~ Smoking_Habit))
model_anova5

#RELAZIONE TRA DUE QUANTITATIVE 
plot(Age, Blood_Pressure,pch=1, col=1,lwd=2)
cor(Age, Blood_Pressure)

plot(Blood_Sugar_Level,Blood_Pressure,pch=1, col=1,lwd=2)
cor(Blood_Sugar_Level,Blood_Pressure)

plot(Sleep_Duration,Sleep_Quality,pch=1, col=1,lwd=2)
cor(Sleep_Duration,Sleep_Quality)

plot(Sleep_Quality,Physical_Activity,pch=1, col=1,lwd=2)
cor(Sleep_Quality,Physical_Activity)

plot(Sleep_Duration,Work_Hours, pch=1, col=1,lwd=2)
cor(Sleep_Duration,Work_Hours)

plot(Sleep_Duration,Caffeine_Intake, pch=1, col=1,lwd=2)
cor(Sleep_Duration,Caffeine_Intake)

#DUE QUANTITATIVE RISPETTO AD UNA QUALITATIVA 
Gender_factor=factor(Gender)
Stress_numeric=as.integer(liv_stress)
Gender_numeric=as.integer(Gender_factor)
#LOW=NERO, MEDIUM=ROSSO, HIGH=VERDE
plot(Sleep_Duration, Sleep_Quality, pch=Stress_numeric, col=Stress_numeric, lwd=2)
plot(Sleep_Duration, Caffeine_Intake, pch=Stress_numeric, col=Stress_numeric, lwd=2)
plot(Age, Blood_Pressure,  pch=Gender_numeric, col=Gender_numeric, lwd=2)

#CLUSTER ANALYSIS CONFERMATIVA
table(liv_stress)
detach(Stress)
Stress2=Stress[,-c(2,3,4,10,14)]
aggregate(Stress2, list(liv_stress),mean)
StressZ=scale(Stress2)
distStress =dist(StressZ, method="euclidean")

#metodi gerarchici

Stress_dendro_single = hclust(distStress,method="single")
plot(Stress_dendro_single)
rect.hclust(Stress_dendro_single, k=3)
SS3 = cutree(Stress_dendro_single, k=3)
SS3
table(SS3)

Stress_dendro_average = hclust(distStress,method="average")
plot(Stress_dendro_average)
rect.hclust(Stress_dendro_average, k=3)
AS3 = cutree(Stress_dendro_average, k=3)
AS3
table(AS3)

Stress_dendro_complete = hclust(distStress,method="complete")
plot(Stress_dendro_complete)
rect.hclust(Stress_dendro_complete, k=3)
CS3 = cutree(Stress_dendro_complete, k=3)
CS3
table(CS3)

Stress_dendro_centroid = hclust(distStress,method="centroid")
plot(Stress_dendro_centroid)
rect.hclust(Stress_dendro_centroid, k=3)
CES3 = cutree(Stress_dendro_centroid, k=3)
CES3
table(CES3)

Stress_dendro_Ward = hclust(distStress,method="ward.D")
plot(Stress_dendro_Ward)
rect.hclust(Stress_dendro_Ward, k=3)
WS3 = cutree(Stress_dendro_Ward, k=3)
WS3
table(WS3)

aggregate(Stress2, list(liv_stress),mean)
#sembra che le differenze sono dovute essenzialmete al livello di caffeina, alla pressione sanguigna, al livello di zucchero nel sangue ed allo screen time
StressZridotto=StressZ[ , -c(1,2,3,4,7)]

distStress2 =dist(StressZridotto, method="euclidean")

#utilizziamo solo i metodi che ci avevano fornito un risultato migliore

Stress_dendro_complete2 = hclust(distStress2,method="complete")
plot(Stress_dendro_complete2)
rect.hclust(Stress_dendro_complete2, k=3)
CS3_2 = cutree(Stress_dendro_complete2, k=3)
CS3_2
table(CS3_2)

Stress_dendro_Ward2 = hclust(distStress2,method="ward.D")
plot(Stress_dendro_Ward2)
rect.hclust(Stress_dendro_Ward2, k=3)
WS3_2 = cutree(Stress_dendro_Ward2, k=3)
WS3_2
table(WS3_2) 

#SCELTA DEL NUMERO OTTIMALE DI CLUSTER TRAMITE NbClust
library(NbClust)

ResClustering_C = NbClust(data = StressZ,  distance = "euclidean", 
                          min.nc = 2, max.nc = 5, method = "complete", index = "all", alphaBeale = 0.0)
ResClustering_C$Best.nc
table(ResClustering_C$Best.nc[1,])

ResClustering_W = NbClust(data = StressZ,  distance = "euclidean", 
                          min.nc = 2, max.nc = 5, method = "ward.D", index = "all", alphaBeale = 0.0)
ResClustering_W$Best.nc
table(ResClustering_W$Best.nc[1,])

#metodo del gomito
library(tidyverse)
wss <- function(k) {
  kmeans(StressZ, k, nstart = 10 )$tot.withinss
}
k.values = 2:5
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

ResClustering_K = NbClust(data = StressZ,  distance = "euclidean", 
                          min.nc = 2, max.nc = 5, method = "kmeans", index = "all")
ResClustering_K$Best.nc
table(ResClustering_K$Best.nc[1,])

#dataset ridotto

ResClustering_W_2 = NbClust(data = StressZridotto,  distance = "euclidean", 
                            min.nc = 2, max.nc = 5, method = "ward.D", index = "all", alphaBeale = 0.0)
ResClustering_W_2$Best.nc
table(ResClustering_W_2$Best.nc[1,])

ResClustering_C_2 = NbClust(data =StressZridotto,  distance = "euclidean", 
                            min.nc = 2, max.nc = 5, method = "complete", index = "all", alphaBeale = 0.0)
ResClustering_C_2$Best.nc
table(ResClustering_C_2$Best.nc[1,])

#metodo del gomito
wss <- function(k) {
  kmeans(StressZridotto, k, nstart = 10 )$tot.withinss
}
k.values = 2:5
wss_values = map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


ResClustering_K2 = NbClust(data = StressZridotto,  distance = "euclidean", 
                           min.nc = 2, max.nc = 5, method = "kmeans", index = "all")
ResClustering_K2$Best.nc
table(ResClustering_K2$Best.nc[1,])

#ANALISI PREVISIVA CON K MEANS SU SPARK

StressZ_ML=as.data.frame(StressZ)
StressZ_ML = cbind(StressZ_ML, Stress[ , 14, drop = FALSE])
str(StressZ_ML)

library(sparklyr)
sc= spark_connect(master="local")
stress_tbl = copy_to(sc, StressZ_ML, "stressdata", overwrite = TRUE)

#suddivisione dei dati in training e test
partitionsStress=stress_tbl %>%
  sdf_random_split(training = 0.7, test = 0.3, seed = 1381)
stress_training = partitionsStress$training
stress_test =partitionsStress$test

library(dplyr)
count(stress_training)
count(stress_test)

stress_group= stress_training %>%
  group_by(Stress_Detection) %>%
  count()%>%
  collect() %>%
  print()
stress_group2= stress_test %>%
  group_by(Stress_Detection) %>%
  count()%>%
  collect() %>%
  print()

#Modello k means su training
kmeans_model2 = stress_training %>%
  ml_kmeans(k = 3, Stress_Detection ~ .)

output2 = ml_summary(kmeans_model2)

output2$predictions %>%
  select(,Stress_Detection,features, label, prediction) %>%
  collect() %>%
  print(n = 50)
# Dalla colonna label capiamo che 0=high, 1=medium ,2=Low

tidy(kmeans_model2) %>% print(width = Inf)

#Fase di previsione su test
predicted2 = ml_predict(kmeans_model2, stress_test) %>%
  collect()

table(predicted2$Stress_Detection)
table(predicted2$label)
table(predicted2$prediction)

tabellaConfusione =table(predicted2$label, predicted2$prediction)
colnames(tabellaConfusione) = c("Medium", "High", "Low")
rownames(tabellaConfusione) = c("Medium", "High", "Low")
tabellaConfusione

error_rate=sum(tabellaConfusione)-sum(diag(tabellaConfusione))
accuracy=1-error_rate/sum(tabellaConfusione)
accuracy

aggregate(Stress2, list(liv_stress),mean)
#dataset ridotto

StressZridotto=StressZ[ , -c(1,2,3,4,7)]
StressZ_ML_ridotto=as.data.frame(StressZridotto)
StressZ_ML_ridotto = cbind(StressZ_ML_ridotto, Stress[ , 14, drop = FALSE])
str(StressZ_ML_ridotto)


stress_tbl2 = copy_to(sc, StressZ_ML_ridotto, "stressdata2", overwrite = TRUE)

partitionsStress=stress_tbl2 %>%
  sdf_random_split(training = 0.7, test = 0.3, seed = 1381)
stress_training = partitionsStress$training
stress_test =partitionsStress$test

count(stress_training)
count(stress_test)

stress_group= stress_training %>%
  group_by(Stress_Detection) %>%
  count()%>%
  collect() %>%
  print()
stress_group2= stress_test %>%
  group_by(Stress_Detection) %>%
  count()%>%
  collect() %>%
  print()

kmeans_model2 <- stress_training %>%
  ml_kmeans(k = 3, Stress_Detection ~ .)

output2 = ml_summary(kmeans_model2)

output2$predictions %>%
  select(,Stress_Detection,features, label, prediction) %>%
  collect() %>%
  print(n = 50)

#Dalla colonna label capiamo che 0=medium, 1=High ,2=Low

tidy(kmeans_model2) %>% print(width = Inf)

predicted2 = ml_predict(kmeans_model2, stress_test) %>%
  collect()

table(predicted2$Stress_Detection)
table(predicted2$label)
table(predicted2$prediction)

tabellaConfusione =table(predicted2$label, predicted2$prediction)
colnames(tabellaConfusione)= c("Medium", "High", "Low")
rownames(tabellaConfusione)= c("Medium", "High", "Low")
tabellaConfusione

error_rate=sum(tabellaConfusione)-sum(diag(tabellaConfusione))
accuracy=1-error_rate/sum(tabellaConfusione)
accuracy

