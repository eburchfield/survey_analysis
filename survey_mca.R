library(FactoMineR)
library(missMDA)
library(factoextra)
library(corrplot)

#create df from full survey data
#data <- data.frame(A = c1$ADP1_A1, B = c1$ADP1_B1, C = c1$ADP1_C1, D = c1$ADP1_D1_1, E = c1$ADP1_E1,
#             F = c1$ADP1_F1, G = c1$ADP1_G1_1, H = c1$ADP1_H1)


#for now predicting HAVE practiced
ADP1 <- data.frame(bethma = c1$ADP1_B1, kakulan = c1$ADP1_B2, OFC = c1$ADP1_B3, water_reuse = c1$ADP1_B4, sd = c1$ADP1_B5,
                     parachute = c1$ADP1_B6, transplant = c1$ADP1_B7, awd = c1$ADP1_B8, low_flood = c1$ADP1_B9, sri = c1$ADP1_B10,
                     agrowell_user = c1$agrowell_user, agrowell_owner = c1$agrowell_owner, female = c1$female, minority = c1$minority,
                     land_owner = c1$owner, head_end = c1$head_end, diverse = c1$diverse_flag, fo = c1$fo, major = c1$major_flag, 
                     rf = c1$rf, minor = c1$minor, from_GN = c1$HH_C.1, farming_family = c1$HH_G.1)

ADP1 <- data.frame(lapply(ADP1, as.factor), stringsAsFactors = T)

#interpretation

ADP1$from_GN <- ifelse(ADP1$from_GN == 1, "from GN", "from outside GN")
ADP1$farming_family <- ifelse(ADP1$farming_family == 1, "farming family", "non-farming family")
ADP1$bethma <- ifelse(ADP1$bethma == 1,"bethma practiced", "bethma not practiced")
ADP1$kakulan <- ifelse(ADP1$kakulan == 1, "kakulam practiced", "kakulan not practiced")
ADP1$OFC <- ifelse(ADP1$OFC == 1, "OFC cultivated", "OFC not cultivated")
ADP1$water_reuse <- ifelse(ADP1$water_reuse == 1, "water reuse practiced", "water reuse not practiced")
ADP1$sd <- ifelse(ADP1$sd == 1, "short duration seeds practiced", "short duration seeds not practiced")
ADP1$parachute <- ifelse(ADP1$parachute == 1, "parachute method practiced", "parachute method not practiced")
ADP1$transplant <- ifelse(ADP1$transplant == 1, "transplanting practiced", "transplanting not practiced")
ADP1$awd <- ifelse(ADP1$awd == 1, "AWD practiced", "AWD not practiced")
ADP1$low_flood <- ifelse(ADP1$low_flood == 1, "low flooding practiced", "low flooding not practiced")
ADP1$sri <- ifelse(ADP1$sri == 1, "sri practiced", "sri not practiced")
ADP1$agrowell_user <- ifelse(ADP1$agrowell_user == 1, "uses agrowell", "does not use agrowell")
ADP1$agrowell_owner <- ifelse(ADP1$agrowell_owner == 1, "owns agrowell", "does not own agrowell")
ADP1$female <- ifelse(ADP1$female == 1, "female", "male")
ADP1$minority <- ifelse(ADP1$minority == 1, "ethnic minority", "sinhalese")
ADP1$land_owner <- ifelse(ADP1$land_owner == 1, "land owner", "does not own land")
ADP1$head_end <- ifelse(ADP1$head_end == 1, "plots at head_end", "plots at middle- or tail-end")
ADP1$diverse <- ifelse(ADP1$diverse == 1, "diverse land use", "monoculture")
ADP1$fo <- ifelse(ADP1$fo == 1, "fo member", "not fo member")
ADP1$major <- ifelse(ADP1$major == 1, "major system", "not major system")
ADP1$rf <- ifelse(ADP1$rf == 1, "rf system", "not rf system")
ADP1$minor <- ifelse(ADP1$minor == 1, "minor system", "not minor system")

ADP1 <- data.frame(lapply(ADP1, as.factor), stringsAsFactors = T)

#nb <- estim_ncpMCA(as.data.frame(ADP1), ncp.max = 5)
res.mca <- MCA(as.data.frame(ADP1), graph = T, na.method = "NA")

#data display
#http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
dim <- dimdesc(res.mca)
summary(res.mca, nb.dec = 2, ncp = 3, nbelements = 25)  #file = my_file.txt to export
ev <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca)
var<- get_mca_var(res.mca)

plot(res.mca, choix = "var")
#corr plot
corrplot(var$contrib, is.corr = F)

#contribution of variables to dim 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 20) +   
               theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20), axis.text = element_text(size = 20), 
                     axis.title = element_text(size = 20), plot.title = element_text(size = 20))
               

#individual contribution
fviz_mca_ind(res.mca, label = "none", habillage = ADP1$major, addEllipses = T, ellipse.level = 0.95)

#ellipse
plotellipses(res.mca, keepvar = 1:4)
