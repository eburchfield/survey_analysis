require(dplyr)
require(gdata)
require(ggplot2)
require(lme4)
require(rjags)
require(ggmcmc)
require(BEST)
require(foreign)

#number of fields
fields <- c1[,c("LAN1B.01", "LAN1B.02", "LAN1B.03", "LAN1B.04", "LAN1B.05", "LAN1B.06", "LAN1B.07", "LAN1B.08" )]
field.count <- rowSums(!is.na(fields))
c1$fieldcount <- field.count


#number of plots of each type
pf.count <- rowSums(fields == 1, na.rm = T)
hl.count <- rowSums(fields == 2, na.rm = T)
hg.count <- rowSums(fields == 3, na.rm = T)
chena.count <- rowSums(fields == 4, na.rm = T)
c1$paddy.count <- pf.count
c1$highland.count <- hl.count
c1$hg.count <- hg.count
c1$chena.count <- chena.count


#diversification
pf.flag <- ifelse(pf.count > 0, 1, 0)
hl.flag <- ifelse(hl.count > 0, 1, 0)
hg.flag <- ifelse(hg.count > 0, 1, 0)
ch.flag <- ifelse(chena.count > 0, 1, 0)
diverse <- pf.flag + hl.flag + hg.flag + ch.flag
c1$diverse_score <- diverse
diverse.flag <- ifelse(diverse > 1, 1, 0)
c1$diverse_flag <- diverse.flag


#position of paddy fields
#he = 1, me = 2, te = 3
positions <- c1[,c("LAN1L.01", "LAN1L.02", "LAN1L.03", "LAN1L.04", "LAN1L.05", "LAN1L.06", "LAN1L.07", "LAN1L.08" )]
paddy.position <- data.frame(f1 = numeric(607), f2 = numeric(607), f3 = numeric(607), f4 = numeric(607), f5 = numeric(607), f6 = numeric(607), f7 = numeric(607), f8 = numeric(607))
for (c in 1:8)  {
  for (r in 1:607) {
    out <- ifelse(fields[r,c] == 1 & (!is.na(fields[r,c])), positions[r,c], NA)
    paddy.position[r,c] = out
    }}
f1pos <- paddy.position$f1
f2pos <- paddy.position$f2
f3pos <- paddy.position$f3


#cultivates on head-end field
he <- apply(paddy.position[,], MARGIN = 1, function(x) any(x == 1))
ifelse(he, 1, 0)
he[is.na(he)] <- 0
c1$head_end <- he
te <- apply(paddy.position[,], MARGIN = 1, function(x) any(x == 3))
ifelse(te, 1, 0)
te[is.na(te)] <- 0
c1$tail_end <- te

#minority flag (1 = S, 2 = T)
sinhalese <- ifelse(c1$HH_K.1 == 1, 1,0)
c1$sinhalese <- sinhalese


#female flag (1 = M, 2 = F)
female <- ifelse(c1$HH2_D.01 == 2, 1, 0)
c1$female <- female

#ownership majority of fields listed (farmer, government, non-owner)
ownership <- c1[,c("LAN1D.01", "LAN1D.02", "LAN1D.03", "LAN1D.04", "LAN1D.05", "LAN1D.06", "LAN1D.07", "LAN1D.08" )]
owner <- rowSums(ownership == 1, na.rm = T)/field.count
gvt_owner <- rowSums(ownership == 2, na.rm = T)/field.count
non_owner <- rowSums(ownership > 2, na.rm = T)/field.count
c1$owner <- ifelse(owner > .5, 1, 0)
c1$gvt_owner <- ifelse(gvt_owner > .5, 1, 0)
c1$non_owner <- ifelse(non_owner > .5, 1, 0)


#majority of fields receive irrigation water from (major, minor, majon_minor, rf)
irrigation <- c1[,c("LAN1H_1.01", "LAN1H_1.02", "LAN1H_1.03", "LAN1H_1.04", "LAN1H_1.05", "LAN1H_1.06", "LAN1H_1.07", "LAN1H_1.08" )]
major <- (rowSums(irrigation == 1, na.rm = T) + rowSums(irrigation == 2, na.rm = T))/field.count
minor <- (rowSums(irrigation == 5, na.rm = T) + rowSums(irrigation == 6, na.rm = T))/field.count 
major_minor <- (rowSums(irrigation == 3, na.rm = T) + rowSums(irrigation == 4, na.rm = T))/field.count
rainfed <- rowSums(irrigation == 8, na.rm = T)/field.count
c1$major <- ifelse(major > .5, 1, 0)
c1$minor <- ifelse(minor > .5, 1, 0)
c1$major_minor <- ifelse(major_minor > .5, 1, 0)
c1$rf <- ifelse(rainfed > .5, 1, 0)

#major irrigation flag
c1$major_flag = c1$major + c1$major_minor

#agrowell user
agrowell_user <- rowSums(irrigation == 7, na.rm = T)
c1$agrowell_user <- ifelse(agrowell_user > 0, 1,0)


#agrowell owner
agrowell_lands <- c1[,c("LAN3D.1", "LAN3D.2", "LAN3D.3")]
agrowell_owner <- rowSums(agrowell_lands == 1, na.rm = T)
c1$agrowell_owner <- ifelse(agrowell_owner > 0, 1, 0)


#other employment
occupation <- c1[,c("HH2_H.01", "HH2_H.02")]
second_oc <- rowSums(occupation != 611, na.rm = T)
c1$second_oc <- ifelse(second_oc > 0, 1, 0)


#past bethma
c1$past_bm <- ifelse(c1$ADP1_B1 == 1, 1, 0)


#fo membership
c1$fo <- ifelse(c1$SAT1_1 == 1, 1, 0)  #note that there were entries with value of 3, not sure what this meant, counted it as no

#practiced in Y2014
c1$bm_2014 <- ifelse(c1$ADP1_C1 == 2, 1, ifelse(c1$ADP1_C1 == 3, 1, 0))

#write out dataframe
#save(c1, file="c1_data_full.Rda")

