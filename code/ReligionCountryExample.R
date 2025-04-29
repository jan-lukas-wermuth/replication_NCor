# ============================================================
# Title:      Data Example Country vs Religion
# Author:     Jan-Lukas Wermuth
# Date:       2025-04-29
# Purpose:    This script calculates the correlation between
#             the variables country and religion for a number
#             of border triangles.
# ============================================================
rm(list = ls())

source(file = "/Users/lukaswermuth/Documents/Dr.Wermuth/Rpackages/NCor/R/NCor.R")

library(dplyr)
library(readxl)
library(arrangements)
library(NCor)
library(RCor)
library(doParallel)


# Case Study: Countries vs Religions --------------------------------------
df <- read_xlsx(path = "/Users/lukaswermuth/Library/CloudStorage/Dropbox/Pohle Wermuth/NominalCorrelation/Data/Religionsbycountryin2020.xlsx", skip = 1)

df <- df %>% dplyr::filter(Religion == "Christians" | Religion == "Muslims" | Religion == "Jews")
colnames(df) <- c("Geography type", "Geography", "Religion", "Count", "Selected religion %")
df <- df[,2:4] # Drop "Geography type" and "Selected religion %"

# Aggregate counts for Western Sahara by religion
sahara_counts <- df %>%
  filter(Geography == "Western Sahara")

# Add Sahara counts to the counts of Morocco
df <- df %>%
  # Join the sahara_counts to Morocco rows
  left_join(sahara_counts, by = "Religion", suffix = c("", "_sahara")) %>%
  # Only add counts for Morocco, keep Western Sahara as is
  mutate(count = ifelse(Geography == "Morocco" & !is.na(Count_sahara), Count + Count_sahara, Count)) %>%
  # Drop the extra column
  select(-c(Count_sahara, Count, Geography_sahara)) %>%
  # Filter out Western Sahara rows
  filter(Geography != "Western Sahara")

colnames(df) <- c("Geography", "Religion", "Count")


## Middle America ----------------------------------------------------------
df_BeGuMe <- df %>% dplyr::filter(Geography == "Belize" | Geography == "Guatemala" | Geography == "Mexico")
contingency_table_BeGuMe <- xtabs(Count ~ Geography + Religion, data = df_BeGuMe) / 10
Start_time <- Sys.time()
NCor(contingency_table_BeGuMe, CIs = FALSE, Test = FALSE) # 0.7807341 (13,798,831 observations and 6.539901 days with old inefficient code: new code twice as fast)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BeGuMe) # 0.01068791
TschuprowT(contingency_table_BeGuMe) # 0.01068791
ContCoef(contingency_table_BeGuMe) # 0.01511326
ContCoef(contingency_table_BeGuMe, correct = TRUE) # 0.01850989
GoodmanKruskalTau(contingency_table_BeGuMe) # 0.0001088612
Lambda(contingency_table_BeGuMe) # 0
UncertCoef(contingency_table_BeGuMe) # 0.000580838

df_HoGuEl <- df %>% dplyr::filter(Geography == "Honduras" | Geography == "Guatemala" | Geography == "El Salvador")
contingency_table_HoGuEl <- xtabs(Count ~ Geography + Religion, data = df_HoGuEl) / 10
Start_time <- Sys.time()
NCor(contingency_table_HoGuEl, CIs = FALSE, Test = FALSE) # 0.7418044
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HoGuEl) # 0.01930349
TschuprowT(contingency_table_HoGuEl) # 0.01930349
ContCoef(contingency_table_HoGuEl) # 0.02728909
ContCoef(contingency_table_HoGuEl, correct = TRUE) # 0.03342217
GoodmanKruskalTau(contingency_table_HoGuEl) # 0.0004325693
Lambda(contingency_table_HoGuEl) # 0.0008477922
UncertCoef(contingency_table_HoGuEl) # 0.0006840075

# South America
df_EcCoPe <- df %>% dplyr::filter(Geography == "Ecuador" | Geography == "Colombia" | Geography == "Peru")
contingency_table_EcCoPe <- xtabs(Count ~ Geography + Religion, data = df_EcCoPe) / 10
Start_time <- Sys.time()
NCor(contingency_table_EcCoPe, CIs = FALSE, Test = FALSE) # 0.5875528
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_EcCoPe) # 0.009934964
TschuprowT(contingency_table_EcCoPe) # 0.009934964
ContCoef(contingency_table_EcCoPe) # 0.01404877
ContCoef(contingency_table_EcCoPe, correct = TRUE) # 0.01720616
GoodmanKruskalTau(contingency_table_EcCoPe) # 0.0001242691
Lambda(contingency_table_EcCoPe) # 2.042118e-06
UncertCoef(contingency_table_EcCoPe) # 0.0002358791

df_BrCoPe <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Colombia" | Geography == "Peru")
contingency_table_BrCoPe <- xtabs(Count ~ Geography + Religion, data = df_BrCoPe) / 20
Start_time <- Sys.time()
NCor(contingency_table_BrCoPe, CIs = FALSE, Test = FALSE) # 0.561392 (pop size: 13,729,172: 4.024758 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrCoPe) # 0.0108985
TschuprowT(contingency_table_BrCoPe) # 0.0108985
ContCoef(contingency_table_BrCoPe) # 0.01541097
ContCoef(contingency_table_BrCoPe, correct = TRUE) # 0.01887451
GoodmanKruskalTau(contingency_table_BrCoPe) # 0.0001515269
Lambda(contingency_table_BrCoPe) # 0
UncertCoef(contingency_table_BrCoPe) # 0.0004138041

df_BrCoVe <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Colombia" | Geography == "Venezuela")
contingency_table_BrCoVe <- xtabs(Count ~ Geography + Religion, data = df_BrCoVe) / 100
Start_time <- Sys.time()
NCor(contingency_table_BrCoVe, CIs = FALSE, Test = FALSE) # 0.4558306
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrCoVe) # 0.0169125
TschuprowT(contingency_table_BrCoVe) # 0.0169125
ContCoef(contingency_table_BrCoVe) # 0.02391104
ContCoef(contingency_table_BrCoVe, correct = TRUE) # 0.02928493
GoodmanKruskalTau(contingency_table_BrCoVe) # 0.0001873878
Lambda(contingency_table_BrCoVe) # 0
UncertCoef(contingency_table_BrCoVe) # 0.0005922764

df_BrVeGu <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Venezuela" | Geography == "Guyana")
contingency_table_BrVeGu <- xtabs(Count ~ Geography + Religion, data = df_BrVeGu) / 2
Start_time <- Sys.time()
NCor(contingency_table_BrVeGu, CIs = FALSE, Test = FALSE) # (pop size: 110,406,831)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrVeGu) # 0.09993574
TschuprowT(contingency_table_BrVeGu) # 0.09993574
ContCoef(contingency_table_BrVeGu) # 0.1399398
ContCoef(contingency_table_BrVeGu, correct = TRUE) # 0.1713905
GoodmanKruskalTau(contingency_table_BrVeGu) # 0.001040657
Lambda(contingency_table_BrVeGu) # 0
UncertCoef(contingency_table_BrVeGu) # 0.005595646

df_BrGuSu <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Guyana" | Geography == "Suriname")
contingency_table_BrGuSu <- xtabs(Count ~ Geography + Religion, data = df_BrGuSu) / 2
Start_time <- Sys.time()
NCor(contingency_table_BrGuSu, CIs = FALSE, Test = FALSE) # (pop size: 97,364,336)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrGuSu) # 0.1985194
TschuprowT(contingency_table_BrGuSu) # 0.1985194
ContCoef(contingency_table_BrGuSu) # 0.2702984
ContCoef(contingency_table_BrGuSu, correct = TRUE) # 0.3310466
GoodmanKruskalTau(contingency_table_BrGuSu) # 0.05451455
Lambda(contingency_table_BrGuSu) # 0
UncertCoef(contingency_table_BrGuSu) # 0.1231787

# df_BrSuFr <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Suriname" | Geography == "French Guiana")
# contingency_table_BrSuFr <- xtabs(Count ~ Geography + Religion, data = df_BrSuFr) / 10
# Start_time <- Sys.time()
# NCor(contingency_table_BrSuFr, CIs = FALSE, Test = FALSE)
# End_time <- Sys.time()
# End_time - Start_time
# 
# CramerV(contingency_table_BrSuFr) # 0.1876206
# TschuprowT(contingency_table_BrSuFr) # 0.1876206
# ContCoef(contingency_table_BrSuFr) # 0.2564613
# ContCoef(contingency_table_BrSuFr, correct = TRUE) # 0.3140996
# GoodmanKruskalTau(contingency_table_BrSuFr) # 0.04495967
# Lambda(contingency_table_BrSuFr) # 0
# UncertCoef(contingency_table_BrSuFr) # 0.1029668

df_BrBoPe <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Bolivia" | Geography == "Peru")
contingency_table_BrBoPe <- xtabs(Count ~ Geography + Religion, data = df_BrBoPe) / 10
Start_time <- Sys.time()
NCor(contingency_table_BrBoPe, CIs = FALSE, Test = FALSE) # 0.7659425 (pop size: 23,704,625: 14.90609 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrBoPe) # 0.01048599
TschuprowT(contingency_table_BrBoPe) # 0.01048599
ContCoef(contingency_table_BrBoPe) # 0.0148278
ContCoef(contingency_table_BrBoPe, correct = TRUE) # 0.01816028
GoodmanKruskalTau(contingency_table_BrBoPe) # 0.0001712962
Lambda(contingency_table_BrBoPe) # 0
UncertCoef(contingency_table_BrBoPe) # 0.0005836464

df_BrBoPa <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Bolivia" | Geography == "Paraguay")
contingency_table_BrBoPa <- xtabs(Count ~ Geography + Religion, data = df_BrBoPa) / 10
Start_time <- Sys.time()
NCor(contingency_table_BrBoPa, CIs = FALSE, Test = FALSE) # 0.5584812 (pop size: 21,123,524: 14.66205 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrBoPa) # 0.00593042
TschuprowT(contingency_table_BrBoPa) # 0.00593042
ContCoef(contingency_table_BrBoPa) # 0.008386586
ContCoef(contingency_table_BrBoPa, correct = TRUE) # 0.01027143
GoodmanKruskalTau(contingency_table_BrBoPa) # 5.309463e-05
Lambda(contingency_table_BrBoPa) # 0
UncertCoef(contingency_table_BrBoPa) # 0.000292512

df_BrArPa <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Argentina" | Geography == "Paraguay")
contingency_table_BrArPa <- xtabs(Count ~ Geography + Religion, data = df_BrArPa) / 10
Start_time <- Sys.time()
NCor(contingency_table_BrArPa, CIs = FALSE, Test = FALSE) # 0.889... (pop size: 24,119,633: 15 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrArPa) # 0.09073317
TschuprowT(contingency_table_BrArPa) # 0.09073317
ContCoef(contingency_table_BrArPa) # 0.1272726
ContCoef(contingency_table_BrArPa, correct = TRUE) # 0.1558764
GoodmanKruskalTau(contingency_table_BrArPa) # 0.01381718
Lambda(contingency_table_BrArPa) # 0.01711374
UncertCoef(contingency_table_BrArPa) # 0.01837438

df_BrArUr <- df %>% dplyr::filter(Geography == "Brazil" | Geography == "Argentina" | Geography == "Uruguay")
contingency_table_BrArUr <- xtabs(Count ~ Geography + Religion, data = df_BrArUr) / 10
Start_time <- Sys.time()
NCor(contingency_table_BrArUr, CIs = FALSE, Test = FALSE) # 0.8855387 (pop size: 23,707,291: 15.22182 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BrArUr) # 0.09157442
TschuprowT(contingency_table_BrArUr) # 0.09157442
ContCoef(contingency_table_BrArUr) # 0.1284332
ContCoef(contingency_table_BrArUr, correct = TRUE) # 0.1572979
GoodmanKruskalTau(contingency_table_BrArUr) # 0.01546116
Lambda(contingency_table_BrArUr) # 0.01868581
UncertCoef(contingency_table_BrArUr) # 0.02084166

df_ChBoPe <- df %>% dplyr::filter(Geography == "Chile" | Geography == "Bolivia" | Geography == "Peru")
contingency_table_ChBoPe <- xtabs(Count ~ Geography + Religion, data = df_ChBoPe) / 10
Start_time <- Sys.time()
NCor(contingency_table_ChBoPe, CIs = FALSE, Test = FALSE) # 0.780991
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ChBoPe) # 0.02359642
TschuprowT(contingency_table_ChBoPe) # 0.02359642
ContCoef(contingency_table_ChBoPe) # 0.03335181
ContCoef(contingency_table_ChBoPe, correct = TRUE) # 0.04084746
GoodmanKruskalTau(contingency_table_ChBoPe) # 0.0006175755
Lambda(contingency_table_ChBoPe) # 0.001158117
UncertCoef(contingency_table_ChBoPe) # 0.001023019

df_ChBoAr <- df %>% dplyr::filter(Geography == "Chile" | Geography == "Bolivia" | Geography == "Argentina")
contingency_table_ChBoAr <- xtabs(Count ~ Geography + Religion, data = df_ChBoAr) / 10
Start_time <- Sys.time()
NCor(contingency_table_ChBoAr, CIs = FALSE, Test = FALSE) # 0.9003676 
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ChBoAr) # 0.07108152
TschuprowT(contingency_table_ChBoAr) # 0.07108152
ContCoef(contingency_table_ChBoAr) # 0.1000204
ContCoef(contingency_table_ChBoAr, correct = TRUE) # 0.1224994
GoodmanKruskalTau(contingency_table_ChBoAr) # 0.006544888
Lambda(contingency_table_ChBoAr) # 0
UncertCoef(contingency_table_ChBoAr) # 0.01346343

df_BoArPa <- df %>% dplyr::filter(Geography == "Bolivia" | Geography == "Argentina" | Geography == "Paraguay")
contingency_table_BoArPa <- xtabs(Count ~ Geography + Religion, data = df_BoArPa) / 10
Start_time <- Sys.time()
NCor(contingency_table_BoArPa, CIs = FALSE, Test = FALSE) # 0.9672459 
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BoArPa) # 0.06380237
TschuprowT(contingency_table_BoArPa) # 0.06380237
ContCoef(contingency_table_BoArPa) # 0.0898651
ContCoef(contingency_table_BoArPa, correct = TRUE) # 0.1100618
GoodmanKruskalTau(contingency_table_BoArPa) # 0.005711228
Lambda(contingency_table_BoArPa) # 0
UncertCoef(contingency_table_BoArPa) # 0.01402566


## Europe ------------------------------------------------------------------
df_NeBeGe <- df %>% dplyr::filter(Geography == "Netherlands" | Geography == "Belgium" | Geography == "Germany")
contingency_table_NeBeGe <- xtabs(Count ~ Geography + Religion, data = df_NeBeGe) / 100
Start_time <- Sys.time()
NCor(contingency_table_NeBeGe, CIs = FALSE, Test = FALSE) # 0.1230555
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_NeBeGe) # 0.02526603
TschuprowT(contingency_table_NeBeGe) # 0.02526603
ContCoef(contingency_table_NeBeGe) # 0.03570877
ContCoef(contingency_table_NeBeGe, correct = TRUE) # 0.04373413
GoodmanKruskalTau(contingency_table_NeBeGe) # 0.000878257
Lambda(contingency_table_NeBeGe) # 0
UncertCoef(contingency_table_NeBeGe) # 0.001177904

df_FrSwGe <- df %>% dplyr::filter(Geography == "France" | Geography == "Switzerland" | Geography == "Germany")
contingency_table_FrSwGe <- xtabs(Count ~ Geography + Religion, data = df_FrSwGe) / 200
Start_time <- Sys.time()
NCor(contingency_table_FrSwGe, CIs = FALSE, Test = FALSE) # 0.229921
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_FrSwGe) # 0.06082414
TschuprowT(contingency_table_FrSwGe) # 0.06082414
ContCoef(contingency_table_FrSwGe) # 0.08570184
ContCoef(contingency_table_FrSwGe, correct = TRUE) # 0.1049629
GoodmanKruskalTau(contingency_table_FrSwGe) # 0.005805636
Lambda(contingency_table_FrSwGe) # 0.01769693
UncertCoef(contingency_table_FrSwGe) # 0.005928165

df_FrSwIt <- df %>% dplyr::filter(Geography == "France" | Geography == "Switzerland" | Geography == "Italy")
contingency_table_FrSwIt <- xtabs(Count ~ Geography + Religion, data = df_FrSwIt) / 100
Start_time <- Sys.time()
NCor(contingency_table_FrSwIt, CIs = FALSE, Test = FALSE) # 0.4115908
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_FrSwIt) # 0.09770329
TschuprowT(contingency_table_FrSwIt) # 0.09770329
ContCoef(contingency_table_FrSwIt) # 0.1368729
ContCoef(contingency_table_FrSwIt, correct = TRUE) # 0.1676344
GoodmanKruskalTau(contingency_table_FrSwIt) # 0.01571566
Lambda(contingency_table_FrSwIt) # 0.0596119
UncertCoef(contingency_table_FrSwIt) # 0.01621297

df_AuSwIt <- df %>% dplyr::filter(Geography == "Austria" | Geography == "Switzerland" | Geography == "Italy")
contingency_table_AuSwIt <- xtabs(Count ~ Geography + Religion, data = df_AuSwIt) / 100
Start_time <- Sys.time()
NCor(contingency_table_AuSwIt, CIs = FALSE, Test = FALSE) # 0.2184952
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AuSwIt) # 0.04040116
TschuprowT(contingency_table_AuSwIt) # 0.04040116
ContCoef(contingency_table_AuSwIt) # 0.05704284
ContCoef(contingency_table_AuSwIt, correct = TRUE) # 0.06986293
GoodmanKruskalTau(contingency_table_AuSwIt) # 0.002168982
Lambda(contingency_table_AuSwIt) # 0
UncertCoef(contingency_table_AuSwIt) # 0.003157655

df_LuFrGe <- df %>% dplyr::filter(Geography == "Luxembourg" | Geography == "France" | Geography == "Germany")
contingency_table_LuFrGe <- xtabs(Count ~ Geography + Religion, data = df_LuFrGe) / 10
Start_time <- Sys.time()
NCor(contingency_table_LuFrGe, CIs = FALSE, Test = FALSE) # 0.2486963
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LuFrGe) # 0.06110729
TschuprowT(contingency_table_LuFrGe) # 0.06110729
ContCoef(contingency_table_LuFrGe) # 0.08609786
ContCoef(contingency_table_LuFrGe, correct = TRUE) # 0.1054479
GoodmanKruskalTau(contingency_table_LuFrGe) # 0.007083558
Lambda(contingency_table_LuFrGe) # 0.01977885
UncertCoef(contingency_table_LuFrGe) # 0.007002734

df_LuFrBe <- df %>% dplyr::filter(Geography == "Luxembourg" | Geography == "France" | Geography == "Belgium")
contingency_table_LuFrBe <- xtabs(Count ~ Geography + Religion, data = df_LuFrBe) / 10
Start_time <- Sys.time()
NCor(contingency_table_LuFrBe, CIs = FALSE, Test = FALSE) # 0.1698809
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LuFrBe) # 0.03162206
TschuprowT(contingency_table_LuFrBe) # 0.03162206
ContCoef(contingency_table_LuFrBe) # 0.0446757
ContCoef(contingency_table_LuFrBe, correct = TRUE) # 0.05471633
GoodmanKruskalTau(contingency_table_LuFrBe) # 0.001378996
Lambda(contingency_table_LuFrBe) # 0
UncertCoef(contingency_table_LuFrBe) # 0.002717926

df_LuGeBe <- df %>% dplyr::filter(Geography == "Luxembourg" | Geography == "Germany" | Geography == "Belgium")
contingency_table_LuGeBe <- xtabs(Count ~ Geography + Religion, data = df_LuGeBe) / 10
Start_time <- Sys.time()
NCor(contingency_table_LuGeBe, CIs = FALSE, Test = FALSE) # 0.1190534
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LuGeBe) # 0.01933028
TschuprowT(contingency_table_LuGeBe) # 0.01933028
ContCoef(contingency_table_LuGeBe) # 0.02732694
ContCoef(contingency_table_LuGeBe, correct = TRUE) # 0.03346853
GoodmanKruskalTau(contingency_table_LuGeBe) # 0.0003927391
Lambda(contingency_table_LuGeBe) # 0
UncertCoef(contingency_table_LuGeBe) # 0.001116434

df_AuSlIt <- df %>% dplyr::filter(Geography == "Austria" | Geography == "Slovenia" | Geography == "Italy")
contingency_table_AuSlIt <- xtabs(Count ~ Geography + Religion, data = df_AuSlIt) / 10
Start_time <- Sys.time()
NCor(contingency_table_AuSlIt, CIs = FALSE, Test = FALSE) # 0.2494335
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AuSlIt) # 0.03679857
TschuprowT(contingency_table_AuSlIt) # 0.03679857
ContCoef(contingency_table_AuSlIt) # 0.05197071
ContCoef(contingency_table_AuSlIt, correct = TRUE) # 0.06365086
GoodmanKruskalTau(contingency_table_AuSlIt) # 0.001818842
Lambda(contingency_table_AuSlIt) # 0
UncertCoef(contingency_table_AuSlIt) # 0.003299415

df_AuGeSw <- df %>% dplyr::filter(Geography == "Austria" | Geography == "Germany" | Geography == "Switzerland")
contingency_table_AuGeSw <- xtabs(Count ~ Geography + Religion, data = df_AuGeSw) / 200
Start_time <- Sys.time()
NCor(contingency_table_AuGeSw, CIs = FALSE, Test = FALSE) # 0.04737463
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AuGeSw) # 0.008496698
TschuprowT(contingency_table_AuGeSw) # 0.008496698
ContCoef(contingency_table_AuGeSw) # 0.01201528
ContCoef(contingency_table_AuGeSw, correct = TRUE) # 0.01471565
GoodmanKruskalTau(contingency_table_AuGeSw) # 4.319832e-05
Lambda(contingency_table_AuGeSw) # 0
UncertCoef(contingency_table_AuGeSw) # 0.0001626768

df_AuGeCz <- df %>% dplyr::filter(Geography == "Austria" | Geography == "Germany" | Geography == "Czechia")
contingency_table_AuGeCz <- xtabs(Count ~ Geography + Religion, data = df_AuGeCz) / 100
Start_time <- Sys.time()
NCor(contingency_table_AuGeCz, CIs = FALSE, Test = FALSE) # 0.2546134
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AuGeCz) # 0.04795946
TschuprowT(contingency_table_AuGeCz) # 0.04795946
ContCoef(contingency_table_AuGeCz) # 0.06766946
ContCoef(contingency_table_AuGeCz, correct = TRUE) # 0.08287782
GoodmanKruskalTau(contingency_table_AuGeCz) # 0.001444883
Lambda(contingency_table_AuGeCz) # 0
UncertCoef(contingency_table_AuGeCz) # 0.009497587

df_PoGeCz <- df %>% dplyr::filter(Geography == "Poland" | Geography == "Germany" | Geography == "Czechia")
contingency_table_PoGeCz <- xtabs(Count ~ Geography + Religion, data = df_PoGeCz) / 100
Start_time <- Sys.time()
NCor(contingency_table_PoGeCz, CIs = FALSE, Test = FALSE) # 0.969063
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PoGeCz) # 0.1337699
TschuprowT(contingency_table_PoGeCz) # 0.1337699
ContCoef(contingency_table_PoGeCz) # 0.1858822
ContCoef(contingency_table_PoGeCz, correct = TRUE) # 0.2276583
GoodmanKruskalTau(contingency_table_PoGeCz) # 0.03124714
Lambda(contingency_table_PoGeCz) # 0
UncertCoef(contingency_table_PoGeCz) # 0.05066534

df_PoSlCz <- df %>% dplyr::filter(Geography == "Poland" | Geography == "Slovakia" | Geography == "Czechia")
contingency_table_PoSlCz <- xtabs(Count ~ Geography + Religion, data = df_PoSlCz) / 20
Start_time <- Sys.time()
NCor(contingency_table_PoSlCz, CIs = FALSE, Test = FALSE) # 0.5290542
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PoSlCz) # 0.02108973
TschuprowT(contingency_table_PoSlCz) # 0.02108973
ContCoef(contingency_table_PoSlCz) # 0.02981212
ContCoef(contingency_table_PoSlCz, correct = TRUE) # 0.03651224
GoodmanKruskalTau(contingency_table_PoSlCz) # 0.0003958002
Lambda(contingency_table_PoSlCz) # 7.159443e-05
UncertCoef(contingency_table_PoSlCz) # 0.00110994

df_PoSlUk <- df %>% dplyr::filter(Geography == "Poland" | Geography == "Slovakia" | Geography == "Ukraine")
contingency_table_PoSlUk <- xtabs(Count ~ Geography + Religion, data = df_PoSlUk) / 20
Start_time <- Sys.time()
NCor(contingency_table_PoSlUk, CIs = FALSE, Test = FALSE) # 0.8903236
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PoSlUk) # 0.06603959
TschuprowT(contingency_table_PoSlUk) # 0.06603959
ContCoef(contingency_table_PoSlUk) # 0.09298941
ContCoef(contingency_table_PoSlUk, correct = TRUE) # 0.1138883
GoodmanKruskalTau(contingency_table_PoSlUk) # 0.00704026
Lambda(contingency_table_PoSlUk) # 0
UncertCoef(contingency_table_PoSlUk) # 0.01120586

df_PoBeUk <- df %>% dplyr::filter(Geography == "Poland" | Geography == "Belarus" | Geography == "Ukraine")
contingency_table_PoBeUk <- xtabs(Count ~ Geography + Religion, data = df_PoBeUk) / 100
Start_time <- Sys.time()
NCor(contingency_table_PoBeUk, CIs = FALSE, Test = FALSE) # 0.8299367
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PoBeUk) # 0.06430619
TschuprowT(contingency_table_PoBeUk) # 0.06430619
ContCoef(contingency_table_PoBeUk) # 0.09056893
ContCoef(contingency_table_PoBeUk, correct = TRUE) # 0.1109238
GoodmanKruskalTau(contingency_table_PoBeUk) # 0.006260771
Lambda(contingency_table_PoBeUk) # 0
UncertCoef(contingency_table_PoBeUk) # 0.00965159

df_PoBeLi <- df %>% dplyr::filter(Geography == "Poland" | Geography == "Belarus" | Geography == "Lithuania")
contingency_table_PoBeLi <- xtabs(Count ~ Geography + Religion, data = df_PoBeLi) / 100
Start_time <- Sys.time()
NCor(contingency_table_PoBeLi, CIs = FALSE, Test = FALSE) # 0.5428084
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PoBeLi) # 0.02393402
TschuprowT(contingency_table_PoBeLi) # 0.02393402
ContCoef(contingency_table_PoBeLi) # 0.03382844
ContCoef(contingency_table_PoBeLi, correct = TRUE) # 0.04143121
GoodmanKruskalTau(contingency_table_PoBeLi) # 0.0008711315
Lambda(contingency_table_PoBeLi) # 0.0006014771
UncertCoef(contingency_table_PoBeLi) # 0.001355399

df_LaBeLi <- df %>% dplyr::filter(Geography == "Latvia" | Geography == "Belarus" | Geography == "Lithuania")
contingency_table_LaBeLi <- xtabs(Count ~ Geography + Religion, data = df_LaBeLi) / 100
Start_time <- Sys.time()
NCor(contingency_table_LaBeLi, CIs = FALSE, Test = FALSE) # 0.3331149
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaBeLi) # 0.02581464
TschuprowT(contingency_table_LaBeLi) # 0.02581464
ContCoef(contingency_table_LaBeLi) # 0.03648311
ContCoef(contingency_table_LaBeLi, correct = TRUE) # 0.04468251
GoodmanKruskalTau(contingency_table_LaBeLi) # 0.0005418758
Lambda(contingency_table_LaBeLi) # 0
UncertCoef(contingency_table_LaBeLi) # 0.001172848

df_LaBeRu <- df %>% dplyr::filter(Geography == "Latvia" | Geography == "Belarus" | Geography == "Russia")
contingency_table_LaBeRu <- xtabs(Count ~ Geography + Religion, data = df_LaBeRu) / 100
Start_time <- Sys.time()
NCor(contingency_table_LaBeRu, CIs = FALSE, Test = FALSE) # 0.9321676
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaBeRu) # 0.06111145
TschuprowT(contingency_table_LaBeRu) # 0.06111145
ContCoef(contingency_table_LaBeRu) # 0.08610367
ContCoef(contingency_table_LaBeRu, correct = TRUE) # 0.105455
GoodmanKruskalTau(contingency_table_LaBeRu) # 0.006237598
Lambda(contingency_table_LaBeRu) # 0
UncertCoef(contingency_table_LaBeRu) # 0.02055494

df_LaEsRu <- df %>% dplyr::filter(Geography == "Latvia" | Geography == "Estonia" | Geography == "Russia")
contingency_table_LaEsRu <- xtabs(Count ~ Geography + Religion, data = df_LaEsRu) / 100
Start_time <- Sys.time()
NCor(contingency_table_LaEsRu, CIs = FALSE, Test = FALSE) # 0.9226419
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaEsRu) # 0.03100333
TschuprowT(contingency_table_LaEsRu) # 0.03100333
ContCoef(contingency_table_LaEsRu) # 0.04380324
ContCoef(contingency_table_LaEsRu, correct = TRUE) # 0.0536478
GoodmanKruskalTau(contingency_table_LaEsRu) # 0.00156619
Lambda(contingency_table_LaEsRu) # 0
UncertCoef(contingency_table_LaEsRu) # 0.006948461

df_FiNoRu <- df %>% dplyr::filter(Geography == "Finland" | Geography == "Norway" | Geography == "Russia")
contingency_table_FiNoRu <- xtabs(Count ~ Geography + Religion, data = df_FiNoRu) / 100
Start_time <- Sys.time()
NCor(contingency_table_FiNoRu, CIs = FALSE, Test = FALSE) # 0.4586712
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_FiNoRu) # 0.03877543
TschuprowT(contingency_table_FiNoRu) # 0.03877543
ContCoef(contingency_table_FiNoRu) # 0.05475448
ContCoef(contingency_table_FiNoRu, correct = TRUE) # 0.06706026
GoodmanKruskalTau(contingency_table_FiNoRu) # 0.002135477
Lambda(contingency_table_FiNoRu) # 0
UncertCoef(contingency_table_FiNoRu) # 0.006131902

df_FiNoSw <- df %>% dplyr::filter(Geography == "Finland" | Geography == "Norway" | Geography == "Sweden")
contingency_table_FiNoSw <- xtabs(Count ~ Geography + Religion, data = df_FiNoSw) / 100
Start_time <- Sys.time()
NCor(contingency_table_FiNoSw, CIs = FALSE, Test = FALSE) # 0.456442
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_FiNoSw) # 0.101006
TschuprowT(contingency_table_FiNoSw) # 0.101006
ContCoef(contingency_table_FiNoSw) # 0.1414087
ContCoef(contingency_table_FiNoSw, correct = TRUE) # 0.1731895
GoodmanKruskalTau(contingency_table_FiNoSw) # 0.01107905
Lambda(contingency_table_FiNoSw) # 0
UncertCoef(contingency_table_FiNoSw) # 0.01587602

df_BeUkRu <- df %>% dplyr::filter(Geography == "Belarus" | Geography == "Ukraine" | Geography == "Russia")
contingency_table_BeUkRu <- xtabs(Count ~ Geography + Religion, data = df_BeUkRu) / 200
Start_time <- Sys.time()
NCor(contingency_table_BeUkRu, CIs = FALSE, Test = FALSE) # 0.7594435
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BeUkRu) # 0.1038069
TschuprowT(contingency_table_BeUkRu) # 0.1038069
ContCoef(contingency_table_BeUkRu) # 0.1452483
ContCoef(contingency_table_BeUkRu, correct = TRUE) # 0.1778922
GoodmanKruskalTau(contingency_table_BeUkRu) # 0.01729935
Lambda(contingency_table_BeUkRu) # 0
UncertCoef(contingency_table_BeUkRu) # 0.02998236

df_RoUkMo <- df %>% dplyr::filter(Geography == "Romania" | Geography == "Ukraine" | Geography == "Moldova")
contingency_table_RoUkMo <- xtabs(Count ~ Geography + Religion, data = df_RoUkMo) / 100
Start_time <- Sys.time()
NCor(contingency_table_RoUkMo, CIs = FALSE, Test = FALSE) # 0.6102135
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_RoUkMo) # 0.04367463
TschuprowT(contingency_table_RoUkMo) # 0.04367463
ContCoef(contingency_table_RoUkMo) # 0.06164778
ContCoef(contingency_table_RoUkMo, correct = TRUE) # 0.0755028
GoodmanKruskalTau(contingency_table_RoUkMo) # 0.003171875
Lambda(contingency_table_RoUkMo) # 0
UncertCoef(contingency_table_RoUkMo) # 0.005134611

df_RoUkHu <- df %>% dplyr::filter(Geography == "Romania" | Geography == "Ukraine" | Geography == "Hungary")
contingency_table_RoUkHu <- xtabs(Count ~ Geography + Religion, data = df_RoUkHu) / 100
Start_time <- Sys.time()
NCor(contingency_table_RoUkHu, CIs = FALSE, Test = FALSE) # 0.5800878
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_RoUkHu) # 0.05429286
TschuprowT(contingency_table_RoUkHu) # 0.05429286
ContCoef(contingency_table_RoUkHu) # 0.07655637
ContCoef(contingency_table_RoUkHu, correct = TRUE) # 0.09376202
GoodmanKruskalTau(contingency_table_RoUkHu) # 0.003313298
Lambda(contingency_table_RoUkHu) # 3.827871e-05
UncertCoef(contingency_table_RoUkHu) # 0.00599469

df_SlUkHu <- df %>% dplyr::filter(Geography == "Slovakia" | Geography == "Ukraine" | Geography == "Hungary")
contingency_table_SlUkHu <- xtabs(Count ~ Geography + Religion, data = df_SlUkHu) / 20
Start_time <- Sys.time()
NCor(contingency_table_SlUkHu, CIs = FALSE, Test = FALSE) # 0.7072875
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SlUkHu) # 0.04980211
TschuprowT(contingency_table_SlUkHu) # 0.04980211
ContCoef(contingency_table_SlUkHu) # 0.07025678
ContCoef(contingency_table_SlUkHu, correct = TRUE) # 0.08604663
GoodmanKruskalTau(contingency_table_SlUkHu) # 0.003243143
Lambda(contingency_table_SlUkHu) # 7.84222e-05
UncertCoef(contingency_table_SlUkHu) # 0.007634101

df_CzSlAu <- df %>% dplyr::filter(Geography == "Czechia" | Geography == "Slovakia" | Geography == "Austria")
contingency_table_CzSlAu <- xtabs(Count ~ Geography + Religion, data = df_CzSlAu) / 20
Start_time <- Sys.time()
NCor(contingency_table_CzSlAu, CIs = FALSE, Test = FALSE) # 0.9511036
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CzSlAu) # 0.1589938
TschuprowT(contingency_table_CzSlAu) # 0.1589938
ContCoef(contingency_table_CzSlAu) # 0.219374
ContCoef(contingency_table_CzSlAu, correct = TRUE) # 0.2686772
GoodmanKruskalTau(contingency_table_CzSlAu) # 0.02931318
Lambda(contingency_table_CzSlAu) # 0
UncertCoef(contingency_table_CzSlAu) # 0.04995671

df_HuSlAu <- df %>% dplyr::filter(Geography == "Hungary" | Geography == "Slovakia" | Geography == "Austria")
contingency_table_HuSlAu <- xtabs(Count ~ Geography + Religion, data = df_HuSlAu) / 20
Start_time <- Sys.time()
NCor(contingency_table_HuSlAu, CIs = FALSE, Test = FALSE) # 0.899647
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HuSlAu) # 0.1718938
TschuprowT(contingency_table_HuSlAu) # 0.1718938
ContCoef(contingency_table_HuSlAu) # 0.2362151
ContCoef(contingency_table_HuSlAu, correct = TRUE) # 0.2893033
GoodmanKruskalTau(contingency_table_HuSlAu) # 0.03137824
Lambda(contingency_table_HuSlAu) # 0.04948332
UncertCoef(contingency_table_HuSlAu) # 0.04996083

df_HuAuSl <- df %>% dplyr::filter(Geography == "Hungary" | Geography == "Austria" | Geography == "Slovenia")
contingency_table_HuAuSl <- xtabs(Count ~ Geography + Religion, data = df_HuAuSl) / 10
Start_time <- Sys.time()
NCor(contingency_table_HuAuSl, CIs = FALSE, Test = FALSE) # 0.8027942
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HuAuSl) # 0.1483112
TschuprowT(contingency_table_HuAuSl) # 0.1483112
ContCoef(contingency_table_HuAuSl) # 0.205277
ContCoef(contingency_table_HuAuSl, correct = TRUE) # 0.251412
GoodmanKruskalTau(contingency_table_HuAuSl) # 0.03300992
Lambda(contingency_table_HuAuSl) # 0.06361609
UncertCoef(contingency_table_HuAuSl) # 0.04427444

df_HuCrSl <- df %>% dplyr::filter(Geography == "Hungary" | Geography == "Croatia" | Geography == "Slovenia")
contingency_table_HuCrSl <- xtabs(Count ~ Geography + Religion, data = df_HuCrSl) / 10
Start_time <- Sys.time()
NCor(contingency_table_HuCrSl, CIs = FALSE, Test = FALSE) # 0.7157109
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HuCrSl) # 0.09024269
TschuprowT(contingency_table_HuCrSl) # 0.09024269
ContCoef(contingency_table_HuCrSl) # 0.1265956
ContCoef(contingency_table_HuCrSl, correct = TRUE) # 0.1550474
GoodmanKruskalTau(contingency_table_HuCrSl) # 0.008505104
Lambda(contingency_table_HuCrSl) # 0.00828176
UncertCoef(contingency_table_HuCrSl) # 0.01492176

df_BoCrSe <- df %>% dplyr::filter(Geography == "Bosnia & Herzegovina" | Geography == "Croatia" | Geography == "Serbia")
contingency_table_BoCrSe <- xtabs(Count ~ Geography + Religion, data = df_BoCrSe) / 10
Start_time <- Sys.time()
NCor(contingency_table_BoCrSe, CIs = FALSE, Test = FALSE) # 0.8432995
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BoCrSe) # 0.3523457
TschuprowT(contingency_table_BoCrSe) # 0.3523457
ContCoef(contingency_table_BoCrSe) # 0.4459902
ContCoef(contingency_table_BoCrSe, correct = TRUE) # 0.5462242
GoodmanKruskalTau(contingency_table_BoCrSe) # 0.1026227
Lambda(contingency_table_BoCrSe) # 0.1090406
UncertCoef(contingency_table_BoCrSe) # 0.1475581

df_BoMoSe <- df %>% dplyr::filter(Geography == "Bosnia & Herzegovina" | Geography == "Montenegro" | Geography == "Serbia")
contingency_table_BoMoSe <- xtabs(Count ~ Geography + Religion, data = df_BoMoSe) / 10
Start_time <- Sys.time()
NCor(contingency_table_BoMoSe, CIs = FALSE, Test = FALSE) # 0.7954003
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BoMoSe) # 0.3216566
TschuprowT(contingency_table_BoMoSe) # 0.3216566
ContCoef(contingency_table_BoMoSe) # 0.4140637
ContCoef(contingency_table_BoMoSe, correct = TRUE) # 0.5071224
GoodmanKruskalTau(contingency_table_BoMoSe) # 0.1713263
Lambda(contingency_table_BoMoSe) # 0.168928
UncertCoef(contingency_table_BoMoSe) # 0.1492885

df_AlMoKo <- df %>% dplyr::filter(Geography == "Albania" | Geography == "Montenegro" | Geography == "Kosovo")
contingency_table_AlMoKo <- xtabs(Count ~ Geography + Religion, data = df_AlMoKo) / 100
Start_time <- Sys.time()
NCor(contingency_table_AlMoKo, CIs = FALSE, Test = FALSE) # 0.8151939
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AlMoKo) # 0.3450268
TschuprowT(contingency_table_AlMoKo) # 0.3450268
ContCoef(contingency_table_AlMoKo) # 0.4385229
ContCoef(contingency_table_AlMoKo, correct = TRUE) # 0.5370787
GoodmanKruskalTau(contingency_table_AlMoKo) # 0.08315006
Lambda(contingency_table_AlMoKo) # 0.1003336
UncertCoef(contingency_table_AlMoKo) # 0.1602682

df_SeMoKo <- df %>% dplyr::filter(Geography == "Serbia" | Geography == "Montenegro" | Geography == "Kosovo")
contingency_table_SeMoKo <- xtabs(Count ~ Geography + Religion, data = df_SeMoKo) / 100
Start_time <- Sys.time()
NCor(contingency_table_SeMoKo, CIs = FALSE, Test = FALSE) # 0.946691
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SeMoKo) # 0.5477222
TschuprowT(contingency_table_SeMoKo) # 0.5477222
ContCoef(contingency_table_SeMoKo) # 0.6123722
ContCoef(contingency_table_SeMoKo, correct = TRUE) # 0.7499997
GoodmanKruskalTau(contingency_table_SeMoKo) # 0.4348757
Lambda(contingency_table_SeMoKo) # 0.5582639
UncertCoef(contingency_table_SeMoKo) # 0.4387559

df_SeNmKo <- df %>% dplyr::filter(Geography == "Serbia" | Geography == "North Macedonia" | Geography == "Kosovo")
contingency_table_SeNmKo <- xtabs(Count ~ Geography + Religion, data = df_SeNmKo) / 100
Start_time <- Sys.time()
NCor(contingency_table_SeNmKo, CIs = FALSE, Test = FALSE) # 0.9069089
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SeNmKo) # 0.4982178
TschuprowT(contingency_table_SeNmKo) # 0.4982178
ContCoef(contingency_table_SeNmKo) # 0.5759759
ContCoef(contingency_table_SeNmKo, correct = TRUE) # 0.7054235
GoodmanKruskalTau(contingency_table_SeNmKo) # 0.2651431
Lambda(contingency_table_SeNmKo) # 0.3845142
UncertCoef(contingency_table_SeNmKo) # 0.3305601

df_AlNmKo <- df %>% dplyr::filter(Geography == "Albania" | Geography == "North Macedonia" | Geography == "Kosovo")
contingency_table_AlNmKo <- xtabs(Count ~ Geography + Religion, data = df_AlNmKo) / 100
Start_time <- Sys.time()
NCor(contingency_table_AlNmKo, CIs = FALSE, Test = FALSE) # 0.736384
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AlNmKo) # 0.331021
TschuprowT(contingency_table_AlNmKo) # 0.331021
ContCoef(contingency_table_AlNmKo) # 0.4239767
ContCoef(contingency_table_AlNmKo, correct = TRUE) # 0.5192633
GoodmanKruskalTau(contingency_table_AlNmKo) # 0.09696914
Lambda(contingency_table_AlNmKo) # 0.1761898
UncertCoef(contingency_table_AlNmKo) # 0.1397822

df_AlNmGr <- df %>% dplyr::filter(Geography == "Albania" | Geography == "North Macedonia" | Geography == "Greece")
contingency_table_AlNmGr <- xtabs(Count ~ Geography + Religion, data = df_AlNmGr) / 100
Start_time <- Sys.time()
NCor(contingency_table_AlNmGr, CIs = FALSE, Test = FALSE) # 0.8577225
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AlNmGr) # 0.4111485
TschuprowT(contingency_table_AlNmGr) # 0.4111485
ContCoef(contingency_table_AlNmGr) # 0.5026569
ContCoef(contingency_table_AlNmGr, correct = TRUE) # 0.6156265
GoodmanKruskalTau(contingency_table_AlNmGr) # 0.2210288
Lambda(contingency_table_AlNmGr) # 0.2748769
UncertCoef(contingency_table_AlNmGr) # 0.2270293

df_BuNmGr <- df %>% dplyr::filter(Geography == "Bulgaria" | Geography == "North Macedonia" | Geography == "Greece")
contingency_table_BuNmGr <- xtabs(Count ~ Geography + Religion, data = df_BuNmGr) / 100
Start_time <- Sys.time()
NCor(contingency_table_BuNmGr, CIs = FALSE, Test = FALSE) # 0.5419116
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BuNmGr) # 0.1854796
TschuprowT(contingency_table_BuNmGr) # 0.1854796
ContCoef(contingency_table_BuNmGr) # 0.2537241
ContCoef(contingency_table_BuNmGr, correct = TRUE) # 0.3107473
GoodmanKruskalTau(contingency_table_BuNmGr) # 0.02518227
Lambda(contingency_table_BuNmGr) # 0.02525299
UncertCoef(contingency_table_BuNmGr) # 0.0432975

df_BuTuGr <- df %>% dplyr::filter(Geography == "Bulgaria" | Geography == "Türkiye" | Geography == "Greece")
contingency_table_BuTuGr <- xtabs(Count ~ Geography + Religion, data = df_BuTuGr) / 100
Start_time <- Sys.time()
NCor(contingency_table_BuTuGr, CIs = FALSE, Test = FALSE) # 0.9936817
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BuTuGr) # 0.6624045
TschuprowT(contingency_table_BuTuGr) # 0.6624045
ContCoef(contingency_table_BuTuGr) # 0.683662
ContCoef(contingency_table_BuTuGr, correct = TRUE) # 0.8373115
GoodmanKruskalTau(contingency_table_BuTuGr) # 0.6388369
Lambda(contingency_table_BuTuGr) # 0.7095574
UncertCoef(contingency_table_BuTuGr) # 0.7300581

df_BuNmSe <- df %>% dplyr::filter(Geography == "Bulgaria" | Geography == "North Macedonia" | Geography == "Serbia")
contingency_table_BuNmSe <- xtabs(Count ~ Geography + Religion, data = df_BuNmSe) / 100
Start_time <- Sys.time()
NCor(contingency_table_BuNmSe, CIs = FALSE, Test = FALSE) # 0.4929697
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BuNmSe) # 0.1758968
TschuprowT(contingency_table_BuNmSe) # 0.1758968
ContCoef(contingency_table_BuNmSe) # 0.241399
ContCoef(contingency_table_BuNmSe, correct = TRUE) # 0.2956522
GoodmanKruskalTau(contingency_table_BuNmSe) # 0.02053356
Lambda(contingency_table_BuNmSe) # 0.03461634
UncertCoef(contingency_table_BuNmSe) # 0.03772781

df_BuRoSe <- df %>% dplyr::filter(Geography == "Bulgaria" | Geography == "Romania" | Geography == "Serbia")
contingency_table_BuRoSe <- xtabs(Count ~ Geography + Religion, data = df_BuRoSe) / 100
Start_time <- Sys.time()
NCor(contingency_table_BuRoSe, CIs = FALSE, Test = FALSE) # 0.779378
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BuRoSe) # 0.1766898
TschuprowT(contingency_table_BuRoSe) # 0.1766898
ContCoef(contingency_table_BuRoSe) # 0.2424234
ContCoef(contingency_table_BuRoSe, correct = TRUE) # 0.2969068
GoodmanKruskalTau(contingency_table_BuRoSe) # 0.03701014
Lambda(contingency_table_BuRoSe) # 0.05271197
UncertCoef(contingency_table_BuRoSe) # 0.05554199

df_HuRoSe <- df %>% dplyr::filter(Geography == "Hungary" | Geography == "Romania" | Geography == "Serbia")
contingency_table_HuRoSe <- xtabs(Count ~ Geography + Religion, data = df_HuRoSe) / 100
Start_time <- Sys.time()
NCor(contingency_table_HuRoSe, CIs = FALSE, Test = FALSE) # 0.8346488
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HuRoSe) # 0.1517594
TschuprowT(contingency_table_HuRoSe) # 0.1517594
ContCoef(contingency_table_HuRoSe) # 0.2098418
ContCoef(contingency_table_HuRoSe, correct = TRUE) # 0.2570027
GoodmanKruskalTau(contingency_table_HuRoSe) # 0.01992498
Lambda(contingency_table_HuRoSe) # 0.02897984
UncertCoef(contingency_table_HuRoSe) # 0.03173494

df_HuCrSe <- df %>% dplyr::filter(Geography == "Hungary" | Geography == "Croatia" | Geography == "Serbia")
contingency_table_HuCrSe <- xtabs(Count ~ Geography + Religion, data = df_HuCrSe) / 10
Start_time <- Sys.time()
NCor(contingency_table_HuCrSe, CIs = FALSE, Test = FALSE) # 0.7907629
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_HuCrSe) # 0.1303253
TschuprowT(contingency_table_HuCrSe) # 0.1303253
ContCoef(contingency_table_HuCrSe) # 0.181255
ContCoef(contingency_table_HuCrSe, correct = TRUE) # 0.2219911
GoodmanKruskalTau(contingency_table_HuCrSe) # 0.02045005
Lambda(contingency_table_HuCrSe) # 0.0412478
UncertCoef(contingency_table_HuCrSe) # 0.02993183


## Africa ------------------------------------------------------------------
df_TuLiAl <- df %>% dplyr::filter(Geography == "Tunisia" | Geography == "Libya" | Geography == "Algeria")
contingency_table_TuLiAl <- xtabs(Count ~ Geography + Religion, data = df_TuLiAl) / 2
Start_time <- Sys.time()
NCor(contingency_table_TuLiAl, CIs = FALSE, Test = FALSE) # (pop size: 30,770,243)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_TuLiAl) # 0.01381256
TschuprowT(contingency_table_TuLiAl) # 0.01381256
ContCoef(contingency_table_TuLiAl) # 0.01953019
ContCoef(contingency_table_TuLiAl, correct = TRUE) # 0.02391949
GoodmanKruskalTau(contingency_table_TuLiAl) # 0.0001509629
Lambda(contingency_table_TuLiAl) # 0.000102635
UncertCoef(contingency_table_TuLiAl) # 0.0004025296

df_MoMaAl <- df %>% dplyr::filter(Geography == "Morocco" | Geography == "Mauritania" | Geography == "Algeria")
contingency_table_MoMaAl <- xtabs(Count ~ Geography + Religion, data = df_MoMaAl) / 4
Start_time <- Sys.time()
NCor(contingency_table_MoMaAl, CIs = FALSE, Test = FALSE) # 0.5021951 (pop size: 21,105,529: 7.902396 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_MoMaAl) # 0.01712339
TschuprowT(contingency_table_MoMaAl) # 0.01712339
ContCoef(contingency_table_MoMaAl) # 0.02420903
ContCoef(contingency_table_MoMaAl, correct = TRUE) # 0.02964989
GoodmanKruskalTau(contingency_table_MoMaAl) # 0.0005015672
Lambda(contingency_table_MoMaAl) # 5.367628e-05
UncertCoef(contingency_table_MoMaAl) # 0.0007365928

df_MaMaAl <- df %>% dplyr::filter(Geography == "Mali" | Geography == "Mauritania" | Geography == "Algeria")
contingency_table_MaMaAl <- xtabs(Count ~ Geography + Religion, data = df_MaMaAl) / 4
Start_time <- Sys.time()
NCor(contingency_table_MaMaAl, CIs = FALSE, Test = FALSE) # 0.7624348 (pop size: 16,613,139: 5.441876 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_MaMaAl) # 0.07274984
TschuprowT(contingency_table_MaMaAl) # 0.07274984
ContCoef(contingency_table_MaMaAl) # 0.1023436
ContCoef(contingency_table_MaMaAl, correct = TRUE) # 0.1253448
GoodmanKruskalTau(contingency_table_MaMaAl) # 0.007921081
Lambda(contingency_table_MaMaAl) # 0.01416976
UncertCoef(contingency_table_MaMaAl) # 0.01063962

df_MaNiAl <- df %>% dplyr::filter(Geography == "Mali" | Geography == "Niger" | Geography == "Algeria")
contingency_table_MaNiAl <- xtabs(Count ~ Geography + Religion, data = df_MaNiAl) / 8
Start_time <- Sys.time()
NCor(contingency_table_MaNiAl, CIs = FALSE, Test = FALSE) # 0.6887366 (newest code: 1.64452 days with pop size 10,660,657)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_MaNiAl) # 0.07302156
TschuprowT(contingency_table_MaNiAl) # 0.07302156
ContCoef(contingency_table_MaNiAl) # 0.1027218
ContCoef(contingency_table_MaNiAl, correct = TRUE) # 0.125808
GoodmanKruskalTau(contingency_table_MaNiAl) # 0.004579089
Lambda(contingency_table_MaNiAl) # 0.007969889
UncertCoef(contingency_table_MaNiAl) # 0.007778945

df_LiNiAl <- df %>% dplyr::filter(Geography == "Libya" | Geography == "Niger" | Geography == "Algeria")
contingency_table_LiNiAl <- xtabs(Count ~ Geography + Religion, data = df_LiNiAl) / 2
Start_time <- Sys.time()
NCor(contingency_table_LiNiAl, CIs = FALSE, Test = FALSE) # (pop size: 36,359,493)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LiNiAl) # 0.009375468
TschuprowT(contingency_table_LiNiAl) # 0.009375468
ContCoef(contingency_table_LiNiAl) # 0.01325775
ContCoef(contingency_table_LiNiAl, correct = TRUE) # 0.01623736
GoodmanKruskalTau(contingency_table_LiNiAl) # 4.048537e-05
Lambda(contingency_table_LiNiAl) # 2.453832e-06
UncertCoef(contingency_table_LiNiAl) # 0.0001601229

df_LiNiCh <- df %>% dplyr::filter(Geography == "Libya" | Geography == "Niger" | Geography == "Chad")
contingency_table_LiNiCh <- xtabs(Count ~ Geography + Religion, data = df_LiNiCh) / 10
Start_time <- Sys.time()
NCor(contingency_table_LiNiCh, CIs = FALSE, Test = FALSE) # 0.9822642
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LiNiCh) # 0.3528634
TschuprowT(contingency_table_LiNiCh) # 0.3528634
ContCoef(contingency_table_LiNiCh) # 0.4465149
ContCoef(contingency_table_LiNiCh, correct = TRUE) # 0.5468669
GoodmanKruskalTau(contingency_table_LiNiCh) # 0.1543103
Lambda(contingency_table_LiNiCh) # 0.1902532
UncertCoef(contingency_table_LiNiCh) # 0.194675

df_LiEgSu <- df %>% dplyr::filter(Geography == "Libya" | Geography == "Egypt" | Geography == "Sudan")
contingency_table_LiEgSu <- xtabs(Count ~ Geography + Religion, data = df_LiEgSu) / 2
Start_time <- Sys.time()
NCor(contingency_table_LiEgSu, CIs = FALSE, Test = FALSE) # (pop size: 77,968,949)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LiEgSu) # 0.06562038
TschuprowT(contingency_table_LiEgSu) # 0.06562038
ContCoef(contingency_table_LiEgSu) # 0.09240419
ContCoef(contingency_table_LiEgSu, correct = TRUE) # 0.1131716
GoodmanKruskalTau(contingency_table_LiEgSu) # 0.005847071
Lambda(contingency_table_LiEgSu) # 3.2735e-07
UncertCoef(contingency_table_LiEgSu) # 0.01072378

df_LiChSu <- df %>% dplyr::filter(Geography == "Libya" | Geography == "Chad" | Geography == "Sudan")
contingency_table_LiChSu <- xtabs(Count ~ Geography + Religion, data = df_LiChSu) / 2
Start_time <- Sys.time()
NCor(contingency_table_LiChSu, CIs = FALSE, Test = FALSE) # (pop size: 32,278,394)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LiChSu) # 0.2887045
TschuprowT(contingency_table_LiChSu) # 0.2887045
ContCoef(contingency_table_LiChSu) # 0.3779975
ContCoef(contingency_table_LiChSu, correct = TRUE) # 0.4629505
GoodmanKruskalTau(contingency_table_LiChSu) # 0.100617
Lambda(contingency_table_LiChSu) # 0.1109512
UncertCoef(contingency_table_LiChSu) # 0.1195354

df_CiGhBf <- df %>% dplyr::filter(Geography == "Côte d'Ivoire" | Geography == "Ghana" | Geography == "Burkina Faso")
contingency_table_CiGhBf <- xtabs(Count ~ Geography + Religion, data = df_CiGhBf) / 10
Start_time <- Sys.time()
NCor(contingency_table_CiGhBf, CIs = FALSE, Test = FALSE) # 0.6296445
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CiGhBf) # 0.2933233
TschuprowT(contingency_table_CiGhBf) # 0.2933233
ContCoef(contingency_table_CiGhBf) # 0.3831629
ContCoef(contingency_table_CiGhBf, correct = TRUE) # 0.4692769
GoodmanKruskalTau(contingency_table_CiGhBf) # 0.09196702
Lambda(contingency_table_CiGhBf) # 0.2041356
UncertCoef(contingency_table_CiGhBf) # 0.1019382

df_ToGhBf <- df %>% dplyr::filter(Geography == "Togo" | Geography == "Ghana" | Geography == "Burkina Faso")
contingency_table_ToGhBf <- xtabs(Count ~ Geography + Religion, data = df_ToGhBf) / 10
Start_time <- Sys.time()
NCor(contingency_table_ToGhBf, CIs = FALSE, Test = FALSE) # 0.6997341
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ToGhBf) # 0.3377083
TschuprowT(contingency_table_ToGhBf) # 0.3377083
ContCoef(contingency_table_ToGhBf) # 0.4309639
ContCoef(contingency_table_ToGhBf, correct = TRUE) # 0.5278209
GoodmanKruskalTau(contingency_table_ToGhBf) # 0.1569479
Lambda(contingency_table_ToGhBf) # 0.3091033
UncertCoef(contingency_table_ToGhBf) # 0.1436132

df_NiNiBe <- df %>% dplyr::filter(Geography == "Niger" | Geography == "Nigeria" | Geography == "Benin")
contingency_table_NiNiBe <- xtabs(Count ~ Geography + Religion, data = df_NiNiBe) / 200
Start_time <- Sys.time()
NCor(contingency_table_NiNiBe, CIs = FALSE, Test = FALSE) # 0.7783009
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_NiNiBe) # 0.2203855
TschuprowT(contingency_table_NiNiBe) # 0.2203855
ContCoef(contingency_table_NiNiBe) # 0.2975549
ContCoef(contingency_table_NiNiBe, correct = TRUE) # 0.3644288
GoodmanKruskalTau(contingency_table_NiNiBe) # 0.06162344
Lambda(contingency_table_NiNiBe) # 0.02181819
UncertCoef(contingency_table_NiNiBe) # 0.1123899

df_NiNiCh <- df %>% dplyr::filter(Geography == "Niger" | Geography == "Nigeria" | Geography == "Chad")
contingency_table_NiNiCh <- xtabs(Count ~ Geography + Religion, data = df_NiNiCh) / 200
Start_time <- Sys.time()
NCor(contingency_table_NiNiCh, CIs = FALSE, Test = FALSE) # 0.7361505
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_NiNiCh) # 0.217089
TschuprowT(contingency_table_NiNiCh) # 0.217089
ContCoef(contingency_table_NiNiCh) # 0.2934902
ContCoef(contingency_table_NiNiCh, correct = TRUE) # 0.3594506
GoodmanKruskalTau(contingency_table_NiNiCh) # 0.06397265
Lambda(contingency_table_NiNiCh) # 0.007617385
UncertCoef(contingency_table_NiNiCh) # 0.1035594

df_CaNiCh <- df %>% dplyr::filter(Geography == "Cameroon" | Geography == "Nigeria" | Geography == "Chad")
contingency_table_CaNiCh <- xtabs(Count ~ Geography + Religion, data = df_CaNiCh)
Start_time <- Sys.time()
NCor(contingency_table_CaNiCh, CIs = FALSE, Test = FALSE) # (pop size: 227,861,269)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CaNiCh) # 0.1112698
TschuprowT(contingency_table_CaNiCh) # 0.1112698
ContCoef(contingency_table_CaNiCh) # 0.1554465
ContCoef(contingency_table_CaNiCh, correct = TRUE) # 0.1903823
GoodmanKruskalTau(contingency_table_CaNiCh) # 0.008360761
Lambda(contingency_table_CaNiCh) # 0.03223519
UncertCoef(contingency_table_CaNiCh) # 0.02065045

df_CaCaCh <- df %>% dplyr::filter(Geography == "Cameroon" | Geography == "Central African Republic" | Geography == "Chad")
contingency_table_CaCaCh <- xtabs(Count ~ Geography + Religion, data = df_CaCaCh)
Start_time <- Sys.time()
NCor(contingency_table_CaCaCh, CIs = FALSE, Test = FALSE) # (pop size: 40,760,069)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CaCaCh) # 0.2857718
TschuprowT(contingency_table_CaCaCh) # 0.2857718
ContCoef(contingency_table_CaCaCh) # 0.3746991
ContCoef(contingency_table_CaCaCh, correct = TRUE) # 0.4589108
GoodmanKruskalTau(contingency_table_CaCaCh) # 0.1002777
Lambda(contingency_table_CaCaCh) # 0.2481111
UncertCoef(contingency_table_CaCaCh) # 0.1021044

df_SuCaCh <- df %>% dplyr::filter(Geography == "Sudan" | Geography == "Central African Republic" | Geography == "Chad")
contingency_table_SuCaCh <- xtabs(Count ~ Geography + Religion, data = df_SuCaCh) / 2
Start_time <- Sys.time()
NCor(contingency_table_SuCaCh, CIs = FALSE, Test = FALSE) # (pop size: 31,292,529)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SuCaCh) # 0.4178357
TschuprowT(contingency_table_SuCaCh) # 0.4178357
ContCoef(contingency_table_SuCaCh) # 0.5087292
ContCoef(contingency_table_SuCaCh, correct = TRUE) # 0.6230635
GoodmanKruskalTau(contingency_table_SuCaCh) # 0.1758508
Lambda(contingency_table_SuCaCh) # 0.2064344
UncertCoef(contingency_table_SuCaCh) # 0.23759

df_SuCaSs <- df %>% dplyr::filter(Geography == "Sudan" | Geography == "Central African Republic" | Geography == "South Sudan")
contingency_table_SuCaSs <- xtabs(Count ~ Geography + Religion, data = df_SuCaSs) / 2
Start_time <- Sys.time()
NCor(contingency_table_SuCaSs, CIs = FALSE, Test = FALSE) # (pop size: 27,224,029)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SuCaSs) # 0.5794066
TschuprowT(contingency_table_SuCaSs) # 0.5794066
ContCoef(contingency_table_SuCaSs) # 0.6338042
ContCoef(contingency_table_SuCaSs, correct = TRUE) # 0.7762484
GoodmanKruskalTau(contingency_table_SuCaSs) # 0.4818024
Lambda(contingency_table_SuCaSs) # 0.5538243
UncertCoef(contingency_table_SuCaSs) # 0.513977

df_SsCaCo <- df %>% dplyr::filter(Geography == "South Sudan" | Geography == "Central African Republic" | Geography == "Congo DR")
contingency_table_SsCaCo <- xtabs(Count ~ Geography + Religion, data = df_SsCaCo) / 20
Start_time <- Sys.time()
NCor(contingency_table_SsCaCo, CIs = FALSE, Test = FALSE) # 0.7701443
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SsCaCo) # 0.152353
TschuprowT(contingency_table_SsCaCo) # 0.152353
ContCoef(contingency_table_SsCaCo) # 0.2106262
ContCoef(contingency_table_SsCaCo, correct = TRUE) # 0.2579634
GoodmanKruskalTau(contingency_table_SsCaCo) # 0.03071673
Lambda(contingency_table_SsCaCo) # 0
UncertCoef(contingency_table_SsCaCo) # 0.04890405

df_SsUgCo <- df %>% dplyr::filter(Geography == "South Sudan" | Geography == "Uganda" | Geography == "Congo DR")
contingency_table_SsUgCo <- xtabs(Count ~ Geography + Religion, data = df_SsUgCo) / 20
Start_time <- Sys.time()
NCor(contingency_table_SsUgCo, CIs = FALSE, Test = FALSE) # 0.7483773
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SsUgCo) # 0.1656478
TschuprowT(contingency_table_SsUgCo) # 0.1656478
ContCoef(contingency_table_SsUgCo) # 0.2280864
ContCoef(contingency_table_SsUgCo, correct = TRUE) # 0.2793477
GoodmanKruskalTau(contingency_table_SsUgCo) # 0.04668628
Lambda(contingency_table_SsUgCo) # 0.07315183
UncertCoef(contingency_table_SsUgCo) # 0.05233173

df_SsUgKe <- df %>% dplyr::filter(Geography == "South Sudan" | Geography == "Uganda" | Geography == "Kenya")
contingency_table_SsUgKe <- xtabs(Count ~ Geography + Religion, data = df_SsUgKe) / 10
Start_time <- Sys.time()
NCor(contingency_table_SsUgKe, CIs = FALSE, Test = FALSE) # 0.09357737
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SsUgKe) # 0.02387771
TschuprowT(contingency_table_SsUgKe) # 0.02387771
ContCoef(contingency_table_SsUgKe) # 0.03374895
ContCoef(contingency_table_SsUgKe, correct = TRUE) # 0.04133385
GoodmanKruskalTau(contingency_table_SsUgKe) # 0.0006104434
Lambda(contingency_table_SsUgKe) # 0.004807723
UncertCoef(contingency_table_SsUgKe) # 0.0009168187

df_SsEtKe <- df %>% dplyr::filter(Geography == "South Sudan" | Geography == "Ethiopia" | Geography == "Kenya")
contingency_table_SsEtKe <- xtabs(Count ~ Geography + Religion, data = df_SsEtKe) / 10
Start_time <- Sys.time()
NCor(contingency_table_SsEtKe, CIs = FALSE, Test = FALSE) # 0.634154 (pop size: 16,313,989: 5.570125 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SsEtKe) # 0.189865
TschuprowT(contingency_table_SsEtKe) # 0.189865
ContCoef(contingency_table_SsEtKe) # 0.259324
ContCoef(contingency_table_SsEtKe, correct = TRUE) # 0.3176057
GoodmanKruskalTau(contingency_table_SsEtKe) # 0.05977555
Lambda(contingency_table_SsEtKe) # 0
UncertCoef(contingency_table_SsEtKe) # 0.05930076

df_SsEtSu <- df %>% dplyr::filter(Geography == "South Sudan" | Geography == "Ethiopia" | Geography == "Sudan")
contingency_table_SsEtSu <- xtabs(Count ~ Geography + Religion, data = df_SsEtSu) / 2
Start_time <- Sys.time()
NCor(contingency_table_SsEtSu, CIs = FALSE, Test = FALSE) # (pop size: 79,662,329)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SsEtSu) # 0.3853567
TschuprowT(contingency_table_SsEtSu) # 0.3853567
ContCoef(contingency_table_SsEtSu) # 0.4785287
ContCoef(contingency_table_SsEtSu, correct = TRUE) # 0.5860756
GoodmanKruskalTau(contingency_table_SsEtSu) # 0.2136019
Lambda(contingency_table_SsEtSu) # 0.2750004
UncertCoef(contingency_table_SsEtSu) # 0.2440457

df_SuEtEr <- df %>% dplyr::filter(Geography == "Sudan" | Geography == "Ethiopia" | Geography == "Eritrea")
contingency_table_SuEtEr <- xtabs(Count ~ Geography + Religion, data = df_SuEtEr) / 2
Start_time <- Sys.time()
NCor(contingency_table_SuEtEr, CIs = FALSE, Test = FALSE) # (pop size: 77,871,329)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SuEtEr) # 0.3684267
TschuprowT(contingency_table_SuEtEr) # 0.3684267
ContCoef(contingency_table_SuEtEr) # 0.4620744
ContCoef(contingency_table_SuEtEr, correct = TRUE) # 0.5659233
GoodmanKruskalTau(contingency_table_SuEtEr) # 0.2497534
Lambda(contingency_table_SuEtEr) # 0.2462317
UncertCoef(contingency_table_SuEtEr) # 0.2335325

df_DsEtEr <- df %>% dplyr::filter(Geography == "Djibouti" | Geography == "Ethiopia" | Geography == "Eritrea")
contingency_table_DsEtEr <- xtabs(Count ~ Geography + Religion, data = df_DsEtEr) / 200
Start_time <- Sys.time()
NCor(contingency_table_DsEtEr, CIs = FALSE, Test = FALSE) # 0.5291694
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_DsEtEr) # 0.09775539
TschuprowT(contingency_table_DsEtEr) # 0.09775539
ContCoef(contingency_table_DsEtEr) # 0.1369445
ContCoef(contingency_table_DsEtEr, correct = TRUE) # 0.1677221
GoodmanKruskalTau(contingency_table_DsEtEr) # 0.009813188
Lambda(contingency_table_DsEtEr) # 0.02902118
UncertCoef(contingency_table_DsEtEr) # 0.02472354

df_DsEtSo <- df %>% dplyr::filter(Geography == "Djibouti" | Geography == "Ethiopia" | Geography == "Somalia")
contingency_table_DsEtSo <- xtabs(Count ~ Geography + Religion, data = df_DsEtSo) / 100
Start_time <- Sys.time()
NCor(contingency_table_DsEtSo, CIs = FALSE, Test = FALSE) # 0.9989128
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_DsEtSo) # 0.3093684
TschuprowT(contingency_table_DsEtSo) # 0.3093684
ContCoef(contingency_table_DsEtSo) # 0.4008289
ContCoef(contingency_table_DsEtSo, correct = TRUE) # 0.4909131
GoodmanKruskalTau(contingency_table_DsEtSo) # 0.1789533
Lambda(contingency_table_DsEtSo) # 0.2325649
UncertCoef(contingency_table_DsEtSo) # 0.2160404

df_KeEtSo <- df %>% dplyr::filter(Geography == "Kenya" | Geography == "Ethiopia" | Geography == "Somalia")
contingency_table_KeEtSo <- xtabs(Count ~ Geography + Religion, data = df_KeEtSo) / 10
Start_time <- Sys.time()
NCor(contingency_table_KeEtSo, CIs = FALSE, Test = FALSE) # 0.8105356 (pop. size: 17,259,119: 5.615437 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KeEtSo) # 0.3475243
TschuprowT(contingency_table_KeEtSo) # 0.3475243
ContCoef(contingency_table_KeEtSo) # 0.4410813
ContCoef(contingency_table_KeEtSo, correct = TRUE) # 0.5402121
GoodmanKruskalTau(contingency_table_KeEtSo) # 0.06911221
Lambda(contingency_table_KeEtSo) # 0.1318504
UncertCoef(contingency_table_KeEtSo) # 0.1845038

df_EgCaGa <- df %>% dplyr::filter(Geography == "Equatorial Guinea" | Geography == "Cameroon" | Geography == "Gabon")
contingency_table_EgCaGa <- xtabs(Count ~ Geography + Religion, data = df_EgCaGa)
Start_time <- Sys.time()
NCor(contingency_table_EgCaGa, CIs = FALSE, Test = FALSE) # 0.6128265 (pop. size: 24,551,669: 17.2035 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_EgCaGa) # 0.1134877
TschuprowT(contingency_table_EgCaGa) # 0.1134877
ContCoef(contingency_table_EgCaGa) # 0.1584679
ContCoef(contingency_table_EgCaGa, correct = TRUE) # 0.1940827
GoodmanKruskalTau(contingency_table_EgCaGa) # 0.01797963
Lambda(contingency_table_EgCaGa) # 0
UncertCoef(contingency_table_EgCaGa) # 0.02996327

df_CoCaGa <- df %>% dplyr::filter(Geography == "Congo" | Geography == "Cameroon" | Geography == "Gabon")
contingency_table_CoCaGa <- xtabs(Count ~ Geography + Religion, data = df_CoCaGa)
Start_time <- Sys.time()
NCor(contingency_table_CoCaGa, CIs = FALSE, Test = FALSE) # (pop. size: 28,029,669)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoCaGa) # 0.1827813
TschuprowT(contingency_table_CoCaGa) # 0.1827813
ContCoef(contingency_table_CoCaGa) # 0.2502658
ContCoef(contingency_table_CoCaGa, correct = TRUE) # 0.3065117
GoodmanKruskalTau(contingency_table_CoCaGa) # 0.05035028
Lambda(contingency_table_CoCaGa) # 0
UncertCoef(contingency_table_CoCaGa) # 0.07811985

df_CoCaCa <- df %>% dplyr::filter(Geography == "Congo" | Geography == "Cameroon" | Geography == "Central African Republic")
contingency_table_CoCaCa <- xtabs(Count ~ Geography + Religion, data = df_CoCaCa)
Start_time <- Sys.time()
NCor(contingency_table_CoCaCa, CIs = FALSE, Test = FALSE) # (pop. size: 30,506,669)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoCaCa) # 0.1724263
TschuprowT(contingency_table_CoCaCa) # 0.1724263
ContCoef(contingency_table_CoCaCa) # 0.2369059
ContCoef(contingency_table_CoCaCa, correct = TRUE) # 0.2901493
GoodmanKruskalTau(contingency_table_CoCaCa) # 0.03654624
Lambda(contingency_table_CoCaCa) # 0
UncertCoef(contingency_table_CoCaCa) # 0.06363469

df_CoCoCa <- df %>% dplyr::filter(Geography == "Congo" | Geography == "Congo DR" | Geography == "Central African Republic")
contingency_table_CoCoCa <- xtabs(Count ~ Geography + Religion, data = df_CoCoCa) / 20
Start_time <- Sys.time()
NCor(contingency_table_CoCoCa, CIs = FALSE, Test = FALSE) # 0.8171115
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoCoCa) # 0.1492207
TschuprowT(contingency_table_CoCoCa) # 0.1492207
ContCoef(contingency_table_CoCoCa) # 0.2064823
ContCoef(contingency_table_CoCoCa, correct = TRUE) # 0.2528881
GoodmanKruskalTau(contingency_table_CoCoCa) # 0.01960405
Lambda(contingency_table_CoCoCa) # 0
UncertCoef(contingency_table_CoCoCa) # 0.04236861

df_CoUgRu <- df %>% dplyr::filter(Geography == "Congo DR" | Geography == "Uganda" | Geography == "Rwanda")
contingency_table_CoUgRu <- xtabs(Count ~ Geography + Religion, data = df_CoUgRu) / 20
Start_time <- Sys.time()
NCor(contingency_table_CoUgRu, CIs = FALSE, Test = FALSE) # 0.7425152
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoUgRu) # 0.1633864
TschuprowT(contingency_table_CoUgRu) # 0.1633864
ContCoef(contingency_table_CoUgRu) # 0.2251315
ContCoef(contingency_table_CoUgRu, correct = TRUE) # 0.2757286
GoodmanKruskalTau(contingency_table_CoUgRu) # 0.04055167
Lambda(contingency_table_CoUgRu) # 0.06668814
UncertCoef(contingency_table_CoUgRu) # 0.04602758

df_CoBuTa <- df %>% dplyr::filter(Geography == "Congo DR" | Geography == "Burundi" | Geography == "Tanzania")
contingency_table_CoBuTa <- xtabs(Count ~ Geography + Religion, data = df_CoBuTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_CoBuTa, CIs = FALSE, Test = FALSE) # 0.9332578 (pop size: 15,526,673: 4.290499 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoBuTa) # 0.3475559
TschuprowT(contingency_table_CoBuTa) # 0.3475559
ContCoef(contingency_table_CoBuTa) # 0.4411137
ContCoef(contingency_table_CoBuTa, correct = TRUE) # 0.5402518
GoodmanKruskalTau(contingency_table_CoBuTa) # 0.1829812
Lambda(contingency_table_CoBuTa) # 0.2156904
UncertCoef(contingency_table_CoBuTa) # 0.1913067

df_CoBuRw <- df %>% dplyr::filter(Geography == "Congo DR" | Geography == "Burundi" | Geography == "Rwanda")
contingency_table_CoBuRw <- xtabs(Count ~ Geography + Religion, data = df_CoBuRw) / 20
Start_time <- Sys.time()
NCor(contingency_table_CoBuRw, CIs = FALSE, Test = FALSE) # 0.4166838
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CoBuRw) # 0.05468285
TschuprowT(contingency_table_CoBuRw) # 0.05468285
ContCoef(contingency_table_CoBuRw) # 0.07710302
ContCoef(contingency_table_CoBuRw, correct = TRUE) # 0.09443153
GoodmanKruskalTau(contingency_table_CoBuRw) # 0.00341175
Lambda(contingency_table_CoBuRw) # 0
UncertCoef(contingency_table_CoBuRw) # 0.005960545

df_RuBuTa <- df %>% dplyr::filter(Geography == "Rwanda" | Geography == "Burundi" | Geography == "Tanzania")
contingency_table_RuBuTa <- xtabs(Count ~ Geography + Religion, data = df_RuBuTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_RuBuTa, CIs = FALSE, Test = FALSE) # 0.8718512
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_RuBuTa) # 0.2499502
TschuprowT(contingency_table_RuBuTa) # 0.2499502
ContCoef(contingency_table_RuBuTa) # 0.3332744
ContCoef(contingency_table_RuBuTa, correct = TRUE) # 0.4081761
GoodmanKruskalTau(contingency_table_RuBuTa) # 0.08404296
Lambda(contingency_table_RuBuTa) # 0
UncertCoef(contingency_table_RuBuTa) # 0.111925

df_RuUgTa <- df %>% dplyr::filter(Geography == "Rwanda" | Geography == "Uganda" | Geography == "Tanzania")
contingency_table_RuUgTa <- xtabs(Count ~ Geography + Religion, data = df_RuUgTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_RuUgTa, CIs = FALSE, Test = FALSE) # 0.6412556
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_RuUgTa) # 0.2209919
TschuprowT(contingency_table_RuUgTa) # 0.2209919
ContCoef(contingency_table_RuUgTa) # 0.2983009
ContCoef(contingency_table_RuUgTa, correct = TRUE) # 0.3653425
GoodmanKruskalTau(contingency_table_RuUgTa) # 0.06212857
Lambda(contingency_table_RuUgTa) # 0.04205784
UncertCoef(contingency_table_RuUgTa) # 0.06921891

df_KeUgTa <- df %>% dplyr::filter(Geography == "Kenya" | Geography == "Uganda" | Geography == "Tanzania")
contingency_table_KeUgTa <- xtabs(Count ~ Geography + Religion, data = df_KeUgTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_KeUgTa, CIs = FALSE, Test = FALSE) # 0.527972 (pop. size: 14,325,660: 2.629821 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KeUgTa) # 0.2112253
TschuprowT(contingency_table_KeUgTa) # 0.2112253
ContCoef(contingency_table_KeUgTa) # 0.2862205
ContCoef(contingency_table_KeUgTa, correct = TRUE) # 0.3505471
GoodmanKruskalTau(contingency_table_KeUgTa) # 0.04749935
Lambda(contingency_table_KeUgTa) # 0.06274672
UncertCoef(contingency_table_KeUgTa) # 0.05391693

df_AnCoZa <- df %>% dplyr::filter(Geography == "Angola" | Geography == "Congo DR" | Geography == "Zambia")
contingency_table_AnCoZa <- xtabs(Count ~ Geography + Religion, data = df_AnCoZa)
Start_time <- Sys.time()
NCor(contingency_table_AnCoZa, CIs = FALSE, Test = FALSE) # (pop. size: 137,037,473)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AnCoZa) # 0.01098114
TschuprowT(contingency_table_AnCoZa) # 0.01098114
ContCoef(contingency_table_AnCoZa) # 0.01552781
ContCoef(contingency_table_AnCoZa, correct = TRUE) # 0.0190176
GoodmanKruskalTau(contingency_table_AnCoZa) # 0.00015745
Lambda(contingency_table_AnCoZa) # 0
UncertCoef(contingency_table_AnCoZa) # 0.0002662359

df_TaCoZa <- df %>% dplyr::filter(Geography == "Tanzania" | Geography == "Congo DR" | Geography == "Zambia")
contingency_table_TaCoZa <- xtabs(Count ~ Geography + Religion, data = df_TaCoZa)
Start_time <- Sys.time()
NCor(contingency_table_TaCoZa, CIs = FALSE, Test = FALSE) # (pop. size: 160,108,783)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_TaCoZa) # 0.3514438
TschuprowT(contingency_table_TaCoZa) # 0.3514438
ContCoef(contingency_table_TaCoZa) # 0.445075
ContCoef(contingency_table_TaCoZa, correct = TRUE) # 0.5451033
GoodmanKruskalTau(contingency_table_TaCoZa) # 0.1698684
Lambda(contingency_table_TaCoZa) # 0.204556
UncertCoef(contingency_table_TaCoZa) # 0.1880235

df_TaMoMa <- df %>% dplyr::filter(Geography == "Tanzania" | Geography == "Mozambique" | Geography == "Malawi")
contingency_table_TaMoMa <- xtabs(Count ~ Geography + Religion, data = df_TaMoMa) / 10
Start_time <- Sys.time()
NCor(contingency_table_TaMoMa, CIs = FALSE, Test = FALSE) # 0.3961997
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_TaMoMa) # 0.1412728
TschuprowT(contingency_table_TaMoMa) # 0.1412728
ContCoef(contingency_table_TaMoMa) # 0.1959181
ContCoef(contingency_table_TaMoMa, correct = TRUE) # 0.2399496
GoodmanKruskalTau(contingency_table_TaMoMa) # 0.02350116
Lambda(contingency_table_TaMoMa) # 2.883491e-07
UncertCoef(contingency_table_TaMoMa) # 0.02658013

df_TaZaMa <- df %>% dplyr::filter(Geography == "Tanzania" | Geography == "Zambia" | Geography == "Malawi")
contingency_table_TaZaMa <- xtabs(Count ~ Geography + Religion, data = df_TaZaMa)
Start_time <- Sys.time()
NCor(contingency_table_TaZaMa, CIs = FALSE, Test = FALSE) # (pop. size: 88,565,693)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_TaZaMa) # 0.2392399
TschuprowT(contingency_table_TaZaMa) # 0.2392399
ContCoef(contingency_table_TaZaMa) # 0.3204897
ContCoef(contingency_table_TaZaMa, correct = TRUE) # 0.3925182
GoodmanKruskalTau(contingency_table_TaZaMa) # 0.07036364
Lambda(contingency_table_TaZaMa) # 3.463979e-07
UncertCoef(contingency_table_TaZaMa) # 0.09730463

df_ZaMoMa <- df %>% dplyr::filter(Geography == "Zambia" | Geography == "Mozambique" | Geography == "Malawi")
contingency_table_ZaMoMa <- xtabs(Count ~ Geography + Religion, data = df_ZaMoMa)
Start_time <- Sys.time()
NCor(contingency_table_ZaMoMa, CIs = FALSE, Test = FALSE) # (pop. size: 57,451,583)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ZaMoMa) # 0.1868204
TschuprowT(contingency_table_ZaMoMa) # 0.1868204
ContCoef(contingency_table_ZaMoMa) # 0.255439
ContCoef(contingency_table_ZaMoMa, correct = TRUE) # 0.3128477
GoodmanKruskalTau(contingency_table_ZaMoMa) # 0.03505031
Lambda(contingency_table_ZaMoMa) # 3.026848e-06
UncertCoef(contingency_table_ZaMoMa) # 0.06064928

df_ZaMoZi <- df %>% dplyr::filter(Geography == "Zambia" | Geography == "Mozambique" | Geography == "Zimbabwe")
contingency_table_ZaMoZi <- xtabs(Count ~ Geography + Religion, data = df_ZaMoZi)
Start_time <- Sys.time()
NCor(contingency_table_ZaMoZi, CIs = FALSE, Test = FALSE) # (pop. size: 52,131,673)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ZaMoZi) # 0.2561863
TschuprowT(contingency_table_ZaMoZi) # 0.2561863
ContCoef(contingency_table_ZaMoZi) # 0.3406349
ContCoef(contingency_table_ZaMoZi, correct = TRUE) # 0.4171909
GoodmanKruskalTau(contingency_table_ZaMoZi) # 0.07521355
Lambda(contingency_table_ZaMoZi) # 6.28174e-06
UncertCoef(contingency_table_ZaMoZi) # 0.1054247

df_ZaAnNa <- df %>% dplyr::filter(Geography == "Zambia" | Geography == "Angola" | Geography == "Namibia")
contingency_table_ZaAnNa <- xtabs(Count ~ Geography + Religion, data = df_ZaAnNa)
Start_time <- Sys.time()
NCor(contingency_table_ZaAnNa, CIs = FALSE, Test = FALSE) # (pop. size: 49,677,753)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ZaAnNa) # 0.01194562
TschuprowT(contingency_table_ZaAnNa) # 0.01194562
ContCoef(contingency_table_ZaAnNa) # 0.01689124
ContCoef(contingency_table_ZaAnNa, correct = TRUE) # 0.02068746
GoodmanKruskalTau(contingency_table_ZaAnNa) # 6.547921e-05
Lambda(contingency_table_ZaAnNa) # 5.172635e-06
UncertCoef(contingency_table_ZaAnNa) # 0.0004000875

df_ZaNaBoZi <- df %>% dplyr::filter(Geography == "Zambia" | Geography == "Namibia" | Geography == "Botswana" | Geography == "Zimbabwe")
contingency_table_ZaNaBoZi <- xtabs(Count ~ Geography + Religion, data = df_ZaNaBoZi)
Start_time <- Sys.time()
NCor(contingency_table_ZaNaBoZi, CIs = FALSE, Test = FALSE) # (pop. size: 33,339,633)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ZaNaBoZi) # 0.01764561
TschuprowT(contingency_table_ZaNaBoZi) # 0.01594461
ContCoef(contingency_table_ZaNaBoZi) # 0.02494689
ContCoef(contingency_table_ZaNaBoZi, correct = TRUE) # 0.03055358
GoodmanKruskalTau(contingency_table_ZaNaBoZi) # 0.0001684446
Lambda(contingency_table_ZaNaBoZi) # 2.374671e-05
UncertCoef(contingency_table_ZaNaBoZi) # 0.0006126376

df_SoNaBo <- df %>% dplyr::filter(Geography == "South Africa" | Geography == "Namibia" | Geography == "Botswana")
contingency_table_SoNaBo <- xtabs(Count ~ Geography + Religion, data = df_SoNaBo) / 20
Start_time <- Sys.time()
NCor(contingency_table_SoNaBo, CIs = FALSE, Test = FALSE) # 0.6942032
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SoNaBo) # 0.02385454
TschuprowT(contingency_table_SoNaBo) # 0.02385454
ContCoef(contingency_table_SoNaBo) # 0.03371624
ContCoef(contingency_table_SoNaBo, correct = TRUE) # 0.04129379
GoodmanKruskalTau(contingency_table_SoNaBo) # 0.0008405988
Lambda(contingency_table_SoNaBo) # 0
UncertCoef(contingency_table_SoNaBo) # 0.003885619

df_SoZiBo <- df %>% dplyr::filter(Geography == "South Africa" | Geography == "Zimbabwe" | Geography == "Botswana")
contingency_table_SoZiBo <- xtabs(Count ~ Geography + Religion, data = df_SoZiBo) / 20
Start_time <- Sys.time()
NCor(contingency_table_SoZiBo, CIs = FALSE, Test = FALSE) # 0.4530741
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SoZiBo) # 0.03044927
TschuprowT(contingency_table_SoZiBo) # 0.03044927
ContCoef(contingency_table_SoZiBo) # 0.0430219
ContCoef(contingency_table_SoZiBo, correct = TRUE) # 0.05269085
GoodmanKruskalTau(contingency_table_SoZiBo) # 0.001516449
Lambda(contingency_table_SoZiBo) # 0
UncertCoef(contingency_table_SoZiBo) # 0.00325657

df_SoZiMo <- df %>% dplyr::filter(Geography == "South Africa" | Geography == "Zimbabwe" | Geography == "Mozambique")
contingency_table_SoZiMo <- xtabs(Count ~ Geography + Religion, data = df_SoZiMo) / 20
Start_time <- Sys.time()
NCor(contingency_table_SoZiMo, CIs = FALSE, Test = FALSE) # 0.8601702
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SoZiMo) # 0.2590275
TschuprowT(contingency_table_SoZiMo) # 0.2590275
ContCoef(contingency_table_SoZiMo) # 0.3439678
ContCoef(contingency_table_SoZiMo, correct = TRUE) # 0.4212728
GoodmanKruskalTau(contingency_table_SoZiMo) # 0.07597919
Lambda(contingency_table_SoZiMo) # 0.1050648
UncertCoef(contingency_table_SoZiMo) # 0.09594978

df_SoEsMo <- df %>% dplyr::filter(Geography == "South Africa" | Geography == "Eswatini" | Geography == "Mozambique")
contingency_table_SoEsMo <- xtabs(Count ~ Geography + Religion, data = df_SoEsMo) / 100
Start_time <- Sys.time()
NCor(contingency_table_SoEsMo, CIs = FALSE, Test = FALSE) # 0.8734708
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_SoEsMo) # 0.2519945
TschuprowT(contingency_table_SoEsMo) # 0.2519945
ContCoef(contingency_table_SoEsMo) # 0.335694
ContCoef(contingency_table_SoEsMo, correct = TRUE) # 0.4111395
GoodmanKruskalTau(contingency_table_SoEsMo) # 0.1186248
Lambda(contingency_table_SoEsMo) # 0.1459142
UncertCoef(contingency_table_SoEsMo) # 0.119256


## Asia --------------------------------------------------------------------
df_AfIrPa <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "Iran" | Geography == "Pakistan")
contingency_table_AfIrPa <- xtabs(Count ~ Geography + Religion, data = df_AfIrPa) / 10
Start_time <- Sys.time()
NCor(contingency_table_AfIrPa, CIs = FALSE, Test = FALSE) # (pop size: 34,638,683)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfIrPa) # 0.04414479
TschuprowT(contingency_table_AfIrPa) # 0.04414479
ContCoef(contingency_table_AfIrPa) # 0.06230886
ContCoef(contingency_table_AfIrPa, correct = TRUE) # 0.07631246
GoodmanKruskalTau(contingency_table_AfIrPa) # 0.002439188
Lambda(contingency_table_AfIrPa) # 5.723799e-05
UncertCoef(contingency_table_AfIrPa) # 0.005650111

df_AfIrTu <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "Iran" | Geography == "Turkmenistan")
contingency_table_AfIrTu <- xtabs(Count ~ Geography + Religion, data = df_AfIrTu) / 20
Start_time <- Sys.time()
NCor(contingency_table_AfIrTu, CIs = FALSE, Test = FALSE) # 0.8266032
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfIrTu) # 0.05236857
TschuprowT(contingency_table_AfIrTu) # 0.05236857
ContCoef(contingency_table_AfIrTu) # 0.07385806
ContCoef(contingency_table_AfIrTu, correct = TRUE) # 0.09045729
GoodmanKruskalTau(contingency_table_AfIrTu) # 0.001568579
Lambda(contingency_table_AfIrTu) # 0
UncertCoef(contingency_table_AfIrTu) # 0.006271875

df_AfChPa <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "China" | Geography == "Pakistan")
contingency_table_AfChPa <- xtabs(Count ~ Geography + Religion, data = df_AfChPa) / 10
Start_time <- Sys.time()
NCor(contingency_table_AfChPa, CIs = FALSE, Test = FALSE) # (pop size: 39,221,933)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfChPa) # 0.5825267
TschuprowT(contingency_table_AfChPa) # 0.5825267
ContCoef(contingency_table_AfChPa) # 0.6358396
ContCoef(contingency_table_AfChPa, correct = TRUE) # 0.7787413
GoodmanKruskalTau(contingency_table_AfChPa) # 0.4702171
Lambda(contingency_table_AfChPa) # 0.6426304
UncertCoef(contingency_table_AfChPa) # 0.4894227

df_AfChTa <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "China" | Geography == "Tajikistan")
contingency_table_AfChTa <- xtabs(Count ~ Geography + Religion, data = df_AfChTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_AfChTa, CIs = FALSE, Test = FALSE) # 0.9992108 (pop size: 18,075,031: 7.535117 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfChTa) # 0.5071439
TschuprowT(contingency_table_AfChTa) # 0.5071439
ContCoef(contingency_table_AfChTa) # 0.5828104
ContCoef(contingency_table_AfChTa, correct = TRUE) # 0.7137941
GoodmanKruskalTau(contingency_table_AfChTa) # 0.4111258
Lambda(contingency_table_AfChTa) # 0.4920069
UncertCoef(contingency_table_AfChTa) # 0.4439521

df_KyChTa <- df %>% dplyr::filter(Geography == "Kyrgyzstan" | Geography == "China" | Geography == "Tajikistan")
contingency_table_KyChTa <- xtabs(Count ~ Geography + Religion, data = df_KyChTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_KyChTa, CIs = FALSE, Test = FALSE) # 0.9879179 (pop size: 14,770,920: 6.7422 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KyChTa) # 0.3722167
TschuprowT(contingency_table_KyChTa) # 0.3722167
ContCoef(contingency_table_KyChTa) # 0.4658005
ContCoef(contingency_table_KyChTa, correct = TRUE) # 0.5704868
GoodmanKruskalTau(contingency_table_KyChTa) # 0.206571
Lambda(contingency_table_KyChTa) # 0.2567476
UncertCoef(contingency_table_KyChTa) # 0.2694608

df_AfUzTa <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "Uzbekistan" | Geography == "Tajikistan")
contingency_table_AfUzTa <- xtabs(Count ~ Geography + Religion, data = df_AfUzTa) / 10
Start_time <- Sys.time()
NCor(contingency_table_AfUzTa, CIs = FALSE, Test = FALSE) # 0.805714
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfUzTa) # 0.05387009
TschuprowT(contingency_table_AfUzTa) # 0.05387009
ContCoef(contingency_table_AfUzTa) # 0.07596368
ContCoef(contingency_table_AfUzTa, correct = TRUE) # 0.09303613
GoodmanKruskalTau(contingency_table_AfUzTa) # 0.00429082
Lambda(contingency_table_AfUzTa) # 0.00955329
UncertCoef(contingency_table_AfUzTa) # 0.007277241

df_AfUzTu <- df %>% dplyr::filter(Geography == "Afghanistan" | Geography == "Uzbekistan" | Geography == "Turkmenistan")
contingency_table_AfUzTu <- xtabs(Count ~ Geography + Religion, data = df_AfUzTu) / 20
Start_time <- Sys.time()
NCor(contingency_table_AfUzTu, CIs = FALSE, Test = FALSE) # 0.8252327
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_AfUzTu) # 0.06865213
TschuprowT(contingency_table_AfUzTu) # 0.06865213
ContCoef(contingency_table_AfUzTu) # 0.09663439
ContCoef(contingency_table_AfUzTu, correct = TRUE) # 0.1183525
GoodmanKruskalTau(contingency_table_AfUzTu) # 0.004971597
Lambda(contingency_table_AfUzTu) # 0.01037606
UncertCoef(contingency_table_AfUzTu) # 0.01105206

df_ArAsGe <- df %>% dplyr::filter(Geography == "Armenia" | Geography == "Azerbaijan" | Geography == "Georgia")
contingency_table_ArAsGe <- xtabs(Count ~ Geography + Religion, data = df_ArAsGe) / 10
Start_time <- Sys.time()
NCor(contingency_table_ArAsGe, CIs = FALSE, Test = FALSE) # 0.9956016
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ArAsGe) # 0.6477722
TschuprowT(contingency_table_ArAsGe) # 0.6477722
ContCoef(contingency_table_ArAsGe) # 0.6754928
ContCoef(contingency_table_ArAsGe, correct = TRUE) # 0.8273064
GoodmanKruskalTau(contingency_table_ArAsGe) # 0.5438039
Lambda(contingency_table_ArAsGe) # 0.677985
UncertCoef(contingency_table_ArAsGe) # 0.6375703

df_ArAsIr <- df %>% dplyr::filter(Geography == "Armenia" | Geography == "Azerbaijan" | Geography == "Iran")
contingency_table_ArAsIr <- xtabs(Count ~ Geography + Religion, data = df_ArAsIr) / 10
Start_time <- Sys.time()
NCor(contingency_table_ArAsIr, CIs = FALSE, Test = FALSE) # 0.957287 (2.913338 days with new code: 9,943,999 observations)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ArAsIr) # 0.612358
TschuprowT(contingency_table_ArAsIr) # 0.612358
ContCoef(contingency_table_ArAsIr) # 0.6546449
ContCoef(contingency_table_ArAsIr, correct = TRUE) # 0.8017729
GoodmanKruskalTau(contingency_table_ArAsIr) # 0.1631516
Lambda(contingency_table_ArAsIr) # 0.2880976
UncertCoef(contingency_table_ArAsIr) # 0.3455404

df_ArGeTu <- df %>% dplyr::filter(Geography == "Armenia" | Geography == "Georgia" | Geography == "Türkiye")
contingency_table_ArGeTu <- xtabs(Count ~ Geography + Religion, data = df_ArGeTu) / 10
Start_time <- Sys.time()
NCor(contingency_table_ArGeTu, CIs = FALSE, Test = FALSE) # 0.9994344 (2.681872 days with new code, size: 8,912,179)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_ArGeTu) # 0.6671706
TschuprowT(contingency_table_ArGeTu) # 0.6671706
ContCoef(contingency_table_ArGeTu) # 0.6862687
ContCoef(contingency_table_ArGeTu, correct = TRUE) # 0.8405041
GoodmanKruskalTau(contingency_table_ArGeTu) # 0.65486
Lambda(contingency_table_ArGeTu) # 0.676854
UncertCoef(contingency_table_ArGeTu) # 0.7769665

df_RuGeAz <- df %>% dplyr::filter(Geography == "Russia" | Geography == "Georgia" | Geography == "Azerbaijan")
contingency_table_RuGeAz <- xtabs(Count ~ Geography + Religion, data = df_RuGeAz) / 200
Start_time <- Sys.time()
NCor(contingency_table_RuGeAz, CIs = FALSE, Test = FALSE) # 0.9193218
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_RuGeAz) # 0.4069833
TschuprowT(contingency_table_RuGeAz) # 0.4069833
ContCoef(contingency_table_RuGeAz) # 0.4988366
ContCoef(contingency_table_RuGeAz, correct = TRUE) # 0.6109476
GoodmanKruskalTau(contingency_table_RuGeAz) # 0.239268
Lambda(contingency_table_RuGeAz) # 0.2436398
UncertCoef(contingency_table_RuGeAz) # 0.2946125

df_BaMyIn <- df %>% dplyr::filter(Geography == "Bangladesh" | Geography == "Myanmar" | Geography == "India")
contingency_table_BaMyIn <- xtabs(Count ~ Geography + Religion, data = df_BaMyIn) / 4
Start_time <- Sys.time()
NCor(contingency_table_BaMyIn, CIs = FALSE, Test = FALSE) # (pop size: 102,369,033)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BaMyIn) # 0.2580035
TschuprowT(contingency_table_BaMyIn) # 0.2580035
ContCoef(contingency_table_BaMyIn) # 0.3427682
ContCoef(contingency_table_BaMyIn, correct = TRUE) # 0.4198035
GoodmanKruskalTau(contingency_table_BaMyIn) # 0.0977221
Lambda(contingency_table_BaMyIn) # 0.01035134
UncertCoef(contingency_table_BaMyIn) # 0.1420633

df_BhChIn <- df %>% dplyr::filter(Geography == "Bhutan" | Geography == "China" | Geography == "India")
contingency_table_BhChIn <- xtabs(Count ~ Geography + Religion, data = df_BhChIn) / 100
Start_time <- Sys.time()
NCor(contingency_table_BhChIn, CIs = FALSE, Test = FALSE) # 0.8354106
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_BhChIn) # 0.3619365
TschuprowT(contingency_table_BhChIn) # 0.3619365
ContCoef(contingency_table_BhChIn) # 0.4556363
ContCoef(contingency_table_BhChIn, correct = TRUE) # 0.5580383
GoodmanKruskalTau(contingency_table_BhChIn) # 0.2619232
Lambda(contingency_table_BhChIn) # 0.3881754
UncertCoef(contingency_table_BhChIn) # 0.2056646

df_MyChIn <- df %>% dplyr::filter(Geography == "Myanmar" | Geography == "China" | Geography == "India")
contingency_table_MyChIn <- xtabs(Count ~ Geography + Religion, data = df_MyChIn) / 4
Start_time <- Sys.time()
NCor(contingency_table_MyChIn, CIs = FALSE, Test = FALSE) # (pop size: 98,095,708)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_MyChIn) # 0.361211
TschuprowT(contingency_table_MyChIn) # 0.361211
ContCoef(contingency_table_MyChIn) # 0.4549122
ContCoef(contingency_table_MyChIn, correct = TRUE) # 0.5571514
GoodmanKruskalTau(contingency_table_MyChIn) # 0.2463292
Lambda(contingency_table_MyChIn) # 0.3824414
UncertCoef(contingency_table_MyChIn) # 0.1944088

df_NeChIn <- df %>% dplyr::filter(Geography == "Nepal" | Geography == "China" | Geography == "India")
contingency_table_NeChIn <- xtabs(Count ~ Geography + Religion, data = df_NeChIn) / 100
Start_time <- Sys.time()
NCor(contingency_table_NeChIn, CIs = FALSE, Test = FALSE) # 0.8286284
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_NeChIn) # 0.3608089
TschuprowT(contingency_table_NeChIn) # 0.3608089
ContCoef(contingency_table_NeChIn) # 0.4545104
ContCoef(contingency_table_NeChIn, correct = TRUE) # 0.5566593
GoodmanKruskalTau(contingency_table_NeChIn) # 0.2549966
Lambda(contingency_table_NeChIn) # 0.3836609
UncertCoef(contingency_table_NeChIn) # 0.19935

df_PaChIn <- df %>% dplyr::filter(Geography == "Pakistan" | Geography == "China" | Geography == "India")
contingency_table_PaChIn <- xtabs(Count ~ Geography + Religion, data = df_PaChIn) / 10
Start_time <- Sys.time()
NCor(contingency_table_PaChIn, CIs = FALSE, Test = FALSE) # (pop size: 60,683,873)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_PaChIn) # 0.4497253
TschuprowT(contingency_table_PaChIn) # 0.4497253
ContCoef(contingency_table_PaChIn) # 0.5366617
ContCoef(contingency_table_PaChIn, correct = TRUE) # 0.6572737
GoodmanKruskalTau(contingency_table_PaChIn) # 0.1667623
Lambda(contingency_table_PaChIn) # 0.2795852
UncertCoef(contingency_table_PaChIn) # 0.2616956

df_IrIrTu <- df %>% dplyr::filter(Geography == "Iraq" | Geography == "Iran" | Geography == "Türkiye")
contingency_table_IrIrTu <- xtabs(Count ~ Geography + Religion, data = df_IrIrTu)
Start_time <- Sys.time()
NCor(contingency_table_IrIrTu, CIs = FALSE, Test = FALSE) # (pop size: 211,084,821)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IrIrTu) # 0.01868914
TschuprowT(contingency_table_IrIrTu) # 0.01868914
ContCoef(contingency_table_IrIrTu) # 0.02642121
ContCoef(contingency_table_IrIrTu, correct = TRUE) # 0.03235924
GoodmanKruskalTau(contingency_table_IrIrTu) # 0.0004287706
Lambda(contingency_table_IrIrTu) # 0.0001115216
UncertCoef(contingency_table_IrIrTu) # 0.0006731945

df_IrJoSa <- df %>% dplyr::filter(Geography == "Iraq" | Geography == "Jordan" | Geography == "Saudi Arabia")
contingency_table_IrJoSa <- xtabs(Count ~ Geography + Religion, data = df_IrJoSa)
Start_time <- Sys.time()
NCor(contingency_table_IrJoSa, CIs = FALSE, Test = FALSE) # (pop size: 86,925,021)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IrJoSa) # 0.1165814
TschuprowT(contingency_table_IrJoSa) # 0.1165814
ContCoef(contingency_table_IrJoSa) # 0.1626749
ContCoef(contingency_table_IrJoSa, correct = TRUE) # 0.1992352
GoodmanKruskalTau(contingency_table_IrJoSa) # 0.01907033
Lambda(contingency_table_IrJoSa) # 0.04138939
UncertCoef(contingency_table_IrJoSa) # 0.02600075

df_IrJoSy <- df %>% dplyr::filter(Geography == "Iraq" | Geography == "Jordan" | Geography == "Syria")
contingency_table_IrJoSy <- xtabs(Count ~ Geography + Religion, data = df_IrJoSy)
Start_time <- Sys.time()
NCor(contingency_table_IrJoSy, CIs = FALSE, Test = FALSE) # (pop size: 72,579,141)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IrJoSy) # 0.08535811
TschuprowT(contingency_table_IrJoSy) # 0.08535811
ContCoef(contingency_table_IrJoSy) # 0.1198446
ContCoef(contingency_table_IrJoSy, correct = TRUE) # 0.146779
GoodmanKruskalTau(contingency_table_IrJoSy) # 0.009452734
Lambda(contingency_table_IrJoSy) # 0.01877554
UncertCoef(contingency_table_IrJoSy) # 0.01285121

df_IrKuSa <- df %>% dplyr::filter(Geography == "Iraq" | Geography == "Kuwait" | Geography == "Saudi Arabia")
contingency_table_IrKuSa <- xtabs(Count ~ Geography + Religion, data = df_IrKuSa)
Start_time <- Sys.time()
NCor(contingency_table_IrKuSa, CIs = FALSE, Test = FALSE) # (pop size: 80,395,021)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IrKuSa) # 0.1469731
TschuprowT(contingency_table_IrKuSa) # 0.1469731
ContCoef(contingency_table_IrKuSa) # 0.203502
ContCoef(contingency_table_IrKuSa, correct = TRUE) # 0.2492381
GoodmanKruskalTau(contingency_table_IrKuSa) # 0.02262725
Lambda(contingency_table_IrKuSa) # 0.04737382
UncertCoef(contingency_table_IrKuSa) # 0.04167183

df_IrSyTu <- df %>% dplyr::filter(Geography == "Iraq" | Geography == "Syria" | Geography == "Türkiye")
contingency_table_IrSyTu <- xtabs(Count ~ Geography + Religion, data = df_IrSyTu)
Start_time <- Sys.time()
NCor(contingency_table_IrSyTu, CIs = FALSE, Test = FALSE) # (pop size: 144,814,541)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IrSyTu) # 0.09518755
TschuprowT(contingency_table_IrSyTu) # 0.09518755
ContCoef(contingency_table_IrSyTu) # 0.1334121
ContCoef(contingency_table_IrSyTu, correct = TRUE) # 0.1633958
GoodmanKruskalTau(contingency_table_IrSyTu) # 0.006211553
Lambda(contingency_table_IrSyTu) # 0.008628771
UncertCoef(contingency_table_IrSyTu) # 0.01172285

df_IsSyJo <- df %>% dplyr::filter(Geography == "Israel" | Geography == "Syria" | Geography == "Jordan")
contingency_table_IsSyJo <- xtabs(Count ~ Geography + Religion, data = df_IsSyJo) / 40
Start_time <- Sys.time()
NCor(contingency_table_IsSyJo, CIs = FALSE, Test = FALSE) # 0.9553924
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IsSyJo) # 0.5914282
TschuprowT(contingency_table_IsSyJo) # 0.5914282
ContCoef(contingency_table_IsSyJo) # 0.6415742
ContCoef(contingency_table_IsSyJo, correct = TRUE) # 0.7857648
GoodmanKruskalTau(contingency_table_IsSyJo) # 0.2944876
Lambda(contingency_table_IsSyJo) # 0.3950279
UncertCoef(contingency_table_IsSyJo) # 0.4010936

df_IsSyLe <- df %>% dplyr::filter(Geography == "Israel" | Geography == "Syria" | Geography == "Lebanon")
contingency_table_IsSyLe <- xtabs(Count ~ Geography + Religion, data = df_IsSyLe) / 2
Start_time <- Sys.time()
NCor(contingency_table_IsSyLe, CIs = FALSE, Test = FALSE) # 0.9333534 (pop size: 16,974,603 - 4.943895 days with newest code) 
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_IsSyLe) # 0.6628411
TschuprowT(contingency_table_IsSyLe) # 0.6628411
ContCoef(contingency_table_IsSyLe) # 0.6839019
ContCoef(contingency_table_IsSyLe, correct = TRUE) # 0.8376053
GoodmanKruskalTau(contingency_table_IsSyLe) # 0.4639545
Lambda(contingency_table_IsSyLe) # 0.5091708
UncertCoef(contingency_table_IsSyLe) # 0.4653284

df_JeOmSa <- df %>% dplyr::filter(Geography == "Yemen" | Geography == "Oman" | Geography == "Saudi Arabia")
contingency_table_JeOmSa <- xtabs(Count ~ Geography + Religion, data = df_JeOmSa) / 5
Start_time <- Sys.time()
NCor(contingency_table_JeOmSa, CIs = FALSE, Test = FALSE) # 0.8570456 (pop size: 14,193,593: 3.483309 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_JeOmSa) # 0.1187267
TschuprowT(contingency_table_JeOmSa) # 0.1187267
ContCoef(contingency_table_JeOmSa) # 0.165587
ContCoef(contingency_table_JeOmSa, correct = TRUE) # 0.2028018
GoodmanKruskalTau(contingency_table_JeOmSa) # 0.02377955
Lambda(contingency_table_JeOmSa) # 1.682459e-06
UncertCoef(contingency_table_JeOmSa) # 0.03775804

df_CaLaTh <- df %>% dplyr::filter(Geography == "Cambodia" | Geography == "Laos" | Geography == "Thailand")
contingency_table_CaLaTh <- xtabs(Count ~ Geography + Religion, data = df_CaLaTh) / 50
Start_time <- Sys.time()
NCor(contingency_table_CaLaTh, CIs = FALSE, Test = FALSE) # 0.8096422
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CaLaTh) # 0.2991536
TschuprowT(contingency_table_CaLaTh) # 0.2991536
ContCoef(contingency_table_CaLaTh) # 0.3896323
ContCoef(contingency_table_CaLaTh, correct = TRUE) # 0.4772001
GoodmanKruskalTau(contingency_table_CaLaTh) # 0.1182479
Lambda(contingency_table_CaLaTh) # 0.1295398
UncertCoef(contingency_table_CaLaTh) # 0.1507457

df_CaLaVi <- df %>% dplyr::filter(Geography == "Cambodia" | Geography == "Laos" | Geography == "Viet Nam")
contingency_table_CaLaVi <- xtabs(Count ~ Geography + Religion, data = df_CaLaVi) / 50
Start_time <- Sys.time()
NCor(contingency_table_CaLaVi, CIs = FALSE, Test = FALSE) # 0.9096223
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_CaLaVi) # 0.3188073
TschuprowT(contingency_table_CaLaVi) # 0.3188073
ContCoef(contingency_table_CaLaVi) # 0.4110177
ContCoef(contingency_table_CaLaVi, correct = TRUE) # 0.5033919
GoodmanKruskalTau(contingency_table_CaLaVi) # 0.154569
Lambda(contingency_table_CaLaVi) # 0.05314241
UncertCoef(contingency_table_CaLaVi) # 0.1789855

df_KaUzKi <- df %>% dplyr::filter(Geography == "Kazakhstan" | Geography == "Uzbekistan" | Geography == "Kyrgyzstan")
contingency_table_KaUzKi <- xtabs(Count ~ Geography + Religion, data = df_KaUzKi) / 10
Start_time <- Sys.time()
NCor(contingency_table_KaUzKi, CIs = FALSE, Test = FALSE) # 0.888525
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KaUzKi) # 0.2814539
TschuprowT(contingency_table_KaUzKi) # 0.2814539
ContCoef(contingency_table_KaUzKi) # 0.369817
ContCoef(contingency_table_KaUzKi, correct = TRUE) # 0.4529314
GoodmanKruskalTau(contingency_table_KaUzKi) # 0.1121781
Lambda(contingency_table_KaUzKi) # 0.1527477
UncertCoef(contingency_table_KaUzKi) # 0.1243433

df_KaChKi <- df %>% dplyr::filter(Geography == "Kazakhstan" | Geography == "China" | Geography == "Kyrgyzstan")
contingency_table_KaChKi <- xtabs(Count ~ Geography + Religion, data = df_KaChKi) / 10
Start_time <- Sys.time()
NCor(contingency_table_KaChKi, CIs = FALSE, Test = FALSE) # 0.8684223 (pop. size: 15.631.679: 6.978253 days with newest code)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KaChKi) # 0.3320383
TschuprowT(contingency_table_KaChKi) # 0.3320383
ContCoef(contingency_table_KaChKi) # 0.4250446
ContCoef(contingency_table_KaChKi, correct = TRUE) # 0.5205712
GoodmanKruskalTau(contingency_table_KaChKi) # 0.1608366
Lambda(contingency_table_KaChKi) # 0.195312
UncertCoef(contingency_table_KaChKi) # 0.1838828

df_KaChRu <- df %>% dplyr::filter(Geography == "Kazakhstan" | Geography == "China" | Geography == "Russia")
contingency_table_KaChRu <- xtabs(Count ~ Geography + Religion, data = df_KaChRu) / 100
Start_time <- Sys.time()
NCor(contingency_table_KaChRu, CIs = FALSE, Test = FALSE) # 0.5452212
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KaChRu) # 0.2609957
TschuprowT(contingency_table_KaChRu) # 0.2609957
ContCoef(contingency_table_KaChRu) # 0.3462691
ContCoef(contingency_table_KaChRu, correct = TRUE) # 0.4240913
GoodmanKruskalTau(contingency_table_KaChRu) # 0.0301722
Lambda(contingency_table_KaChRu) # 0.09429408
UncertCoef(contingency_table_KaChRu) # 0.07793899

df_KaTuUz <- df %>% dplyr::filter(Geography == "Kazakhstan" | Geography == "Turkmenistan" | Geography == "Uzbekistan")
contingency_table_KaTuUz <- xtabs(Count ~ Geography + Religion, data = df_KaTuUz) / 20
Start_time <- Sys.time()
NCor(contingency_table_KaTuUz, CIs = FALSE, Test = FALSE) # 0.9007929
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KaTuUz) # 0.2867311
TschuprowT(contingency_table_KaTuUz) # 0.2867311
ContCoef(contingency_table_KaTuUz) # 0.3757797
ContCoef(contingency_table_KaTuUz, correct = TRUE) # 0.4602342
GoodmanKruskalTau(contingency_table_KaTuUz) # 0.1136022
Lambda(contingency_table_KaTuUz) # 0.1530217
UncertCoef(contingency_table_KaTuUz) # 0.1281485

df_KiTaUz <- df %>% dplyr::filter(Geography == "Kyrgyzstan" | Geography == "Tajikistan" | Geography == "Uzbekistan")
contingency_table_KiTaUz <- xtabs(Count ~ Geography + Religion, data = df_KiTaUz) / 10
Start_time <- Sys.time()
NCor(contingency_table_KiTaUz, CIs = FALSE, Test = FALSE) # 0.52832
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_KiTaUz) # 0.0695043
TschuprowT(contingency_table_KiTaUz) # 0.0695043
ContCoef(contingency_table_KiTaUz) # 0.0978225
ContCoef(contingency_table_KiTaUz, correct = TRUE) # 0.1198076
GoodmanKruskalTau(contingency_table_KiTaUz) # 0.003164383
Lambda(contingency_table_KiTaUz) # 0
UncertCoef(contingency_table_KiTaUz) # 0.007680969

df_LaMyTh <- df %>% dplyr::filter(Geography == "Laos" | Geography == "Myanmar" | Geography == "Thailand")
contingency_table_LaMyTh <- xtabs(Count ~ Geography + Religion, data = df_LaMyTh) / 4
Start_time <- Sys.time()
NCor(contingency_table_LaMyTh, CIs = FALSE, Test = FALSE) # 0.8200487
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaMyTh) # 0.3626358
TschuprowT(contingency_table_LaMyTh) # 0.3626358
ContCoef(contingency_table_LaMyTh) # 0.4563335
ContCoef(contingency_table_LaMyTh, correct = TRUE) # 0.5588922
GoodmanKruskalTau(contingency_table_LaMyTh) # 0.2321637
Lambda(contingency_table_LaMyTh) # 0.4372856
UncertCoef(contingency_table_LaMyTh) # 0.1934409

df_LaMyCh <- df %>% dplyr::filter(Geography == "Laos" | Geography == "Myanmar" | Geography == "China")
contingency_table_LaMyCh <- xtabs(Count ~ Geography + Religion, data = df_LaMyCh) / 4
Start_time <- Sys.time()
NCor(contingency_table_LaMyCh, CIs = FALSE, Test = FALSE) # (pop size: 34,761,883)
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaMyCh) # 0.04343062
TschuprowT(contingency_table_LaMyCh) # 0.04343062
ContCoef(contingency_table_LaMyCh) # 0.06130465
ContCoef(contingency_table_LaMyCh, correct = TRUE) # 0.07508256
GoodmanKruskalTau(contingency_table_LaMyCh) # 0.00326865
Lambda(contingency_table_LaMyCh) # 0
UncertCoef(contingency_table_LaMyCh) # 0.004993176

df_LaViCh <- df %>% dplyr::filter(Geography == "Laos" | Geography == "Viet Nam" | Geography == "China")
contingency_table_LaViCh <- xtabs(Count ~ Geography + Religion, data = df_LaViCh) / 50
Start_time <- Sys.time()
NCor(contingency_table_LaViCh, CIs = FALSE, Test = FALSE) # 0.833618
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_LaViCh) # 0.08015971
TschuprowT(contingency_table_LaViCh) # 0.08015971
ContCoef(contingency_table_LaViCh) # 0.1126415
ContCoef(contingency_table_LaViCh, correct = TRUE) # 0.1379571
GoodmanKruskalTau(contingency_table_LaViCh) # 0.01258103
Lambda(contingency_table_LaViCh) # 0
UncertCoef(contingency_table_LaViCh) # 0.02549725

df_MoRuCh <- df %>% dplyr::filter(Geography == "Mongolia" | Geography == "Russia" | Geography == "China")
contingency_table_MoRuCh <- xtabs(Count ~ Geography + Religion, data = df_MoRuCh) / 100
Start_time <- Sys.time()
NCor(contingency_table_MoRuCh, CIs = FALSE, Test = FALSE) # 0.337427
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_MoRuCh) # 0.09353833
TschuprowT(contingency_table_MoRuCh) # 0.09353833
ContCoef(contingency_table_MoRuCh) # 0.1311407
ContCoef(contingency_table_MoRuCh, correct = TRUE) # 0.160614
GoodmanKruskalTau(contingency_table_MoRuCh) # 0.01522363
Lambda(contingency_table_MoRuCh) # 0.06474721
UncertCoef(contingency_table_MoRuCh) # 0.01498725

df_NkRuCh <- df %>% dplyr::filter(Geography == "North Korea" | Geography == "Russia" | Geography == "China")
contingency_table_NkRuCh <- xtabs(Count ~ Geography + Religion, data = df_NkRuCh) / 100
Start_time <- Sys.time()
NCor(contingency_table_NkRuCh, CIs = FALSE, Test = FALSE) # 0.333136
End_time <- Sys.time()
End_time - Start_time

CramerV(contingency_table_NkRuCh) # 0.08761395
TschuprowT(contingency_table_NkRuCh) # 0.08761395
ContCoef(contingency_table_NkRuCh) # 0.1229645
ContCoef(contingency_table_NkRuCh, correct = TRUE) # 0.1506002
GoodmanKruskalTau(contingency_table_NkRuCh) # 0.01528578
Lambda(contingency_table_NkRuCh) # 0.06422365
UncertCoef(contingency_table_NkRuCh) # 0.01381666
