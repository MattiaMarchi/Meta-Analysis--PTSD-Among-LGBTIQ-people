###----------------------------------------------------------------------------------------
###------------------------Meta Analysis - PTSD in LGBTIQ people---------------------------
###----------------------------------------------------------------------------------------
library(meta)
library(metafor)
library(tidyverse)
library(metaforest)

#Import data
df <- structure(list(Publication.Year = c(2022L, 2020L, 2022L, 2016L, 2015L, 2019L, 2022L, 2020L, 2016L, 2021L, 2021L,2009L, 2021L, 2021L, 2014L, 2022L, 2018L, 2020L, 2010L, 2012L, 2019L, 2022L, 2022L, 2019L, 2021L, 2015L, 2004L),
                     Author = structure(1:27, levels = c("Alba et al. 2022","Bettis et al. 2020", "Brewerton et al. 2022", "Brown et al. 2016", "Burns et al. 2015", "Caceres et al. 2019", "Carey et al. 2022", "Evans-Polce et al. 2020", "Flentje et al. 2016", "Hao et al. 2021", 
                                                         "Harper et al. 2021", "Hatzenbuehler et al. 2009", "Holloway et al. 2021", "Jeffery et al. 2021", "Lehavot et al. 2014", "Livingston et al. 2022", "Lucas et al. 2018", "McDonald et al. 2020", "Mustanski et al. 2010", "Roberts et al. 2012", "Rodriguez-Seijas et al. 2019", "Schefter et al. 2022", 
                                                         "Terra et al. 2022", "Walukevich-Dienst et al. 2019", "Wang et al. 2021", "Weiss et al. 2015", "Whitbeck et al. 2004"), class = "factor"), 
                     Name.of.cohort = structure(c(1L, 1L, 1L, 17L, 1L, 18L, 15L, 1L, 14L, 8L, 10L, 12L, 3L, 2L, 1L, 16L, 1L, 4L, 6L, 7L, 13L, 1L, 5L, 9L, 11L, 1L, 1L), levels = c("", "2015 Health-Related Behavior Survey of Active Duty Military Personnel", 
                                                                                                                                                                                   "Active duty military service members (Data from the Department of Defense-funded study Improving Acceptance, Integration and Health among LGBT Service Members)", 
                                                                                                                                                                                   "Active duty U.S. soldiers enrolled in the Defense Language Institute Foreign Language Center", "BHRCS", "Community sample ethnically diverse youths who self-identified as LGBT", "Growing Up Today Study", "Homeless young people who utilized services from Larkin Street Youth Services, a community-based organization (CBO) in San Francisco", 
                                                                                                                                                                                   "Louisiana State University", "Members of the LGBTQ community in Kenia", "MSD Cohort", "National Epidemiologic Survey on Alcohol and Related Conditions (NESARC II; N=34653)", "National Epidemiologic Survey on Alcohol and Related Conditions-III (NESARC-III)", 
                                                                                                                                                                                   "San Francisco biennial homeless survey", "US Military", "Veterans Health Administration (VHA)", "Veterans Integrated Service Networks (VISNs)", "Wave 3 Chicago Health and Life Experiences of Women Study (CHLEW)"), class = "factor"),
                     Date = structure(c(17L, 19L, 19L, 3L,9L, 10L, 14L, 11L, 15L, 18L, 1L, 6L, 18L, 15L, 12L, 4L, 13L, 16L, 8L, 7L, 11L, 20L, 21L, 1L, 5L, 2L, 1L), levels = c("", "1995-2007", "1996-2013", "1999-2021", "2000-2012", "2004-2005", "2007", "2007-2008", "2009-2013", "2010-2012", "2012-2013", "2013", "2014-2015", "2014-2016", "2015", "2016", "2017", "2017-2018", "2017-2019", "2017-2020", "NR"), class = "factor"), 
                     Country = structure(c(1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 2L, 4L, 4L, 4L, 4L), levels = c("Australia", "Brazil", "Kenia", "USA"), class = "factor"), Study.Design = structure(c(2L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 4L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 1L, 2L, 3L), levels = c("Cohort Study", "Cross-sectional Study", "Longitudinal Cohort Study", "Longitudinal Retrospective Study"), class = "factor"),
                     N.Homo....Females. = structure(c(17L, 13L, 10L, 1L, 12L, 11L, 6L, 18L, 2L, 9L, 7L, 1L, 1L, 14L, 8L, 1L, 1L, 1L, 5L, 7L, 15L, 4L, 16L, 3L, 1L, 13L, 1L), levels = c("", "100 (56%)", "13", "14", "152", "1824", "196", "209", "26", "29", "323", "329", "38", "424", "581", "60", "756", "NR"), class = "factor"),
                     N.Bi....Females. = structure(c(1L, 2L, 14L, 1L, 18L, 3L, 5L, 19L, 17L, 7L, 8L, 1L, 1L, 10L, 11L, 1L, 1L, 1L, 15L, 6L, 13L, 16L, 4L, 9L, 1L, 12L, 1L), levels = c("", "125", "137", "151", "1614", "172", "25", "250", "31", "439", "55", "57", "581", "69", "70", "8", "88 (75%)", "96", "NR"), class = "factor"),
                     N.Trans....Females. = structure(c(1L, 1L, 1L, 7L, 1L, 1L, 1L, 12L, 6L, 5L, 9L, 1L, 8L, 1L, 1L, 11L, 1L, 1L, 3L, 1L, 1L, 2L, 10L, 1L, 4L, 1L, 1L), levels = c("", "1", "20", "2890", "3", "49 (67.3%)/cis 962 (35.2%)", "5135", "58", "62", "9", "9995", "NR"), class = "factor"),
                     Others....Females. = structure(c(1L, 4L, 9L, 1L, 8L, 11L, 1L, 12L, 10L, 7L, 5L, 1L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 7L, 1L, 3L, 1L), levels = c("", "1", "14", "15", "16", "187", "2", "24", "29", "54 (38.9%)", "87", "NR"), class = "factor"), N.Sexual.Minority....Females. = c(NA, 178L, 127L, 5135L, NA, NA, 3438L, 3203L, 242L, 56L, NA, 577L, 245L, 863L, 264L, 9995L, 110L, 67L, NA, 368L, 1162L, 22L, 221L, 46L, 2890L, 109L, 63L),
                     N.Hetero....Females. = c(NA, 266L, 415L, 15405L, NA, NA, 93492L, 32593L, 714L, 44L, NA, 34076L, 295L, 13542L, 442L, 29985L, 330L, 573L, NA, 7828L, 32425L, 379L, 1241L, 393L, 8670L, 368L, 366L), Females.. = structure(c(1L, 19L, 22L, 10L, 2L, 3L, 9L, 1L, 11L, 4L, 1L, 1L, 24L, 6L, 21L, 13L, 17L, 20L, 8L, 23L, 12L, 14L, 7L, 16L, 5L, 18L, 15L), levels = c("", "0", "100", "100 (28%)", "11560 (12.4%)", "14405 (37.6%)", "1475 (47.8%)", "246 (50.8%)", "29.2", "30", "35.1", "36309 (56.3%)", "39980 (35.5%)", "401 (100%)", "428 (56.3%)", 
                                                                                                                                                                                                                                                                                                                                                                                      "439 (100%)", "440 (39.1%)", "477 (100%)", "57.5", "640 (26.6%)", "706 (100%)", "96.7", "9784 (62.8%)", "tot:544 (59.6% male, 29.8% female and 10.7% transgender)"), class = "factor"), 
                     Age.Mean..SD. = structure(c(21L, 4L, 13L, 19L, 9L, 6L, 16L, 2L, 17L, 12L, 2L, 3L, 14L, 1L, 18L, 1L, 2L, 2L, 8L, 10L, 7L, 20L, 1L, 11L, 2L, 15L, 5L), levels = c("", "≥18", "≥25", "14.7 (1.8)", "17.4 (1.05)", "18-75", "18-90", "18.3 (1.3)", "18.9 (1.3)", "19-27", "20.7 (2.5)", "21.7 (NR)", "25.0 (7.2)", "27.7 (6.1)", "36.1 (10.5)", "40 (NR)", "41.8 (14.0)", "49.8 (13.5)", "55.8 (13.5)", "60 (11.0)", "NR"), class = "factor"), 
                     Outcome.PTSD = structure(c(10L, 6L, 14L, 13L, 9L, 25L, 1L, 16L, 8L, 20L, 18L, 3L, 20L, 23L, 21L, 11L, 22L, 24L, 7L, 2L, 15L, 17L, 4L, 19L, 12L, 5L, 26L), levels = c("17-item PTSD Checklist−Civilian Version", "7-item Short Screening Scale for DSM-IV PTSD; cut-off ≥6 symptoms", "AUDADIS-IV to assess past 12-month DSM-IV", "Brazilian version of the Development and Well-Being Behavior Assessment (DAWBA) interview based on DSM-IV", 
                                                                                                                                                                                          "CAPS (Clinician Administered PTSD Scale for DSM-IV)", "Children’s Interview for Psychiatric Syndromes (ChIPS)", "Diagnostic Interview Schedule for Children (DISC) version 4.0 based on DSM-IV", "Do you suffer from or are you undergoing a treatment for PTSD", "DSM-IV", "Have you ever been diagnosed with PTSD?", "ICD", "ICD-9", "ICD9", "LEC-5 and PCL - 5", "Past-year DSM-5 diagnosis", 
                                                                                                                                                                                          "Past-year PTSD", "Post-traumatic Stress Disorder Checklist for DSM-5 (PCL-5); cut-off ≥33", "Primary Care Post-Traumatic Stress Disorder (PC-PTSD); cut-off ≥3", "PTSD Checklist for DSM-5 (PCL-5) Life Events Checklist for DSM-5 (LEC-5)", "PTSD Checklist for DSM-5 (PCL-5); cut-off ≥33", "PTSD Checklist- Civilian Version (PCL-C); cut-off ≥50", 
                                                                                                                                                                                          "PTSD Checklist–Civilian Version (PCL-C); cut-off ≥50", "PTSD CheckList—Civilian Version cut-off ≥50", "PTSD DSM-5 (PTSD Checklist-5); cut-off ≥38", "Short screening PTSD scale", "UM-CIDI (University of Michigan Composite International Diagnostic Interview) based on DSM III-R"), class = "factor"), 
                     N.PTSD.LGBTIQ = c(NA, 56L, 80L, 1989L, NA, NA, 332L, 430L, 70L, 46L, NA, 75L, 38L, 88L, 103L, 4691L, 40L, 10L, NA, 126L, 437L, 3L, 11L, 10L, 562L, 95L, 30L), N.PTSD.non.LGBTIQ = c(NA, 51L, 188L, 2770L, NA, NA, 5312L, 1575L, 194L, 33L, NA, 2180L, 18L, 1110L, 141L, 8127L, 81L, 41L, NA, 830L, 4183L, 13L, 28L, 48L, 824L, 281L, 122L),
                     N.PTSD.LGBTIQ.each = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L), levels = c("", "LGB"), class = "factor"), N_G = c(513L, NA, NA, NA, 329L, NA, 657L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), E_G = c(39L, NA, NA, NA, 44L, NA, 45L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), N_L = c(243L, NA, NA, NA, NA, 410L, 1167L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                     E_L = c(46L, NA, NA, NA, NA, 133L, 91L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), N_LG = c(756L, NA, NA, NA, 329L, 410L, 1824L, NA, 100L, NA, NA, NA, NA, 424L, NA, NA, NA, NA, 152L, 196L, NA, NA, NA, NA, NA, NA, NA), E_LG = c(85L, NA, NA, NA, 44L, 133L, 136L, NA, 30L, NA, NA, NA, NA, 73L, NA, NA, NA, NA, 17L, 63L, NA, NA, NA, NA, NA, NA, NA), N_B = c(NA, NA, NA, NA, 96L, 137L, 1614L, NA, 88L, NA, NA, NA, NA, 439L, NA, NA, NA, NA, 70L, 172L, NA, NA, NA, NA, NA, NA, NA),
                     E_B = c(NA, NA, NA, NA, 22L, 67L, 196L, NA, 18L, NA, NA, NA, NA, 120L, NA, NA, NA, NA, 5L, 63L, NA, NA, NA, NA, NA, NA, NA), N_others = c(NA, NA, NA, NA, 24L, NA, NA, NA, 54L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), E_others = c(NA, NA, NA, NA, 6L, NA, NA, NA, 22L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), N_T = c(NA, NA, NA, 5135L, NA, NA, NA, NA, 49L, NA, 62L, NA, 58L, NA, NA, 9995L, NA, NA, 20L, NA, NA, NA, NA, NA, 2890L, NA, NA),
                     E_T = c(NA, NA, NA, 1989L, NA, NA, NA, NA, 28L, NA, 40L, NA, 18L, NA, NA, 4691L, NA, NA, 2L, NA, NA, NA, NA, NA, 562L, NA, NA), N_cis = c(NA, NA, NA, 15405L, NA, NA, NA, NA, 962L, NA, 462L, NA, 187L, NA, NA, 29985L, NA, NA, 222L, NA, NA, NA, NA, NA, 8670L, NA, NA), E_cis = c(NA, NA, NA, 2770L, NA, NA, NA, NA, 265L, NA, 236L, NA, 20L, NA, NA, 8127L, NA, NA, 22L, NA, NA, NA, NA, NA, 824L, NA, NA), Note = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = c("", "cis here are SM"), class = "factor"),
                     Females...1 = structure(c(1L, 17L, 19L, 9L, 2L, 3L, 7L, 1L, 10L, 6L, 1L, 1L, 8L, 12L, 3L, 11L, 13L, 5L, 15L, 18L, 16L, 3L, 14L, 3L, 4L, 3L, 16L), levels = c("", "0", "100", "12.4", "26.6", "28", "29.2", "29.8", "30", "35.1", "35.5", "37.6", "39.1", "47.8", "50.8", "56.3", "57.5", "62.8", "96.7"), class = "factor"), Age.Mean..SD..1 = structure(c(1L, 2L, 8L, 14L, 5L, 1L, 11L, 1L, 12L, 7L, 1L, 1L, 9L, 1L, 13L, 1L, 1L, 1L, 4L, 1L, 1L, 15L, 1L, 6L, 1L, 10L, 3L), levels = c("", "14.7", "17.4", "18.3", "18.9", "20.7", "21.7", "25", "27.7", "36.1", "40", "41.8", "49.8", "55.8", "60"), class = "factor"), 
                     Outcome.PTSD.1 = structure(c(8L, 2L, 7L, 4L, 3L, 9L, 1L, 8L, 8L, 7L, 6L, 3L, 7L, 7L, 7L, 4L, 6L, 7L, 3L, 3L, 3L, 7L, 3L, 7L, 5L, 3L, 3L), levels = c("17-item PTSD Checklist−Civilian Version", "CHIPS", "DSM", "ICD", "ICD-9", "PC-PTSD", "PCL-5", "Self-reported", "Short screening PTSD scale"), class = "factor"), OutcomeMeasure = structure(c(3L, 2L, 2L, 1L, 1L, 2L, 2L, 3L, 3L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L), levels = c("Manual", "Scale", "Self-reported"), class = "factor")), class = "data.frame", row.names = c(NA, -27L))

###----------------------------------------------------------------------------------------
###------------1. Meta Analysis of PTSD in LGBTIQ and Controls (Hetero/Cis)----------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
smvsc <- metabin(event.e = N.PTSD.LGBTIQ, n.e = N.Sexual.Minority....Females.,
                 event.c = N.PTSD.non.LGBTIQ, n.c = N.Hetero....Females.,
                 data = df, studlab = Author, sm = "OR")
smvsc
#Forest plot
forest.meta(smvsc, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "LGBTIQ", label.c = "Controls",
            label.left = "Favours LGBTIQ", label.right = "Favours controls", allstudies = F)
#----------------------------------Publication Bias
#Funnel Plot
funnel(smvsc, xlab = "Hedges' g")
#Egger's test
smvsc_rma <- escalc(measure = "OR",
                    ai = N.PTSD.LGBTIQ, ci = N.PTSD.non.LGBTIQ,
                    n1i = N.Sexual.Minority....Females., n2i = N.Hetero....Females.,
                    data = df)
#Pooling ES
m_re <- rma(yi = smvsc_rma$yi, vi = smvsc_rma$vi)
m_re
regtest(m_re)
#Perform trim-and-fill analysis
m_taf <- trimfill(m_re)
m_taf

###----------------------------------------------------------------------------------------
###--------------2. Meta Analysis of PTSD in LG and Controls (Hetero/Cis)------------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
LGvsC <- metabin(event.e = E_LG, n.e = N_LG,
                 event.c = N.PTSD.non.LGBTIQ, n.c = N.Hetero....Females.,
                 data = df, studlab = Author, sm = "OR")
LGvsC
#Forest plot
forest.meta(LGvsC, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "LG", label.c = "Controls",
            label.left = "Favours LG", label.right = "Favours controls", allstudies = F)
#----L vs G
#Perform meta-analysis
LvsG <- metabin(event.e = E_L, n.e = N_L,
                event.c = E_G, n.c = N_G,
                data = df, studlab = Author, sm = "OR")
LvsG
#Forest plot
forest.meta(LvsG, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "L", label.c = "G",
            label.left = "Favours L", label.right = "Favours G", allstudies = F)

###----------------------------------------------------------------------------------------
###--------------3. Meta Analysis of PTSD in B and Controls (Hetero/Cis)-------------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
BvsC <- metabin(event.e = E_B, n.e = N_B,
                event.c = N.PTSD.non.LGBTIQ, n.c = N.Hetero....Females.,
                data = df, studlab = Author, sm = "OR")
BvsC
#Forest plot
forest.meta(BvsC, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "B", label.c = "Controls",
            label.left = "Favours B", label.right = "Favours controls", allstudies = F)

###----------------------------------------------------------------------------------------
###--------------4. Meta Analysis of PTSD in Q and Controls (Hetero/Cis)-------------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
QvsC <- metabin(event.e = E_others, n.e = N_others,
                event.c = N.PTSD.non.LGBTIQ, n.c = N.Hetero....Females.,
                data = df, studlab = Author, sm = "OR")
QvsC
#Forest plot
forest.meta(QvsC, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "Q", label.c = "Controls",
            label.left = "Favours Q", label.right = "Favours controls", allstudies = F)

###----------------------------------------------------------------------------------------
###-----------------5. Meta Analysis of PTSD in T and Controls (Cis)-----------------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
df$E_cis
TvsC <- metabin(event.e = E_T, n.e = N_T,
                event.c = E_cis, n.c = N_cis,
                data = df, studlab = Author, sm = "OR")
TvsC
#Forest plot
forest.meta(TvsC, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "T", label.c = "Controls",
            label.left = "Favours T", label.right = "Favours controls", allstudies = F)

###----------------------------------------------------------------------------------------
###----------------------6. Meta Analysis of PTSD in LG and B------------------------------
###----------------------------------------------------------------------------------------
#Perform meta-analysis
df$N_l
LGvsB <- metabin(event.e = E_B, n.e = N_B,
                 event.c = E_LG, n.c = N_LG,
                 data = df, studlab = Author, sm = "OR")
LGvsB
#Forest plot
forest.meta(LGvsB, layout = "RevMan5", digits.sd = 2, random = T, fixed = F,
            label.e = "B", label.c = "LG",
            label.left = "Favours B", label.right = "Favours LG", allstudies = F)

###----------------------------------------------------------------------------------------
###----7. Leave-one-out and Meta regression of PTSD in LGBTIQ and Controls (Hetero/Cis)----
###----------------------------------------------------------------------------------------
#Perform meta-analysis
smvsc_rma <- escalc(measure = "OR",
                    ai = N.PTSD.LGBTIQ, ci = N.PTSD.non.LGBTIQ,
                    n1i = N.Sexual.Minority....Females., n2i = N.Hetero....Females.,
                    data = df)
m_re <- rma(yi = smvsc_rma$yi, vi = smvsc_rma$vi)
m_re
#---------Leave-one-out analysis
SMvsC_loo <- leave1out(m_re, transf=exp, progbar = T)
SMvsC_loo
#---------Meta-Regression
#-----Univariable
smvsc_rma$Age <- as.character(smvsc_rma$Age.Mean..SD..1)
smvsc_rma$Age <- as.numeric(smvsc_rma$Age)
#Age
m_uni_age <- rma(yi = yi,
                 vi = vi,
                 mods = ~ Age,
                 data = smvsc_rma)
m_uni_age
#sex
smvsc_rma$Females <- as.character(smvsc_rma$Females...1)
smvsc_rma$Females <- as.numeric(smvsc_rma$Females)
m_uni_Fem <- rma(yi = yi,
                 vi = vi,
                 mods = ~ Females,
                 data = smvsc_rma)
m_uni_Fem
#country
smvsc_rma$Country
m_uni_Country <- rma(yi = yi,
                     vi = vi,
                     mods = ~ Country,
                     data = smvsc_rma)
m_uni_Country
#measure of PTSD
m_uni_Meas <- rma(yi = yi,
                  vi = vi,
                  mods = ~ OutcomeMeasure,
                  data = smvsc_rma)
m_uni_Meas