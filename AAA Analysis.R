library(tidyverse)
library(tidymodels)
library(rsample)
library(xgboost)
library(rpart.plot)
#set directory
setwd("C:/Users/William/Dropbox (IMC)/2021_Synthetic_Data")
#read raw data from csv
#this may be multiple files for years and/or for Professional/Inpatient/Outpatient
D<- read.csv("SIMDATA_SAMPLE_MSK_10-19-21_ET_02.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
ICD<- read.csv("MSK_ICD_Codes_wCLASS-210907-34-WSM.csv",header = TRUE,stringsAsFactors = FALSE)
CPT<- read.csv("MSK_CPT-DRG-REV_wCLS.MDN-211013-36-WSM.csv",header = TRUE,stringsAsFactors = FALSE)
##Grab the code to create the mMaxPL level
D.1<-subset(D,!is.na(D$BRalpha))#subset for BRalias to figure MaxPL for MSD
table(D.1$BRalpha)#review
D.1$PL<-CPT$PL[match(D.1$PX,CPT$Code)]#Match the PX in D to reference list CPT
table(D.1$PL)#review
mMaxPL <-aggregate(PL~BRID,data=D.1,max)#aggregate to build maxPL reference
head(mMaxPL)#review
D.1$mMaxPL<-mMaxPL$PL[match(D.1$BRID,mMaxPL$BRID)]#use the match command to create a vector which reports values based on match criteria
table(D.1$mMaxPL)#review

#subset the balance of rows.
D.2<-subset(D,is.na(D$BRalpha))
length(D.2$BRID)
D.2$mMaxPL<-"NA"#create "NA" mMaxPL for balance on D.2
D.2$PL<-"NA"#create "NA" PL for balance on D.2

#recombine     
D<-rbind(D.1,D.2)

##Recoding some PL's for strange CPT codes
D <- within(D, PL[CPT == "'99232" & AMOUNT < 150] <- 3)
D <- within(D, PL[CPT == "'99233" & AMOUNT < 150] <- 3)
D <- within(D, PL[CPT == "'99220" & AMOUNT < 220] <- 3)
D <- within(D, PL[CPT == "'99222" & AMOUNT < 170] <- 3)
D <- within(D, PL[CPT == "'99231" & AMOUNT < 170] <- 3)
D <- within(D, PL[CPT == "'99239" & AMOUNT < 150] <- 3)
D <- within(D, PL[CPT == "'99223" & AMOUNT < 130] <- 3)
D <- within(D, PL[CPT == "'99223" & AMOUNT < 250] <- 5)
D <- within(D, PL[CPT == "'99226" & AMOUNT < 200] <- 3)
D <- within(D, PL[CPT == "'99226" & AMOUNT < 300] <- 4)
D <- within(D, PL[CPT == "'99225" & AMOUNT < 100] <- 3)
D <- within(D, PL[CPT == "'20936" & AMOUNT < 100] <- 3)
D <- within(D, PL[CPT == "'99221" & AMOUNT < 140] <- 3)
D <- within(D, PL[CPT == "'99219" & AMOUNT < 160] <- 3)



##First RUN BREAK



D$Current_age = round(as.numeric(difftime(Sys.Date(),D$DOB, units = "weeks"))/52.25)
##Creating Current Age

##Now time to do some analysis for those non-specific surgery charges

SURGMAX9 <- subset(D, D$mMaxPL == 9)
SURGMAX9

SURG9 <- subset(D, D$PL == 9)
SURG9

SURG9CPT <- as.vector(SURG9$CPT)
SURG9TAB <- table(SURG9CPT)
SURG9DF <- as.data.frame(SURG9TAB)

SURG9BP <- ggplot(data = SURG9DF, mapping = aes(SURG9CPT, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("CPT Codes in Surgeries")
SURG9BP
##Pretty useless, lets sub set to where Freq > 400

SURG9400 <- as.data.frame(subset(SURG9DF, SURG9DF$Freq > 400))
SURG9400
SURG9400VEC <- as.vector(SURG9400$SURG9CPT)


SURG9400BP <- ggplot(data = SURG9400, mapping = aes(SURG9400VEC, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("CPT Codes in Surgeries (>400 cases)")
SURG9400BP

## What to make of the surgeries with no CPT code? Most of them are thousands and 
## thousands of dollars. What to do with these?
NACPT <- subset(SURG9, SURG9$CPT == "")
NACPT

NACPTDRGVEC <- as.vector(NACPT$DRG)
NACPTDRGTAB <- table(NACPTDRGVEC)
NACPTDRGDF <- as.data.frame(NACPTDRGTAB)

NACPTDRGBP <- ggplot(data = NACPTDRGDF, mapping = aes(NACPTDRGVEC, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("DRG Codes of Surgeries with NO CPT Code")
NACPTDRGBP

## That 470 DRG code is for Major Joint Replacements...
JR <- subset(SURG9, SURG9$DRG == "'470")
JRAGE <- as.vector(JR$Current_age)

p <- ggplot(JR, aes(x=Current_age)) + 
  geom_density() +
  ggtitle("Age Density of Patients for Joint Replacement") +
  geom_density(color="darkblue")
p
##Doesn't really show much. We know this


##How about distribution of surgery cases in SURG9400 by Body Region?
SURG9BRVEC <- as.vector(SURG9$BRalpha)
SURG9BRTAB <- table(SURG9BRVEC)
SURG9BRDF <- as.data.frame(SURG9BRTAB)

SURG9BRBP <- ggplot(data = SURG9BRDF, mapping = aes(SURG9BRVEC, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("Cases by Body Region where PL = 9 (AAA)")
SURG9BRBP

## Ok,now how about the cost by body region for SURG9400


##Let's look at the Under Subsequent Hospital Care Services codes
SHCS <- subset(D, D$CPT == "'99223" | D$CPT == "'99232")
SHCSVEC <- as.vector(SHCS$CPT)
SHCSAM <- as.vector(SHCS$AMOUNT)
SHCSDF <- as.data.frame(SHCSVEC,SHCSAM)

SHCSBP <- ggplot(data = SHCSDF, mapping = aes(SHCSAM, SHCSVEC)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,500)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "CPT Code") + 
  ggtitle("Distribution of Costs for Subsequent Hospital Care Services")
SHCSBP
##Alright, so what is up with these super cheap surgeries??

##Any body region in particular they are being focused on?
SHCSBRVEC <- as.vector(SHCS$BRalpha)
SHCSBRAM <- as.vector(SHCS$AMOUNT)
SHCSBRDF <- as.data.frame(SHCSBRVEC,SHCSBRAM)

SHCSBRBP <- ggplot(data = SHCSBRDF, mapping = aes(SHCSBRAM, SHCSBRVEC)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,500)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "CPT Code") + 
  ggtitle("Distribution of Costs for Subsequent Hospital Care Services by Body Region")
SHCSBRBP

##Lets look at cost distribution for body regions in SURG9400
SURG9400BRR <- subset(SURG9, SURG9$CPT == "'27130" | SURG9$CPT == "'27447" |
                     SURG9$CPT == "'29826" | SURG9$CPT == "'29827"
                     | SURG9$CPT == "'64635" | SURG9$CPT == "'64636"
                     | SURG9$CPT == "'99220" | SURG9$CPT == "'99223"
                     | SURG9$CPT == "'99222" | SURG9$CPT == "'99231"
                     | SURG9$CPT == "'99232" | SURG9$CPT == "'99233"
                     | SURG9$CPT == "'99239")
SURG9400BRRVEC <- as.vector(SURG9400BRR$BRalpha)
SURG9400BRRAM <- as.vector(SURG9400BRR$AMOUNT)
SURG9400BRRDF <- as.data.frame(SURG9400BRRVEC, SURG9400BRRAM)

SURG9400BRRBP <- ggplot(data = SURG9400BRRDF, mapping = aes(SURG9400BRRAM, SURG9400BRRVEC)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,4000)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "Body Region",
    title = "Distribution of costs by Body Region (where PL = 9 & #Cases >400)")
SURG9400BRRBP

##Lets take a peak at the distribution of costs in SURG9400 by cpt codes
SURG9400CPT <- as.vector(SURG9400BRR$CPT)
SURG9400CPTDF <- as.data.frame(SURG9400CPT, SURG9400BRRAM)












## Trying some loops to create new count variables

# Probably want to do a 'mutate' function in dplyr package
PLCOUNT <- D %>% 
  group_by(EMID) %>% 
  count(EMID == TRUE)

PLCOUNT <- PLCOUNT %>% 
  filter(`EMID == TRUE` == FALSE)

PLCOUNT <- select(PLCOUNT, EMID, n)

PL9COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 9) %>% 
  drop_na()

PL8COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 8) %>% 
  drop_na()

PL7COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 7) %>% 
  drop_na()

PL6COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 6) %>% 
  drop_na()

PL5COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 5) %>% 
  drop_na()

PL4COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 4) %>% 
  drop_na()

PL3COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 3) %>% 
  drop_na()

PL2COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 2) %>% 
  drop_na()

PL1COUNT <- D %>% 
  group_by(EMID) %>%
  count(PL == 1) %>% 
  drop_na()

PL9COUNT <- PL9COUNT %>% 
  filter(`PL == 9` == TRUE)

PL8COUNT <- PL8COUNT %>% 
  filter(`PL == 8` == TRUE)

PL7COUNT <- PL7COUNT %>% 
  filter(`PL == 7` == TRUE)

PL6COUNT <- PL6COUNT %>% 
  filter(`PL == 6` == TRUE)

PL5COUNT <- PL5COUNT %>% 
  filter(`PL == 5` == TRUE)

PL4COUNT <- PL4COUNT %>% 
  filter(`PL == 4` == TRUE)

PL3COUNT <- PL3COUNT %>% 
  filter(`PL == 3` == TRUE)

PL2COUNT <- PL2COUNT %>% 
  filter(`PL == 2` == TRUE)

PL1COUNT <- PL1COUNT %>% 
  filter(`PL == 1` == TRUE)
  
PL9COUNT <- select(PL9COUNT, EMID, n)

PL8COUNT <- select(PL8COUNT, EMID, n)

PL7COUNT <- select(PL7COUNT, EMID, n)

PL6COUNT <- select(PL6COUNT, EMID, n)

PL5COUNT <- select(PL5COUNT, EMID, n)

PL4COUNT <- select(PL4COUNT, EMID, n)

PL3COUNT <- select(PL3COUNT, EMID, n)

PL2COUNT <- select(PL2COUNT, EMID, n)

PL1COUNT <- select(PL1COUNT, EMID, n)



D99 <- D

D99 <- D99 %>% 
  full_join(PLCOUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PLCOUNT = n)

D99 <- D99 %>% 
  full_join(PL1COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL1COUNT = n)

D99 <- D99 %>% 
  full_join(PL2COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL2COUNT = n)

D99 <- D99 %>% 
  full_join(PL3COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL3COUNT = n)

D99 <- D99 %>% 
  full_join(PL4COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL4COUNT = n)

D99 <- D99 %>% 
  full_join(PL5COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL5COUNT = n)

D99 <- D99 %>% 
  full_join(PL6COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL6COUNT = n)

D99 <- D99 %>% 
  full_join(PL7COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL7COUNT = n)

D99 <- D99 %>% 
  full_join(PL8COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL8COUNT = n)

D99 <- D99 %>% 
  full_join(PL9COUNT, by = c("EMID" = "EMID"))

D99 <- D99 %>% 
  rename(PL9COUNT = n)
##Ok we have some PL counts now

##How about some break down of the distribution of PL counts?

##First need unique EMID data set, will still have all counts correct

DUNIQ <- D99[!duplicated(D99$EMID),]

COUNTDF <- DUNIQ %>% 
  select(PLCOUNT, PL2COUNT, PL3COUNT, PL4COUNT, PL5COUNT, PL6COUNT, PL7COUNT, PL8COUNT, PL9COUNT)




## Trying Boosted Decision Tree Models
set.seed(600)
D70 <- as_tibble(D99)

D70[is.na(D70)] = 0
drop <- c("DRG","REV", "Class", "X", "POS", "ZIP", "BRalpha", "DATE", "BRID", "PX",
          "ICD10")

D70 = D70[,!(names(D70) %in% drop)]

D70[sapply(D70, is.character)] <- lapply(D70[sapply(D70, is.character)], 
                                       as.factor)

D70$PL <- as.factor(D70$PL)
D70$mMaxPL <- as.factor(D70$mMaxPL)
D70$DOB <- as.Date(D70$DOB)
D70 <- D70 %>% 
  mutate(
    mMaxPL9 = ifelse(mMaxPL == 9, TRUE, FALSE)
  )
D70$mMaxPL9 <- as.factor(D70$mMaxPL9)

ids <- sample(unique(D70$EMID), 3000) # important: the UNIQUE id numbers
D69 <- D70[D70$EMID %in% ids, ]

drop2 <- c("EMID")
D69 = D69[,!(names(D69) %in% drop2)]

D69split <- initial_split(D69)
D69_train <- training(D69split)
D69_test <- testing(D69split)


folds <- vfold_cv(D69_train, v = 3)

spec <- decision_tree(tree_depth = 15) %>% 
  set_mode("classification") %>% 
  set_engine("rpart")

df_recipe <- recipe(mMaxPL9 ~ PL1COUNT + PL2COUNT + PL3COUNT +
                      PL4COUNT + PL5COUNT + PL6COUNT + PL7COUNT,
                    data = D69_train)

#workflow
tree_wf <- workflow() %>%
  add_recipe(df_recipe) %>%
  add_model(spec) %>%
  fit(D69_train) #results are found here 


rpart.plot(tree_wf$fit$fit$fit, roundint = FALSE)


###Maybe let us try this with body region
set.seed(600)
D60 <- as_tibble(D99)
D60 <- D60 %>% 
  subset(D60$BRalpha == "SPIN")

D60[is.na(D60)] = 0
drop <- c("DRG","REV", "Class", "X", "POS", "ZIP", "BRalpha", "DATE", "BRID", "PX",
          "ICD10")

D60 = D60[,!(names(D60) %in% drop)]

D60[sapply(D60, is.character)] <- lapply(D60[sapply(D60, is.character)], 
                                         as.factor)

D60$PL <- as.factor(D60$PL)
D60$mMaxPL <- as.factor(D60$mMaxPL)
D60$DOB <- as.Date(D60$DOB)
D60 <- D60 %>% 
  mutate(
    mMaxPL9 = ifelse(mMaxPL == 9, TRUE, FALSE)
  )
D60$mMaxPL9 <- as.factor(D60$mMaxPL9)


drop2 <- c("EMID")
D68 = D68[,!(names(D68) %in% drop2)]


D68split <- initial_split(D68)
D68_train <- training(D68split)
D68_test <- testing(D68split)


folds <- vfold_cv(D68_train, v = 3)

spec <- decision_tree(tree_depth = 15) %>% 
  set_mode("classification") %>% 
  set_engine("rpart")

df_recipe <- recipe(mMaxPL9 ~ PL1COUNT + PL2COUNT + PL3COUNT +
                      PL4COUNT + PL5COUNT + PL6COUNT + PL7COUNT,
                    data = D68_train)

#workflow
tree_wf <- workflow() %>%
  add_recipe(df_recipe) %>%
  add_model(spec) %>%
  fit(D68_train) #results are found here 


rpart.plot(tree_wf$fit$fit$fit, roundint = FALSE)

##Lets try to tune a grid
tune_spec <- decision_tree(tree_depth = tune(),
                      min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("rpart")

#create Grid
tree_grid <- grid_regular(
  parameters(tune_spec),
  levels = 3
)

#Tune Grid
tune_results <- tune_grid(
  tune_spec,
  mMaxPL9 ~ PL1COUNT + PL2COUNT + PL3COUNT +
    PL4COUNT + PL5COUNT + PL6COUNT + PL7COUNT,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(accuracy)
)

autoplot(tune_results)

#Use best params
final_params <- select_best(tune_results)
best_spec <- finalize_model(tune_spec,
                            final_params)

#Rerun with best params
tune_tree_wf <- workflow() %>%
  add_recipe(df_recipe) %>%
  add_model(best_spec) %>%
  fit(D68_train) #results are found here 

rpart.plot(tune_tree_wf$fit$fit$fit, roundint = FALSE) #It worked for Spine!!