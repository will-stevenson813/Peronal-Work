library(tidyverse)
##Setting Directory
setwd("C:/Users/William/Desktop")
DATA <- read.csv("BA1.0.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)

##Reviewing Variable Names
names(DATA)

##Now time to clean
##First want make sure dates are coded as dates
head(DATA$Date)
year <- as.vector(substring(DATA$Date, 1, 4))
table(year)
month <- as.vector(substring(DATA$Date, 5, 7))
table(month)
day <- "-01"
newdate <- paste(year, month, day, sep="", collapse = NULL)
head(newdate)
finaldate <- as.Date(newdate)
DATA$Date <- finaldate
##Good, now the dates are fixed
##Now time to separate those codes from descriptions

splitcodes <- str_extract(DATA$ICD10,"(\\w+)")
trimicd <- trimws(splitcodes, which = "left",)
description <- sapply(sapply(strsplit(DATA$ICD10, split = ' '), function(x) x[2: length(x)]), paste, collapse = ' ')
head(description)
donedescription <- trimws(description, which = "left",)

cbind(DATA, donedescription)
##ICD10 codes and their description cut off. Now time for CPT codes
splitcpt <- str_extract(DATA$Procedure,"(\\w+)")
trimcpt <- trimws(splitcpt, which = "left",)
trimcpt
nchcpt <- as.vector(nchar(trimcpt))

ifelse(nchcpt != 5, CPTDESCRIP <- DATA$Procedure, CPTDESCRIP <- "a")
cptdes <- sapply(sapply(strsplit(DATA$Procedure, split = ' '), function(x) x[2: length(x)]), paste, collapse = ' ')
DATA$Procedure <- cptdes
##Ok, that should be all the cleaning of codes, time to put them back together


CPTCODE <- splitcpt
ICD10 <- splitcodes
ICD10DESCRIP <- donedescription
PROCEDUREDESCRIP <- cptdes
BA2_0 <- cbind(DATA, ICD10DESCRIP)
BA2_0 <- cbind(BA2_0, PROCEDUREDESCRIP)
BA2_0$Procedure <- CPTCODE
BA2_0$ICD10 <- ICD10
##OK, it is not quite perfect yet, but close enough for now


##Now we start thinking about Emma's question


##How can we search for lumped in Px 9 claims that were not Px 9

##Need to add the apostrophe in front of CPT codes so they match ones on CPT Sheet

cpt<-sub(" .*","",BA2_0$Procedure)
cpt<-trimws(cpt)
cpt<-substr(cpt,1,5)
cpt<-paste("'",cpt,sep="")
cpt<-ifelse(cpt=="'","NA",cpt)
BA2_0$Procedure<-cpt



setwd("C:/Users/William/Dropbox (IMC)/2021_Synthetic_Data")
#read pre-processed data from csv
ICD<- read.csv("MSK_ICD_Codes_wCLASS-210907-34-WSM.csv",header = TRUE,stringsAsFactors = FALSE)
CPT<- read.csv("MSK_CPT-DRG-REV_wCLS.MDN-211013-36-WSM.csv",header = TRUE,stringsAsFactors = FALSE)
BA2_0$BRALIAS<-ICD$BRALIAS[match(BA2_0$ICD10,ICD$Dx)]

BA2_0$BRID<-paste(BA2_0$EMID,BA2_0$BRALIAS,sep="-")

D.1<-subset(BA2_0,!is.na(BA2_0$BRALIAS))#subset for BRalias to figure MaxPL for MSD
table(D.1$BRALIAS)#review
D.1$PL<-CPT$PL[match(D.1$Procedure,CPT$Code)]#Match the PX in D to reference list CPT
table(D.1$PL)#review
mMaxPL <-aggregate(PL~BRID,data=D.1,max)#aggregate to build maxPL reference
head(mMaxPL)#review
D.1$mMaxPL<-mMaxPL$PL[match(D.1$BRID,mMaxPL$BRID)]#use the match command to create a vector which reports values based on match criteria
table(D.1$mMaxPL)#review

#subset the balance of rows.
D.2<-subset(BA2_0,is.na(BA2_0$BRALIAS))
length(D.2$BRID)
D.2$mMaxPL<-"NA"#create "NA" mMaxPL for balance on D.2
D.2$PL<-"NA"#create "NA" PL for balance on D.2

#recombine
##This will be BA4_0 CSV
BA2_0<-rbind(D.1,D.2)


##Alright, lets take a look at who did not get a PL for whatever reason
NAPX <- subset(BA2_0,is.na(BA2_0$PL))
table(BA2_0$Procedure)

##Lets do some messing around looking at services that usually go along with other
##This example, lets look at "Ambulatory Surgery"

AMBLT9 <- subset(BA2_0, BA2_0$Procedure == "'Ambul" & BA2_0$mMaxPL < 9)
AMBLT9  

AMBN9 <- subset(BA2_0, BA2_0$Procedure == "'Ambul")
AMBN9
##Important here, 
(60/468) ##About 13 percent of Ambulatory 

##Looks like we have a couple of people who were hit with the 'Ambulatory Surgery'
##but did not get hit with the PL = 9. Maybe we can learn something from these individual

EMRGLT9 <- subset(BA2_0, BA2_0$Procedure == "'Emerg" & BA2_0$mMaxPL < 9)
EMRGLT9

##Looking at some of the observations where if not for unused code

URGLT9 <- subset(BA2_0, BA2_0$Procedure == "'Urgen" & BA2_0$mMaxPL < 9)
URGLT9
URG <- subset(BA2_0, BA2_0$Procedure == "'Urgen" & BA2_0$mMaxPL == 9)

##Nothing really to comment on "Urgent Care"
##Now lets look at some density curves to see what is similar about these people
##age

p <- ggplot(AMBLT9, aes(x=Age, color = Gender)) + 
  geom_density() +
  ggtitle("Age Density of Patients") +
  geom_density(color="darkblue")
p

##How about code counts
AMBSURG <- AMBLT9
SURGICD <- as.vector(AMBSURG$ICD10)
tabSURG <- table(SURGICD)
AMBSURGDF <- as.data.frame(tabSURG)

AMBSURGBP <- ggplot(data = AMBSURGDF, mapping = aes(SURGICD, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("ICD-10 Codes in Ambulatory Surgery")
AMBSURGBP

##Maybe lets look at "other services" to see what we get for those people
OTR <- subset(BA2_0, BA2_0$Procedure == "'Other")
OTR

##Now lets look at the distribution of these "Ambulatory Services" 
## costs by each body region

SURGBR <- as.vector(AMBLT9$BRALIAS)
SURGAM <- as.vector(AMBLT9$Allowed)
SURGDF <- as.data.frame(SURGBR,SURGAM)

SURGBP <- ggplot(data = SURGDF, mapping = aes(SURGAM,SURGBR)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,5000)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "Body Region") + 
  ggtitle("Distribution of Costs for Ambulatory Surgery (By Body Region)")
SURGBP

##Ok, thats cool. Now lets try "Surgical Supplies"
##Ok, not in this data set. Let us move it over to a new script for AAA
##For here, what about "Operating Room Services"?
OP <- subset(BA2_0, BA2_0$Procedure == "'Opera")
OP
OPLT9 <- subset(BA2_0, BA2_0$Procedure == "'Opera" & BA2_0$mMaxPL < 9)

##Age density graph of age of patients who's Operating Room Services
##were the only PL = 9 found.
OPLT9DG <- ggplot(OPLT9, aes(x=Age, color = Gender)) + 
  geom_density() +
  ggtitle("Age Density of Patients") +
  geom_density(color="darkblue")
OPLT9DG

##How about cost distribution by Body Region
OPLT9BR <- as.vector(OPLT9$BRALIAS)
OPLT9AM <- as.vector(OPLT9$Allowed)
OPLT9DF <- as.data.frame(OPLT9BR,OPLT9AM)


OPLT9BP <- ggplot(data = OPLT9DF, mapping = aes(OPLT9AM,OPLT9BR)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,40000)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "Body Region") + 
  ggtitle("Distribution of Costs for Operating Room Services (By Body Region)")
OPLT9BP

##Lets try and ignore the outlier
OPLT9BP2 <- ggplot(data = OPLT9DF, mapping = aes(OPLT9AM,OPLT9BR)) +
  geom_boxplot(alpha = 0.2, color="blue", fill = "light blue") +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0,20000)) +
  stat_summary(
    geom = "point",
    fun = "mean") +
  labs (
    x = "Amount($)",
    y = "Body Region") + 
  ggtitle("Distribution of Costs for Operating Room Services (By Body Region)")
OPLT9BP2

##How about the ICD10 codes for these people?
OPLT9ICD <- as.vector(OPLT9$ICD10)
tabOPLT9 <- table(OPLT9ICD)
OPLT9ICDDF <- as.data.frame(tabOPLT9)

OPLT9ICDBP <- ggplot(data = OPLT9ICDDF, mapping = aes(OPLT9ICD, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("ICD-10 Codes in Operating Room Services")
OPLT9ICDBP ##Ok this is a little cluttered. Need to makie a cutoff for # of Claims

NSPCLT9 <- subset(BA2_0, BA2_0$Procedure == "'Opera" | BA2_0$Procedure == "'Ambul" & BA2_0$mMaxPL < 9)
NSPCLT9

NSPCLT9BR <- as.vector(NSPCLT9$BRALIAS)
NSPCLT9TAB <- table(NSPCLT9BR)
NSPCLT9DF <- as.data.frame(NSPCLT9TAB)


NSPCLT9BP <- ggplot(data = NSPCLT9DF, mapping = aes(NSPCLT9BR, Freq)) +
  geom_bar(stat = 'identity', alpha = 0.2, color="blue", fill = "light blue",
           width = 0.3) +
  ggtitle("Cases by Body Region of Non-Specific CPT codes")
NSPCLT9BP