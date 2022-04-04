##Starting to build better cost models
setwd("C:/Users/ws15e/OneDrive - Florida State Students/Documents/R")
D <- read.csv(file = "SynDat1154420_Processed-220323-01-WSM.csv", stringsAsFactors = FALSE, header = TRUE)
A <- read.csv(file = "MSK_CPT-DRG-REV_wCLS.MDN-211013-36-WSM.csv", stringsAsFactors = FALSE, header = TRUE)

require(tidyr)
require(tidyverse)

##Alrighty, let us start out by looking at CPT codes for this data set

D$amount[D$amount < 0] <- NA           ##First off, let us replace all negative numbers to missing

CPT_count <- as.data.frame(table(D$cpt)) #Quick table of cpt counts

CPT_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT Counts")  ##Just a quikc visualization (ugly)

CPTLcount <- subset(CPT_count, Freq > 9000)    ## How about the CPT codes with more than 9000 claims?

CPTLcount %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT counts (with more than 9000 Claims)")  # Visualization of those CPT codes


##Now, we cannot look at all of these, how about when we exclude zeros?
DN0 <- subset(D, D$amount != 0)
N0 <- as.numeric(nrow(D) - nrow(DN0))
P0 <- as.numeric((nrow(DN0) - nrow(D))/nrow(D))    ##So, we have 249,268 claims with a zero amount


##About 29.944% of our cases have a zero for amount



##How about the CPT count of cases with non-zero for amount?
CPT_N0_count <- as.data.frame(table(DN0$cpt))

CPT_N0_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT with Non-Zero Amount Count")   #Quick visualization



##Which CPT codes have the most non-zeros?
CPT_LN0_count <- subset(CPT_N0_count, CPT_N0_count$Freq > 9000)    #Another subset of Freq > 9000 (No-zero amounts)

CPT_LN0_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT with Non-Zero Amount & > 9000 Claims Count")    # Visualization of those CPT codes

##Now to try and see percentage of Zeros by CPT code
count_joined <- CPT_count %>% 
  inner_join(CPT_N0_count, by = c("Var1" = "Var1"))

count_joined <- count_joined %>% 
  mutate(
    percent0 = scales::percent(round(abs((Freq.y - Freq.x)/Freq.x), 3))
  )

count_joined <- count_joined %>% 
  arrange(percent0)

drop <- c("percentzero")
count_joined = count_joined[,!(names(count_joined) %in% drop)]
##This data frame shows percentage of cases that have an actual amount in the amount tab





##Everything above is for the entire data set, not just including things that are in scoop
##Now, let us subset the data so that only CPT codes that are in scoop are focused on
inscoopcode <- as.vector(A$Code)
inscoopcode <- gsub("\\'","",inscoopcode) #Getting rid of leading apostrophes

IN <- D %>% 
  subset(D$cpt %in% inscoopcode)
##This should be a data frame of our original data but only including In-Scoop Procedures
IN_CPT_count <- as.data.frame(table(IN$cpt))

IN_CPT_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT Counts (In Scope)")

IN_CPTL_count <- subset(IN_CPT_count, Freq > 9000)

IN_CPTL_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT counts (with more than 9000 Claims & In Scope) ")

##Now, we cannot look at all of these, how about when we exclude zeros?
INN0 <- subset(IN, IN$amount != 0)
N0 <- as.numeric(nrow(IN) - nrow(INN0))
P0 <- as.numeric((nrow(INN0) - nrow(IN))/nrow(IN))

##So, we have 162,447 claims with a zero amount
##About 22.144% of our cases have a zero for amount

##How about the CPT count of cases with non-zero for amount?
IN_0_count <- as.data.frame(table(INN0$cpt))

IN_0_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT with Non-Zero Amount Count (In Scope)")

##Which CPT codes have the most Non-Zeros?
IN_L0_count <- subset(IN_0_count, IN_0_count$Freq > 9000)

IN_L0_count %>% 
  ggplot(mapping = aes(Var1, Freq)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("CPT with Non-Zero Amount & > 9000 Claims Count (In Scope)")

##Now to try and see percentage of Non-Zeros by CPT code
IN_Joined <- IN_CPT_count %>% 
  inner_join(IN_0_count, by = c("Var1" = "Var1"))

IN_Joined <- IN_Joined %>% 
  mutate(
    percent0 = scales::percent(round(abs((Freq.y - Freq.x)/Freq.x), 3))
  )

IN_Joined <- IN_Joined %>% 
  arrange(percent0)

drop <- c("percentzero")
IN_Joined = IN_Joined[,!(names(IN_Joined) %in% drop)]


##Let us get some descriptive statistics for our in-scope CPT codes
IN_Joined %>% 
  ggplot(mapping = aes(Var1,percent0)) +
  geom_bar(
    stat = 'identity', alpha = 0.2, color = "blue", fill = "light blue",
    width = 0.3) +
  ggtitle("Percentage of Claims that are Amount = 0 (By CPT code In-Scope)")


##Now we should look at cost distribution with and without zeros for out in-scope procedures
INSCOPE <- IN %>% 
  select(cpt,amount)

INSCOPE$cpt <- as.factor(INSCOPE$cpt)

INSCOPE %>% 
  ggplot(mapping = aes(x = cpt,y = amount)) +
  geom_boxplot(outlier.shape = NULL)
  
##Ok, that is super ugly. Let us just look at the top 20 CPT codes by number of cases
IN_Joined <- IN_Joined %>% 
  arrange(desc(Freq.x))
  
TOPIN <- IN_Joined[1:30,]
NUMcasesIN <- as.numeric(sum(TOPIN$Freq.x))
(528624/733598)


##So the top 30 In-Scope CPT codes account for %72.06 of our total in-scope data set
## Let us do a little break down of how many prominent CPT codes encomapss our data

TOP80_percentIN <- IN_Joined[1:50,]
NUM_Cases80 <- as.numeric(sum(TOP80_percentIN$Freq.x))
(NUM_Cases80 / 733598)

TOP90_percentIN <- IN_Joined[1:100,]
NUM_Cases90 <- as.numeric(sum(TOP90_percentIN$Freq.x))
(NUM_Cases90 / 733598)

TOP95_percentIN <- IN_Joined[1:175,]
NUM_Cases95 <- as.numeric(sum(TOP95_percentIN$Freq.x))
(NUM_Cases95 / 733598)







##We will focus on these for now
IN_30_VEC_CPT <- as.vector(TOPIN$Var1)

IN30FRAME <- IN %>% 
  filter(cpt %in% IN_30_VEC_CPT)

IN30CPTAM <- IN30FRAME %>% 
  select(cpt,amount)

IN30CPTAM %>% 
  ggplot(mapping = aes(x = factor(cpt), y = amount)) +
  geom_boxplot(outlier.shape = NULL)
  
##This was with all of our Zero's, now how about when we ignore them?
NO_IN_0 <- IN30FRAME %>% 
  filter(amount != 0)
(413367/733598)

##Only %56.35 of our in-scope top 30 cpt codes have Non-Zero values in Amount

NO_IN_0 %>% 
  ggplot(mapping = aes(factor(cpt), amount)) +
  geom_boxplot(outlier.shape = NULL)

##Interestingly, removing zeros from these cpt codes did not change much




##Now trying to add the summary stats to columns in A (probably gonna use loops again)
IN_N0_sum <- tapply(INN0$amount, INN0$cpt, summary) #This works super well(No-Zero's)


##Simple tappply allows us to get easy array of summary statistics
IN_sum <- tapply(IN$amount, IN$cpt, summary) #(Has Zero's)


##Must create emtpy frame to be joined with A once filled
empty <- as.data.frame(matrix(ncol = 6, nrow = 870))
nZERO <- as.data.frame(matrix(ncol = 6, nrow = 860))




##Loop to fill frame with summary statistics in each column of their own(For No-Zero's)
for(i in 1:870){
  empty[i,] = IN_sum[[i]]
}
  

for(r in 1:860){
  nZERO[r,] = IN_N0_sum[[r]]
}

##Grab the dimension names from our array so we can join on some variable
vecdims <- as.vector(dimnames(IN_sum))

empty[,7] <- vecdims  #For No-Zero's


nZerodims <- as.vector(dimnames(IN_N0_sum))
nZERO[,7] <- nZerodims



##Renaming variables so we know which numbers they actually are
SUM_IN_Y0 <- empty %>% 
  mutate(
    CPT = empty$V7,
    `Min(Zeros)` = empty$V1,
    `1st Qtr(zeros)` = empty$V2,
    `Median(Zeros)` = empty$V3,
    `Mean(Zeros)` = empty$V4,
    `3rd Qtr(Zeros` = empty$V5,
    `Max(Zeros)` = empty$V6
    
  )



#Now to get rid of those dumb other generated names
vecc <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7")
SUM_IN_Y0 = SUM_IN_Y0[,!(names(SUM_IN_Y0) %in% vecc)]


##Have to add that ' in front of all the codes so they will match up with A when we join
SUM_IN_Y0$CPT <- sub("^", "'", SUM_IN_Y0$CPT)


#Doing the join
CPTFRAME <- SUM_IN_Y0 %>% 
  inner_join(A, by = c("CPT" = "Code"))


#Now we have the old and current Medians
##Have to take care of those
CPTFRAME <- CPTFRAME %>% 
  mutate(
    OldMedian = CPTFRAME$Median.y,
    CurrentMedian = CPTFRAME$Median.x
  )

badnames <- c("Median.y", "Median.x")
CPTFRAME = CPTFRAME[,!(names(CPTFRAME) %in% badnames)]


##Fixing up the new summary statistics without zero's
SUM_IN_N0 <- nZERO %>% 
  mutate(
    CPT = nZERO$V7,
    `Min(NO-Zeros)` = nZERO$V1,
    `1st Qtr(NO-zeros)` = nZERO$V2,
    `Median(NO-Zeros)` = nZERO$V3,
    `Mean(NO-Zeros)` = nZERO$V4,
    `3rd Qtr(NO-Zeros)` = nZERO$V5,
    `Max(NO-Zeros)` = nZERO$V6
    
  )


SUM_IN_N0 = SUM_IN_N0[,!(names(SUM_IN_N0) %in% vecc)]


##Have to add that ' in front of all the codes so they will match up with A when we join
SUM_IN_N0$CPT <- sub("^", "'", SUM_IN_N0$CPT)


#Doing the join
CPTFRAME_N0 <- SUM_IN_N0 %>% 
  inner_join(A, by = c("CPT" = "Code"))


#Now we have the old and current Medians
##Have to take care of those
CPTFRAME_N0 <- CPTFRAME_N0 %>% 
  mutate(
    OldMedian = CPTFRAME_N0$Median.y,
    CurrentMedian = CPTFRAME_N0$Median.x
  )


CPTFRAME_N0 = CPTFRAME_N0[,!(names(CPTFRAME_N0) %in% badnames)]


FINAL_FRAME_CPT <- CPTFRAME_N0 %>% 
  inner_join(CPTFRAME, by = c("CPT" = "CPT"))


OUT <- c("Procedure.x", "PL.x", "PLalias.x", "TYPE.x", "Sig.Nois.x", "CLN.x", "CLASS.x", "Median.x")
FINAL_FRAME_CPT = FINAL_FRAME_CPT[,!(names(FINAL_FRAME_CPT) %in% OUT)]

FINALCOST <- FINAL_FRAME_CPT
##Our updated CPT statistics^

# IN_Joined has a percentage of In-Scope CPT codes that have a zero amount(FREQ.x is number of Claims that CPT code has
# FREQ.y is the number of non-zero Amounts for that CPT code. So that percentage is the percentage of Zero-Amount
# for that CPT code.)





## Alright, let us try and do the counts and everything by unique EMID like we talked about

INCOUNT <- IN %>% 
  group_by(emid) %>% 
  distinct(cpt, .keep_all = TRUE) # Okay cool, this produced a data frame
                                  # where each EMID only has unique CPT codes
                                  # Not quite sure which value for AMOUNT it decided to grap so some questions there

IN_UNIQUE_COUNT <- as.data.frame(table(INCOUNT$cpt))
      #Count of cpt codes where each EMID only has unique codes by group

IN_EMID_COUNT <- as.data.frame(table(INCOUNT$emid))
      # Count of unique CPT codes by EMID

TOTAL_IN_EMID <- as.numeric(nrow(IN_EMID_COUNT))
      # We have 44,749 EMID's in this data set with in-scope CPT codes




  
  
  
  






