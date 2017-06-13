install.packages("knitr", dependencies = T)

getwd()
setwd('C:/Users/pneupane/Documents/study topics/udacity -Data Analyst ND/P4 EDA/project')
ls()


install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')
install.packages('RCurl')
install.packages('bitops')



#install.packages('gridExtra') 
library(gridExtra) 
library(grid)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(RCurl)
library(bitops)

library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(reshape)
library(plyr)
library(RColorBrewer)



# Read in the loan dataset
loan = read.csv("prosperLoanData.csv")
dim(loan)
#[1] 113937     81

colnames(loan)
str(loan)

# DS details
#This data set contains 113,937 loans with 81 variables on each loan, 
#including loan amount, borrower rate (or interest rate), current loan status, 
#borrower income, borrower employment status, borrower credit history, and the latest payment information.

# What to look for?
#Ask your own questions about this data set. There are MANY variables in this 
#data set and you are not expected to explore all of them. You should explore 
#between 10-15 variables in your analysis.

summary(loan)


splom(~  loan[c("CreditGrade","Term","LoanStatus", "BorrowerAPR",
          "EstimatedLoss","EstimatedReturn"  )] )




######### IDEAS ######### 
# Study of attributes affecting the Loan Status of past loan
#
# - Compare atrributes of loans with Completed vs. Delinquent vs. Past Due status
#     - Attributes: what was their Prosper Score/CreditGrade/Occupation/EmploymentStatus/HomeownerStatus/DebtToIncomeRatio, etc?
#
# - Is DateCreditPulled same or close to date loan originated? If not, does it mean credit can be pulled after the loan is approved?
#   If yes, how many time can it be pulled? Does this affect the loan terms?
#
# - 





###---------------------------------------------------------------------
### Histogram of Loan Status

ggplot(aes(x=LoanStatus), data = loan) + 
  geom_histogram(stat="count") +
  coord_flip() 

###---------------------------------------------------------------------
#### Histogram of Prosper score
ggplot(aes(x=ProsperScore), data = loan) + 
  geom_histogram(stat="count") +
  coord_flip() 


###---------------------------------------------------------------------
#### Histogram of Credit Score Lower value
ggplot(aes(x=CreditScoreRangeLower), data = subset(loan,CreditScoreRangeLower>0)) + 
  geom_histogram(stat="count") 


###---------------------------------------------------------------------
### Boxplot of loan term
ggplot(aes(x=LoanStatus_mod ,y=DelinquenciesLast7Years), data = subset(loan,IncomeVerifiable=='True' & DelinquenciesLast7Years>0)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,20))


ggplot(aes(x=ProsperScore), data = loan) + 
  geom_histogram(stat="count") +
  coord_flip() +
  labs(x="Prosper Score",y="Count of loans") +
  facet_grid(.~LoanStatus_mod)



###---------------------------------------------------------------------
### Relationshop between Loan Status and ProsperScore
#### Is ProsperScore a good indicator that the loan will be paid off??
ggplot(aes(x=ProsperScore,y=LoanStatus), data = loan) +
  geom_jitter(alpha=1/20) +
  scale_x_continuous(limits=c(1,10),breaks=seq(1,10,1))
  

#------ THose with score of 10 seem to have lower cases with Chanrgedoffs, defaults, and past-dues 
#        compared to other scores. These buckets might just have less people in them overall.


### Combine Past Due (>60 days), Defaulted and Chargedoff into one bucket
table(loan$LoanStatus)
loan$LoanStatus_mod = as.character(loan$LoanStatus)
loan$LoanStatus_mod[loan$LoanStatus %in% c("Past Due (31-60 days)","Past Due (61-90 days)", "Past Due (91-120 days)", "Past Due (>120 days)","Chargedoff", "Defaulted")] ="Bad Loan"
loan$LoanStatus_mod[loan$LoanStatus %in% c("Past Due (1-15 days)",  "Past Due (16-30 days)")] ="Past Due (<30 days)"
loan$LoanStatus_mod[loan$LoanStatus %in% c("FinalPaymentInProgress")] ="Completed"


### Remove cancelled
loan = loan[loan$LoanStatus != "Cancelled",]
table(loan$LoanStatus_mod)


print(levels(loan$IncomeRange))

# reorder levels for Income Range
loan$IncomeRange = factor(loan$IncomeRange,levels=c("Not displayed","Not employed","$0","$1-24,999",     
                                                    "$25,000-49,999", "$50,000-74,999", "$75,000-99,999","$100,000+")
)


### Relationshop between Loan Status and ProsperScore
#### Is ProsperScore a good indicator that the loan will be paid off??
ggplot(aes(x=ProsperScore), data = loan) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod), position="fill") +
  scale_x_continuous(breaks=seq(0,10,1))
# ------ Loans with higher Prosper-score seems to have smaller proportion of bad loans

# Remove Current Loan Status
ggplot(aes(x=ProsperScore), data = subset(loan,LoanStatus !='Current')) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod), position="fill") +
  labs(y="Percentage of counts") +
  scale_x_continuous(breaks=seq(0,10,1))
# ------ Ditto. Prosper Scores below 7 seem to have same proportion of bad loans




levels = c(-Inf,500,600,700,800,900,Inf)
labels = c("<500","500-600","600-700","700-800","800-900","900+")

loan = 
  loan %>%
    mutate(CreditScoreBucket=cut(CreditScoreRangeLower,levels, labels=labels))


# Split by Credit Score
ggplot(aes(x=ProsperScore), data = subset(loan,LoanStatus !='Current' & !(CreditScoreBucket %in% c("<500","900+")) & !is.na(CreditScoreBucket) ) ) +
  geom_bar(stat="count", aes(fill=LoanStatus_mod), position="fill") +
  labs(y="Percentage of counts") +
  scale_x_continuous(breaks=seq(0,10,1)) +
  facet_grid(.~ CreditScoreBucket )
# ------ Credit scores 900 (800 to 900) seem to have very less proportion of Bad loans for higher Prosper Scores.
#        but for lower Prosper scores in the same range, the proportion of Bad loans are same or higher compared to other 
#        Credit Score ranges 




a=summary(loan$Term)

a=t(t(a))
names(a)[1] <- "Summary of Loan Term"
a[1]

cbind(' ' = 'Summary of Loan Term',a)



ggplot(aes(y=InquiriesLast6Months,x=CreditScoreRangeLower, color=LoanStatus_mod, group=LoanStatus_mod), data = loan) +
  geom_line(stat='summary', fun.y = mean, aes(color=LoanStatus_mod), size=1 ) +
  scale_x_continuous(limits = c(350,900)) 


ggplot(aes(x=CreditScoreRangeLower, color=LoanStatus_mod, group=LoanStatus_mod), data = loan) +
  geom_density() +
  scale_x_continuous(limits = c(350,900))


ggplot(aes(x=ProsperScore, color=LoanStatus_mod, group=LoanStatus_mod), data = loan) +
  geom_density() +
  scale_x_continuous(limits = c(0,1))

###---------------------------------------------------------------------
### Relationship between Loan Status and Credit-Score
ggplot(aes(x=LoanStatus_mod ,y=CreditScoreRangeLower), data = subset(loan,CreditScoreRangeLower>0)) +
  geom_boxplot()
# ------ IQR range for Bad Loans are lower compared to other Loan Status

summary(loan$CreditScoreRangeLower)

# Study of effect of Income Range
  # Summarize Credit Score at Income Range and Loan Status

table(loan$IncomeRange)

loan.mean = 
  subset(loan,CreditScoreRangeLower>0) %>% 
  group_by(IncomeRange, LoanStatus_mod) %>%
  summarise(mean.CreditScore = mean(CreditScoreRangeLower) ) %>%
  arrange(IncomeRange,LoanStatus_mod)




# Plot of mean Credit Scores for each Loan Status and Income Range
ggplot(aes(x=IncomeRange,y=mean.CreditScore, color=LoanStatus_mod, group=LoanStatus_mod), data = loan.mean) +
  geom_line(size=1)
  
# ------ In general, the credit scores for people with bad loans are lower compared to that for other groups
#        Surprisingly, overall credit scores for unemployed people are fairly high. This means that we cannot just look at credit scores
#        to determine the credit-worthiness of an individual







###---------------------------------------------------------------------
### Relationships between Number of Current Delinquencies for each Loan Status
ggplot(aes(x=LoanStatus_mod ,y=DelinquenciesLast7Years), data = subset(loan,IncomeVerifiable=='True' & DelinquenciesLast7Years>0)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,20))



###---------------------------------------------------------------------
### Relationships between Inquiries in last 6 months  and Debt to income ratio
ggplot(aes(x=IncomeRange,y=DebtToIncomeRatio), data = loan) +
  geom_point()

# Zoom in
ggplot(aes(x=IncomeRange,y=DebtToIncomeRatio), data = loan) +
  geom_jitter( aes(color=LoanStatus_mod)) +
  scale_colour_brewer(type = 'div') 


+ 
  coord_cartesian(xlim=c(0,1),ylim=c(0,50))



###---------------------------------------------------------------------
### Relationshop between Inquiries in last 6 months  and Debt to income ratio
ggplot(aes(x=DebtToIncomeRatio,y=InquiriesLast6Months), data = loan) +
  geom_jitter(aes(color=LoanStatus_mod)) +
  scale_x_continuous(limits=c(0,2.5),breaks=seq(0,2.5,.5)) +
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,10) ) +
  scale_colour_brewer(type = 'div') +
  geom_smooth(aes(color=LoanStatus_mod),stat='summary', fun.y=mean)
# ------ Cases with bad loans have relatively higher inquiries within last 6 months


# Mean inquiries in past 6 months for different Loan Types
ggplot(aes(x=DebtToIncomeRatio ,y=InquiriesLast6Months), data = loan) +
  geom_smooth(aes(color=LoanStatus_mod),stat='summary', fun.y=mean) +
  scale_colour_brewer(type = 'div') 

ggplot(aes(x=.5*round(DebtToIncomeRatio/.5,1) ,y=InquiriesLast6Months), data = loan) +
  geom_smooth(aes(color=LoanStatus_mod),stat='summary', fun.y=mean) +
  scale_colour_brewer(type = 'div') 
# ------ Bad loans have higher mean number of inquiries regardless of their Debt to income Ratio







###---------------------------------------------------------------------
### Relationshop between Income Range and Loan Status
ggplot(aes(x=IncomeRange), data = loan) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod),position="fill") +
  scale_colour_brewer(type = 'div')

ggplot(aes(x=IncomeRange), data = subset(loan,IncomeVerifiable=='True')) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod)) +
  scale_colour_brewer(type = 'div')
# ------ At first glance, Bad loans do not seem to be affected by the income range



# Show the percentage of Income Range counts for each Loan Status
ggplot(aes(x=IncomeRange), data =  subset(loan,IncomeVerifiable=='True')) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod),position = "fill") +
  labs(y="Percentage counts")

# Exclude loans that are still current. 
ggplot(aes(x=IncomeRange), data = subset(loan,LoanStatus !='Current') ) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod),position = "fill") +
  labs(y="Percentage counts")

# ------  Loans with Lower income (or no income) have higher percentage of Bad Loans compared to those with 
#         higher income. However, there is not a huge percentage difference between Bad loans in lower income
#         group vs. higher income group


# Split for verifiable vs. non-verifieable income
ggplot(aes(x=IncomeRange), data = subset(loan,LoanStatus !='Current') ) +
  geom_bar(stat="count",aes(fill=LoanStatus_mod),position = "fill") +
  labs(y="Percentage counts") +
  facet_grid(.~IncomeVerifiable)

# ------ Cases where income is NOT verifiable seems to have same percentage of Bad Loans regardless of the stated income range.
#        This meanz they are possibly lying about their true income.




# Income Range and Debt to Income Ratio
ggplot(aes(x=IncomeRange ,y=DebtToIncomeRatio),data=loan ) +
  geom_jitter(alpha=1/10)
  #geom_smooth(method='lm',aes(color=LoanStatus_mod),stat='summary', fun.y=mean) +
  #scale_y_continuous(limits=c(0,50),breaks=seq(0,50,5) ) +
  scale_colour_brewer(type = 'div') 
# ------  AS expected, people with low income have much higher debt to income ratio, regardless of the Loan Status.
#         
#         Also, people with higher income seem to have higher concentration of 'Current' loans than the ones with lower income. Does this
#         mean that people with higher income are in debt more often that people with lower incomes?
  
  
  
# Income Range and Current Delinquencies
  ggplot(aes(x=IncomeRange ,y=CurrentDelinquencies), data = subset(loan,IncomeVerifiable=='True')) +
    geom_jitter(alpha=1/3,aes(color=LoanStatus_mod)) 
  
  
# Income Range and Current Delinquencies (remove Current Loan Status)
  ggplot(aes(x=IncomeRange ,y=CurrentDelinquencies), data = subset(loan,IncomeVerifiable=='True' & LoanStatus_mod != "Current")) +
    geom_jitter(alpha=1/3,aes(color=LoanStatus_mod)) 
  
# ------  # of current delinquencies does not seem to be any different for different Income ranges.
  
  
# Income Range and Amount of Current Delinquencies (remove Current Loan Status)
ggplot(aes(x=IncomeRange ,y=log(AmountDelinquent)), data = subset(loan,IncomeVerifiable=='True' & LoanStatus_mod != "Current" & AmountDelinquent>0)) +
  geom_jitter(alpha=1/3,aes(color=LoanStatus_mod)) 

# ------  # and Amount of current delinquencies does not seem to be any different for various Income ranges.

    




###---------------------------------------------------------------------
### Number of Current Delinquencies for each Loan Status
ggplot(aes(x=LoanStatus_mod ,y=CurrentDelinquencies), data = subset(loan,IncomeVerifiable=='True' & CurrentDelinquencies>0)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(0,5))

# ------ Upper IQR limit of Current number of Delinquencies for Bad Loan is much higher than other groups


###---------------------------------------------------------------------
### Amount of Current Delinquencies for each value of Loan Status
ggplot(aes(x=LoanStatus_mod ,y=log(AmountDelinquent)), data = subset(loan,IncomeVerifiable=='True' & AmountDelinquent>0)) +
  geom_boxplot() 

# ------ All Loan Status groups seem to have similar amounts of currently delinquent loans





summary(loan$ListingCategory..numeric.)


###=============================================================
### Pairwise comparisons of variables that (might) affect Loan Status 
ggpairs(data=  loan, 
       
        columns=c("BorrowerAPR","EstimatedReturn","EstimatedLoss", "AvailableBankcardCredit"),
        axisLabels= "show")
# ------ 



ggpairs(data=  loan, 
        mapping=aes(color = LoanStatus_mod,alpha=.05),
        columns=c("CreditScoreRangeUpper","CurrentCreditLines","OpenCreditLines")
)
# ------ Number of creit lines are very similar across all Loan Status values.
#        Very little correlation between ones credit score to the number of credit lines open



ggpairs(data=  loan, 
        mapping=aes(color = LoanStatus_mod,alpha=.05),
        columns=c("CreditScoreRangeUpper","CurrentCreditLines","OpenCreditLines")
)
# ------ Number of creit lines are very similar across all Loan Status values.
#        Very little correlation between ones credit score to the number of credit lines open





ggpairs(data=  loan, 
        
        columns=c("IsBorrowerHomeowner","Term","BankcardUtilization","AvailableBankcardCredit")
)
#------ Bad Loans seem to have higher monthly payments, although it is not conclusive

ggpairs(data=  loan, 
        mapping=aes(color = LoanStatus_mod,alpha=.05),
        columns=c("InquiriesLast6Months","CurrentDelinquencies","AmountDelinquent")
)
#------ Higher number of Inquiries seems to be associated with Bad Loans, and it also seems to be
#       related to higher number of current delinquencies

ggpairs(data=  loan, 
        mapping=aes(color = LoanStatus_mod,alpha=.05),
        columns=c("InquiriesLast6Months","CurrentDelinquencies","AmountDelinquent")
)
#------ Higher number of Inquiries seems to be associated with Bad Loans, and it also seems to be
#       related to higher number of current delinquencies

ggpairs(data=  loan, 
        
        columns=c("OnTimeProsperPayments","StatedMonthlyIncome","ProsperPrincipalBorrowed", "Recommendations")
)
        

# Study how Debt to income ratio affects 


        
        
        head(loan, 20)


ggpairs(data=  loan, 
        mapping=aes(color = LoanStatus_mod,alpha=.05),
        columns=c("ProsperRating..Alpha.","ListingCategory..numeric.")
) + theme(legend.position = "bottom")



+
  scale_colour_brewer()

table(loan$LoanStatus)

+
  scale_x_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 100)) 


        
# TO make the Loan Status more manageable, combine past due, Default and charge-off situations



