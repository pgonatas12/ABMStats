#Data obtained from http://hbiostat.org/data courtesy of the Vanderbilt University Department of Biostatistics.

#Had to edit some columns because of decimal loss when converting data to csv

abm_data<-read.csv("C:/Users/enrag/Documents/Mock Data Analytics/ABM Data/abm.csv",header=T)

print(abm_data, digits=15)

#ABM = 0 means Acute Viral Meningitis
#AMB = 1 means Acute Bacterial Meningitis


#Data will be imputed via mice package

library(mice)

abm_impute<-complete(mice(data=abm_data,method="pmm"))

#Race and Sex were not imputed

impute_bact<-abm_impute[abm_impute$abm==1,]
impute_vir<-abm_impute[abm_impute$abm==0,]




#Try individual variables

t.test(impute_bact$wbc,impute_vir$wbc)
#t = 7.9503, df = 483.09, p-value = 1.326e-14
#T-test suggests that wbc levels are different between viral and bacterial

t.test(impute_bact$bloodgl,impute_vir$bloodgl)
#t = 6.8461, df = 489.24, p-value = 2.282e-11
#Significant difference between blood glucose

t.test(impute_bact$pmn,impute_vir$pmn)
#t = -3.7464, df = 554.37, p-value = 0.0001981
#PMN levels are different

t.test(impute_bact$gl,impute_vir$gl)
#t = -10.388, df = 550.87, p-value < 2.2e-16

t.test(impute_bact$reds,impute_vir$reds)
#t = 1.9184, df = 547.39, p-value = 0.05559
#No significant difference in reds at 5% confidence

t.test(impute_bact$whites,impute_vir$whites)
#t = 6.3891, df = 310.5, p-value = 6.098e-10

#Check normality and variance of each variable used

shapiro.test(abm_impute$wbc)
#W = 0.81504, p-value < 2.2e-16

var.test(impute_bact$wbc,impute_vir$wbc)
#F = 5.8412, num df = 295, denom df = 284, p-value < 2.2e-16

#WBC Does not follow a normal dist and has unequal variance. T-test does not hold


shapiro.test(abm_impute$bloodgl)
#W = 0.83018, p-value < 2.2e-16

var.test(impute_bact$bloodgl,impute_vir$bloodgl)
#F = 2.6778, num df = 295, denom df = 284, p-value = 2.22e-16

#T-test invalid for bloodgl


shapiro.test(abm_impute$pmn)
#W = 0.94653, p-value = 1.233e-13

var.test(impute_bact$pmn,impute_vir$pmn)
#F = 1.51, num df = 295, denom df = 284, p-value = 0.0004928

#T-test invalid for pmn


shapiro.test(abm_impute$gl)
#W = 0.95198, p-value = 8.296e-13

var.test(impute_bact$gl,impute_vir$gl)
#F = 2.195, num df = 295, denom df = 284, p-value = 4.651e-11

#Reject t-test for gl


shapiro.test(abm_impute$whites)
#W = 0.31768, p-value < 2.2e-16

var.test(impute_bact$whites,impute_vir$whites)
#F = 15.01, num df = 295, denom df = 284, p-value < 2.2e-16

#Data doesn't follow normal dist, so let's try a non-parametric method

wilcox.test(impute_bact$wbc,impute_vir$wbc)
#W = 60400, p-value < 2.2e-16

wilcox.test(impute_bact$bloodgl,impute_vir$bloodgl)
#W = 58132, p-value = 3.248e-15

wilcox.test(impute_bact$pmn,impute_vir$pmn)
#W = 36553, p-value = 0.005291

wilcox.test(impute_bact$gl,impute_vir$gl)
#W = 21742, p-value < 2.2e-16

wilcox.test(impute_bact$reds,impute_vir$reds)
#W = 52510, p-value = 3.306e-07

wilcox.test(impute_bact$whites,impute_vir$whites)
#W = 67030, p-value < 2.2e-16

#While t-test doesn't fit here, Wilcox Signed Rank Test gives us similar results.
#There is significant evidence that Bacterial Meng has different effects than viral infections





#Now let's try with removing nulls from the data



