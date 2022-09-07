###### Time-at-surface analysis for marine turtles in the Gulf of Mexico ####

### required packages 
# readxl
# ggplot2
# dplyr
# tidyr
# hms
# lubridate
# MASS
# nlme
# mgcv
# GGally
# caTools
# ggsci
# Metrics 
# viridis          

options(stringsAsFactors = FALSE)

# Load data - data release associated with manuscript 

# Run a correlation test on covariates from data 
ggcorr(corr.test, label = TRUE, label_alpha = TRUE)

# Code the factor variables 
dat$month <- as.factor(dat$month)
dat$GOM <- as.factor(dat$GOM)
dat$id <- as.factor(dat$id)
dat$season <- as.factor(dat$season)

# Check columns for NAs 
sapply(dat, function(x) sum(is.nan(x)))

# Create a unique ID column in the data 
dat$UNQ_ID <- paste(dat$id, dat$FID_, sep = "_")
colnames(dat)

# Separate out species models 
CC_dat <- dat %>% 
  filter(Species == "CC")
LK_dat <- dat %>% 
  filter(Species == "LK")
CM_dat <- dat %>% 
  filter(Species=="CM")

######## LOGGERHEAD MODEL ########
# Below code for the CC model 
N=length(CC_dat[,1]) # 6058
N
CC_dat$group=-99*array(1,N)
for (i in 1:N){
  if (CC_dat$id[i]==100391){CC_dat$group[i]= 1}
  if (CC_dat$id[i]==100392){CC_dat$group[i]= 2}
  if (CC_dat$id[i]==100393){CC_dat$group[i]= 3}
  if (CC_dat$id[i]==100394){CC_dat$group[i]= 4}
  if (CC_dat$id[i]==100395){CC_dat$group[i]= 5}
  if (CC_dat$id[i]==100396){CC_dat$group[i]= 6}
  if (CC_dat$id[i]==100397){CC_dat$group[i]= 7}
  if (CC_dat$id[i]==100398){CC_dat$group[i]= 8}
  if (CC_dat$id[i]==100399){CC_dat$group[i]= 9}
  if (CC_dat$id[i]==100596){CC_dat$group[i]= 10}
  if (CC_dat$id[i]==100597){CC_dat$group[i]= 11}
  if (CC_dat$id[i]==100598){CC_dat$group[i]= 12}
  if (CC_dat$id[i]==100600){CC_dat$group[i]= 13}
  if (CC_dat$id[i]==100601){CC_dat$group[i]= 14}
  if (CC_dat$id[i]==100604){CC_dat$group[i]= 15}
  if (CC_dat$id[i]==100605){CC_dat$group[i]= 16}
  if (CC_dat$id[i]==100608){CC_dat$group[i]= 17}
  if (CC_dat$id[i]==100609){CC_dat$group[i]= 18}
  if (CC_dat$id[i]==100611){CC_dat$group[i]= 19}
  if (CC_dat$id[i]==100613){CC_dat$group[i]= 20}
  if (CC_dat$id[i]==100614){CC_dat$group[i]= 21}
  if (CC_dat$id[i]==106338){CC_dat$group[i]= 22}
  if (CC_dat$id[i]==106339){CC_dat$group[i]= 23}
  if (CC_dat$id[i]==106340){CC_dat$group[i]= 24}
  if (CC_dat$id[i]==106341){CC_dat$group[i]= 25}
  if (CC_dat$id[i]==106342){CC_dat$group[i]= 26}
  if (CC_dat$id[i]==106343){CC_dat$group[i]= 27}
  if (CC_dat$id[i]==106344){CC_dat$group[i]= 28}
  if (CC_dat$id[i]==106346){CC_dat$group[i]= 29}
  if (CC_dat$id[i]==106347){CC_dat$group[i]= 30}
  if (CC_dat$id[i]==106349){CC_dat$group[i]= 31}
  if (CC_dat$id[i]==106350){CC_dat$group[i]= 32}
  if (CC_dat$id[i]==106354){CC_dat$group[i]= 33}
  if (CC_dat$id[i]==106355){CC_dat$group[i]= 34}
  if (CC_dat$id[i]==106356){CC_dat$group[i]= 35}
  if (CC_dat$id[i]==106357){CC_dat$group[i]= 36}
  if (CC_dat$id[i]==106363){CC_dat$group[i]= 37}
  if (CC_dat$id[i]==106364){CC_dat$group[i]= 38}
  if (CC_dat$id[i]==106365){CC_dat$group[i]= 39}
  if (CC_dat$id[i]==110809){CC_dat$group[i]= 40}
  if (CC_dat$id[i]==110810){CC_dat$group[i]= 41}
  if (CC_dat$id[i]==110811){CC_dat$group[i]= 42}
  if (CC_dat$id[i]==117512){CC_dat$group[i]= 43}
  if (CC_dat$id[i]==117513){CC_dat$group[i]= 44}
  if (CC_dat$id[i]==117514){CC_dat$group[i]= 45}
  if (CC_dat$id[i]==117515){CC_dat$group[i]= 46}
  if (CC_dat$id[i]==117516){CC_dat$group[i]= 47}
  if (CC_dat$id[i]==119943){CC_dat$group[i]= 48}
  if (CC_dat$id[i]==119944){CC_dat$group[i]= 49}
  if (CC_dat$id[i]==119945){CC_dat$group[i]= 50}
  if (CC_dat$id[i]==119946){CC_dat$group[i]= 51}
  if (CC_dat$id[i]==119947){CC_dat$group[i]= 52}
  if (CC_dat$id[i]==119948){CC_dat$group[i]= 53}
  if (CC_dat$id[i]==119949){CC_dat$group[i]= 54}
  if (CC_dat$id[i]==119950){CC_dat$group[i]= 55}
  if (CC_dat$id[i]==119951){CC_dat$group[i]= 56}
  if (CC_dat$id[i]==119952){CC_dat$group[i]= 57}
  if (CC_dat$id[i]==122185){CC_dat$group[i]= 58}
  if (CC_dat$id[i]==122187){CC_dat$group[i]= 59}
  if (CC_dat$id[i]==128356){CC_dat$group[i]= 60}
  if (CC_dat$id[i]==128359){CC_dat$group[i]= 61}
  if (CC_dat$id[i]==128363){CC_dat$group[i]= 62}
  if (CC_dat$id[i]==128374){CC_dat$group[i]= 63}
  if (CC_dat$id[i]==128375){CC_dat$group[i]= 64}
  if (CC_dat$id[i]==129506){CC_dat$group[i]= 65}
  if (CC_dat$id[i]==129507){CC_dat$group[i]= 66}
  if (CC_dat$id[i]==129508){CC_dat$group[i]= 67}
  if (CC_dat$id[i]==129509){CC_dat$group[i]= 68}
  if (CC_dat$id[i]==129510){CC_dat$group[i]= 69}
  if (CC_dat$id[i]==129511){CC_dat$group[i]= 70}
  if (CC_dat$id[i]==129512){CC_dat$group[i]= 71}
  if (CC_dat$id[i]==129513){CC_dat$group[i]= 72}
  if (CC_dat$id[i]==129514){CC_dat$group[i]= 73}
  if (CC_dat$id[i]==129515){CC_dat$group[i]= 74}
  if (CC_dat$id[i]==132009){CC_dat$group[i]= 75}
  if (CC_dat$id[i]==154833){CC_dat$group[i]= 76}
  if (CC_dat$id[i]==154834){CC_dat$group[i]= 77}
  if (CC_dat$id[i]==154835){CC_dat$group[i]= 78}
  if (CC_dat$id[i]==154836){CC_dat$group[i]= 79}
  if (CC_dat$id[i]==154837){CC_dat$group[i]= 80}
  if (CC_dat$id[i]==154838){CC_dat$group[i]= 81}
  if (CC_dat$id[i]==154839){CC_dat$group[i]= 82}
  if (CC_dat$id[i]==154840){CC_dat$group[i]= 83}
  if (CC_dat$id[i]==154841){CC_dat$group[i]= 84}
  if (CC_dat$id[i]==154842){CC_dat$group[i]= 85}
  if (CC_dat$id[i]==154843){CC_dat$group[i]= 86}
  if (CC_dat$id[i]==154844){CC_dat$group[i]= 87}
  if (CC_dat$id[i]==154845){CC_dat$group[i]= 88}
  if (CC_dat$id[i]==154846){CC_dat$group[i]= 89}
  if (CC_dat$id[i]==154847){CC_dat$group[i]= 90}
  if (CC_dat$id[i]==161455){CC_dat$group[i]= 91}
  if (CC_dat$id[i]==161456){CC_dat$group[i]= 92}
  if (CC_dat$id[i]==161457){CC_dat$group[i]= 93}
  if (CC_dat$id[i]==161458){CC_dat$group[i]= 94}
  if (CC_dat$id[i]==161459){CC_dat$group[i]= 95}
  if (CC_dat$id[i]==161460){CC_dat$group[i]= 96}
  if (CC_dat$id[i]==161461){CC_dat$group[i]= 97}
  if (CC_dat$id[i]==161462){CC_dat$group[i]= 98}
  if (CC_dat$id[i]==161463){CC_dat$group[i]= 99}
  if (CC_dat$id[i]==161464){CC_dat$group[i]= 100}
  if (CC_dat$id[i]==161465){CC_dat$group[i]= 101}
  if (CC_dat$id[i]==161466){CC_dat$group[i]= 102}
  if (CC_dat$id[i]==161467){CC_dat$group[i]= 103}
  if (CC_dat$id[i]==161468){CC_dat$group[i]= 104}
  if (CC_dat$id[i]==171515){CC_dat$group[i]= 105}
  if (CC_dat$id[i]==171516){CC_dat$group[i]= 106}
  if (CC_dat$id[i]==171517){CC_dat$group[i]= 107}
  if (CC_dat$id[i]==171518){CC_dat$group[i]= 108}
  if (CC_dat$id[i]==171519){CC_dat$group[i]= 109}
  if (CC_dat$id[i]==171520){CC_dat$group[i]= 110}
  if (CC_dat$id[i]==172666){CC_dat$group[i]= 111}
  if (CC_dat$id[i]==172667){CC_dat$group[i]= 112}
  if (CC_dat$id[i]==172668){CC_dat$group[i]= 113}
  if (CC_dat$id[i]==172670){CC_dat$group[i]= 114}
  if (CC_dat$id[i]==172671){CC_dat$group[i]= 115}
  if (CC_dat$id[i]==172672){CC_dat$group[i]= 116}
  if (CC_dat$id[i]==172673){CC_dat$group[i]= 117}
  if (CC_dat$id[i]==172674){CC_dat$group[i]= 118}
  if (CC_dat$id[i]==175685){CC_dat$group[i]= 119}
  if (CC_dat$id[i]==175686){CC_dat$group[i]= 120}
  if (CC_dat$id[i]==175687){CC_dat$group[i]= 121}
  if (CC_dat$id[i]==175688){CC_dat$group[i]= 122}
  if (CC_dat$id[i]==175689){CC_dat$group[i]= 123}
  if (CC_dat$id[i]==175690){CC_dat$group[i]= 124}
  if (CC_dat$id[i]==175693){CC_dat$group[i]= 125}
  if (CC_dat$id[i]==175694){CC_dat$group[i]= 126}
  if (CC_dat$id[i]==175695){CC_dat$group[i]= 127}
  if (CC_dat$id[i]==175696){CC_dat$group[i]= 128}
  if (CC_dat$id[i]==175697){CC_dat$group[i]= 129}
  if (CC_dat$id[i]==175698){CC_dat$group[i]= 130}
  if (CC_dat$id[i]==175699){CC_dat$group[i]= 131}
  if (CC_dat$id[i]==176008){CC_dat$group[i]= 132}
}
levels(CC_dat$id)  # 126 turtles

#  [1] "100391" "100392" "100393" "100394" "100395" "100396" "100397" "100398" "100399" "100597" "100598" "100600" "100601" "100604" "100605" "100608" "100609" "100611" "100613" "100614"
# [21] "106338" "106339" "106340" "106341" "106342" "106343" "106344" "106346" "106347" "106349" "106350" "106354" "106355" "106356" "106357" "106363" "106364" "106365" "110811" "117512"
# [41] "117513" "117514" "117515" "117516" "119943" "119944" "119945" "119946" "119947" "119948" "119949" "119950" "119951" "119952" "122185" "122187" "128356" "128359" "128374" "128375"
# [61] "129506" "129507" "129508" "129509" "129510" "129511" "129512" "129513" "129514" "129515" "132009" "154833" "154834" "154835" "154836" "154837" "154838" "154839" "154840" "154841"
# [81] "154842" "154843" "154844" "154845" "154846" "154847" "161455" "161456" "161457" "161458" "161459" "161460" "161461" "161462" "161463" "161464" "161466" "161467" "161468" "171515"
#[101] "171516" "171517" "171518" "171519" "171520" "172666" "172667" "172668" "172670" "172672" "172673" "172674" "175685" "175686" "175687" "175688" "175689" "175690" "175693" "175694"
#[121] "175695" "175696" "175697" "175698" "175699" "176008"


# Split into training and testing - CC model.  
final_train=CC_dat[(CC_dat$group<86),]  
NCC=length(final_train[,1])
NCC
levels(factor(final_train$id))
# 41 complete track turtles, 70.9% of the data.  

final_test=CC_dat[(CC_dat$group>85),]  
NCC2=length(final_test[,1])
NCC2
levels(factor(final_test$id))
# 17 complete track turtles, 29.1% of the data. Note 41/58 = 70.7% of whole turtles.   

NCC  # your sample size for CC
for (i in 1:NCC){
  final_train$ptop2T[i] <- ( (final_train$top2T[i]/100)*(NCC-1) +0.5 ) / NCC}   
  # divide by 100 to get response variable between 0 and 1

NCC=length(CC_dat[,1])
NCC
for (i in 1:NCC){
  CC_dat$ptop2T[i] <- ( (CC_dat$top2T[i]/100)*(NCC-1) +0.5 ) / NCC}   
  # divide by 100 to get response variable between 0 and 1
str(CC_dat)

#Run GAM (all possible covariates listed)
mod <- gam(ptop2T ~  season + GOM +
             s(sst, k=5) + 
             s(anom, k=5) +
             s(DIST_shelf, k=5) +
             s(DIST_shore, k=5) +
             s(logdepth, k=5) + 
             s(logsalin, k=5) + 
             s(current_abs, k=5) +
             s(current_dir, k=5) +
             s(ssh, k=5) +
             s(CI.FGM, k=5) +
             s(SST.FGM, k=5) +
             s(lat_utm,lon_utm) + #spatial smoother
             s(id, bs="re"),      #ID as random effect 
           family=betar(link = 'logit'), data=final_train, method = "REML")
summary(mod)  
AIC(mod)  # models sorted by AIC score; lowest value chosen as best model
gam.check(mod, pages=1)
par(mar=c(4,4,2,2))
plot(mod, rug=T, scale=0, shade=T, scheme = T, pages = 1) 
pacf(resid(mod), lag.max = 36, main = "pACF")

###### Further check to ensure that we can predict new turtles ######
#http://127.0.0.1:20975/library/stats/html/Logistic.html
logit_pred1 <-predict.gam(mod, final_train, exclude = "s(id)")
pred1 <- plogis(logit_pred1)

NCC2=length(final_test[,1])
NCC2  # your sample size for CC
for (i in 1:NCC2){
  final_test$ptop2T[i] <- ( (final_test$top2T[i]/100)*(NCC2-1) +0.5 ) / NCC2}   # divide   by 100 to get between 0 and 1

logit_pred2 <- predict.gam(mod,final_test, exclude = "s(id)")
pred2 <- plogis(logit_pred2)

y1_yp2= ( final_train$ptop2T -  pred1)^2
y1_me2 = (  final_train$ptop2T - mean(final_train$ptop2T) )^2
Rsq_CC_top2T = 1 - sum(y1_yp2)/sum(y1_me2)
Rsq_CC_top2T

CCfinal_test=cbind(final_test$ptop2T,pred2)

y2_yp2= ( final_test$ptop2T -  pred2)^2
y2_me2 = ( final_test$ptop2T - mean( final_test$ptop2T) )^2
nRsq_CC_top2T = 1 - sum(y2_yp2)/sum(y2_me2)
nRsq_CC_top2T
####### end further checks ######

# Get the predictions and SE for the final_test data 
preds <- predict.gam(mod, final_test, type = "response", se.fit = TRUE, 
                     na.action = "na.omit", exclude = "s(id)")
range(preds)
format((range(preds)), scientific=FALSE)
dive_preds <- as.vector(preds$fit)
dive_SE <- as.vector(preds$se.fit)
format((summary(dive_preds)), scientific=FALSE)
# make a dataframe with the predictions, SE, and lat/long for mapping


##### all above code replicated for CM and LK species group 


