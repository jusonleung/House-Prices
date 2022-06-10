# Version: 0.0.1
# 

library(rstudioapi)             # Set relative path
library(plyr)                   # Data manipulation
library(dplyr)                  # Data manipulation
library(corrplot)               # Data visualization
library(ggplot2)                # Data visualization
library(lares)                  # Correlation and model visualization (can be used to rank correlation of one variable to all others)
                                # May require running "install.packages('remotes')" and "remotes::install_github("laresbernardo/lares")"
library(DataExplorer)           # Auto exploratory data analysis
library(xgboost)                # Gradient boosting predictor
library(caret)                  # One-hot encoding factor variables and hypertune paramemters
library(randomForest)           # Rnd Forest


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Read in data as dataframe
train_raw = read.csv("./data/train.csv", stringsAsFactors = FALSE)
test_raw = read.csv("./data/test.csv", stringsAsFactors = FALSE)


# Combine data for easier manipulation
combined_raw = bind_rows(train_raw, test_raw)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Key Stats~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

png("graphs/train_intro.png", width = 350, height = 200)
train_intro_graph = plot_intro(train_raw)
dev.off()

any_na = function(x) any(is.na(x))
train_any_na_col = select_if(train_raw, any_na)
png("graphs/train_missing.png", width = 350, height = 400)
train_missing_graph = plot_missing(train_any_na_col)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data treatment~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_imputed = combined_raw

SPLogDiff = select(combined_imputed, SalePrice)
combined_imputed$SalePrice[is.na(combined_imputed$SalePrice) == 0] = log(combined_imputed$SalePrice[is.na(combined_imputed$SalePrice) == 0])
SPLogDiff$LogSP = combined_imputed$SalePrice


png("graphs/SPLogDiff.png", width = 350, height = 150)
SPLogDiff_graph = plot_qq(SPLogDiff)
dev.off()


#Check and fixing data error/inconsistent
#check which variables contain missing values
sort(colSums(sapply(combined_imputed[which(colSums(is.na(combined_imputed)) > 0)], is.na)), decreasing = TRUE)

#Pool variables
#Check consistency of PoolQC and PoolArea
combined_imputed[combined_imputed$PoolArea>0 & is.na(combined_imputed$PoolQC), c('PoolArea', 'PoolQC','OverallQual')]
#Replace their PoolQC related to its OverallQual
combined_imputed$PoolQC[2421] <- 2
combined_imputed$PoolQC[2504] <- 3
combined_imputed$PoolQC[2600] <- 2



#Garage variables
allGarageVar <- c('GarageType', 'GarageYrBlt', 'GarageFinish','GarageQual','GarageCond','GarageCars','GarageArea')

#There are 157 NAs in GarageType, NA means no garage
#However, there are 159 NAs in GarageYrBlt, GarageFinish, GarageQual and GarageCond, there is inconsistency
#Check those 157 NAs of GarageType are also have NAs in GarageYrBlt, GarageFinish, GarageQual and GarageCond
length(which(is.na(combined_imputed$GarageType)& is.na(combined_imputed$GarageYrBlt) & is.na(combined_imputed$GarageFinish) & is.na(combined_imputed$GarageQual) & is.na(combined_imputed$GarageCond)))

#Check which rows are inconsistent
combined_imputed[is.na(combined_imputed$GarageYrBlt) & !is.na(combined_imputed$GarageType), allGarageVar]
#We also found that both NA of GarageCars and GarageArea is in row 2577

#Fixing row 2127
#Check relation between YearBuilt and GarageYrBlt
#ggplot(combined_imputed[!is.na(combined_imputed$GarageYrBlt),], aes(x=YearBuilt, y=GarageYrBlt)) + geom_point(size=1)
#We also found that there is a row that its GarageYrBlt value is over 2200, it doesn't make sense, we will fix it later
length(which(combined_imputed$GarageYrBlt == combined_imputed$YearBuilt))
#we see there are 2216 out of 2919 rows that their YearBuilt is equal to GarageYrBlt
#Fixing row 2127
#Replace row 2127's GarageYrBlt with its YearBuilt
combined_imputed$GarageYrBlt[2127] <- combined_imputed$YearBuilt[2127]
#Replace row 2127's GarageFinish, GarageQual and GarageCond with mode of all data
combined_imputed$GarageFinish[2127] <- names(sort(-table(combined_imputed$GarageFinish)))[1]
combined_imputed$GarageQual[2127] <- names(sort(-table(combined_imputed$GarageQual)))[1]
combined_imputed$GarageCond[2127] <- names(sort(-table(combined_imputed$GarageCond)))[1]
combined_imputed[2127,c(allGarageVar,'YearBuilt')]

#Fixing row 2577
combined_imputed[2577,allGarageVar]
#In this row, all variable related to garage are NA except GarageType
#It's seems like this house have no garage and there is an error on its GarageType value, so we treat it has no garage
combined_imputed$GarageType[2577] <- NA
combined_imputed$GarageCars[2577] <- 0
combined_imputed$GarageArea[2577] <- 0
combined_imputed[2577,allGarageVar]

#We have found an error in GarageYrBlt before
combined_imputed[which(combined_imputed$GarageYrBlt>2021),c('GarageYrBlt','YearBuilt','YearRemodAdd')]
#We think there is a typo here, the GarageYrBlt should be 2007
combined_imputed$GarageYrBlt[2593] <- 2007


#Basement variables

allBsmtVar <- c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')
#Revisit the numbers of NAs of all Basement related variables
colSums(sapply(combined_imputed[allBsmtVar], is.na))
#Check if 79 rows that NA in BsmtFinType1 are also NAs in BsmtQual, BsmtCond, BsmtExposure and BsmtFinType2
length(which(is.na(combined_imputed$BsmtFinType1) & is.na(combined_imputed$BsmtQual) & is.na(combined_imputed$BsmtCond) & is.na(combined_imputed$BsmtExposure) & is.na(combined_imputed$BsmtFinType2)))
#Check those rows with not NA in BsmtFinType1 but have NA in either BsmtQual, BsmtCond, BsmtExposure or BsmtFinType2
combined_imputed[!is.na(combined_imputed$BsmtFinType1) &( is.na(combined_imputed$BsmtQual) | is.na(combined_imputed$BsmtCond) | is.na(combined_imputed$BsmtExposure) | is.na(combined_imputed$BsmtFinType2)),allBsmtVar]

#Fixing rows 333 by replace its BsmtFinType2 with mode of all data
table(combined_imputed$BsmtFinType2)
combined_imputed$BsmtFinType2[333] <- 'Unf'

#Fixing rows 949, 1488 and 2349 by replace its BsmtExposure with mode of all data
combined_imputed$BsmtExposure[c(949,1488,2349)] <- names(sort(-table(combined_imputed$BsmtExposure)))[1]

#Fixing rows 2218 and 2219 by replace its BsmtQual with mode of all data
combined_imputed$BsmtQual[c(2218,2219)] <- names(sort(-table(combined_imputed$BsmtQual)))[1]

#Fixing rows 2041, 2186 and 2525 by replace its BsmtCond with mode of all data
combined_imputed$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(combined_imputed$BsmtCond)))[1]

#Check which rows have NA in either BsmtFinSF1, BsmtFinSF2, BsmtUnfSF ,TotalBsmtSF, BsmtFullBath, or BsmtHalfBath
combined_imputed[is.na(combined_imputed$BsmtFinSF1) | is.na(combined_imputed$BsmtFinSF2) | is.na(combined_imputed$BsmtUnfSF) | is.na(combined_imputed$TotalBsmtSF)| is.na(combined_imputed$BsmtFullBath)| is.na(combined_imputed$BsmtHalfBath),allBsmtVar]
#There are two row, and all basement variables on this row is NA or 0, we treat them has no basement
combined_imputed[c(2121,2189),c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')] <- 0
colSums(sapply(combined_imputed[allBsmtVar], is.na))

#Masonry variables
combined_imputed[is.na(combined_imputed$MasVnrType)|is.na(combined_imputed$MasVnrArea),c('MasVnrType','MasVnrArea')]
#row 2611 have NA in MasVnrType but not NA in MasVnrArea
#Replace it MasVnrType with mode of all data(out of 'None')
table(combined_imputed$MasVnrType)
combined_imputed$MasVnrType[2611] <- 'BrkFace'


# Transform meaningful NA values into processable feature data
combined_imputed$PoolQC[combined_imputed$PoolArea == 0 & is.na(combined_raw$PoolQC) == 1] = "None"
combined_imputed$MiscFeature[is.na(combined_imputed$MiscFeature) == 1] = "None"
combined_imputed$Alley[is.na(combined_imputed$Alley) == 1] = "None"
combined_imputed$Fence[is.na(combined_imputed$Fence) == 1] = "None"
combined_imputed$GarageType[is.na(combined_imputed$GarageType) == 1] = "None"
combined_imputed$GarageFinish[is.na(combined_imputed$GarageFinish) == 1] = "None"
combined_imputed$GarageQual[is.na(combined_imputed$GarageQual) == 1] = "None"
combined_imputed$GarageCond[is.na(combined_imputed$GarageCond) == 1] = "None"
combined_imputed$GarageYrBlt[is.na(combined_imputed$GarageYrBlt) == 1] = combined_imputed$YearBuilt[is.na(combined_imputed$GarageYrBlt) == 1]
combined_imputed$BsmtCond[is.na(combined_imputed$BsmtCond) == 1] = "None"
combined_imputed$BsmtQual[is.na(combined_imputed$BsmtQual) == 1] = "None"
combined_imputed$BsmtExposure[is.na(combined_imputed$BsmtExposure) == 1] = "None"                         #Basement NAs are not equal, more investigation needed
combined_imputed$BsmtFinType1[is.na(combined_imputed$BsmtFinType1) == 1] = "None"
combined_imputed$BsmtFinType2[is.na(combined_imputed$BsmtFinType2) == 1] = "None"
combined_imputed$BsmtFullBath[is.na(combined_imputed$BsmtFullBath) == 1] = 0
combined_imputed$BsmtHalfBath[is.na(combined_imputed$BsmtHalfBath) == 1] = 0
#combined_imputed$PoolQC[is.na(combined_imputed$PoolQC) == 1] = "None"
combined_imputed$FireplaceQu[combined_imputed$Fireplaces == 0 & is.na(combined_imputed$FireplaceQu) == 1] = "None"


zone_mode_func = function(x)
{
  mode_tbl = table(x$MSZoning)
  x$ZoneMode = rep(names(mode_tbl)[which.max(mode_tbl)], nrow(x))
  x
}
combined_imputed = ddply(combined_imputed, .(Neighborhood), .fun=zone_mode_func)
combined_imputed$MSZoning[is.na(combined_imputed$MSZoning) == 1] = combined_imputed$ZoneMode[is.na(combined_imputed$MSZoning) == 1]

count(combined_imputed, 'MasVnrType')
combined_imputed$MasVnrType[is.na(combined_imputed$MasVnrType) == 1 & combined_imputed$MasVnrArea > 0] = 'BrkFace'

combined_imputed$MasVnrArea[is.na(combined_imputed$MasVnrType) == 1] = 0
combined_imputed$MasVnrType[is.na(combined_imputed$MasVnrType) == 1] = "None"

#filter(combined_imputed, is.na(Functional) == 1)

func_mode_func = function(x)
{
  mode_tbl = table(x$Functional)
  x$FuncMode = rep(names(mode_tbl)[which.max(mode_tbl)], nrow(x))
  x
}
combined_imputed = ddply(combined_imputed, .(OverallCond), .fun = func_mode_func)
combined_imputed$Functional[is.na(combined_imputed$Functional) == 1] = combined_imputed$FuncMode[is.na(combined_imputed$Functional) == 1]

#filter(combined_imputed, RoofMatl == 'Tar&Grv')

ext1_mode_func = function(x)
{
  mode1_tbl = table(x$Exterior1st)
  x$Ext1Mode = rep(names(mode1_tbl)[which.max(mode1_tbl)], nrow(x))
  x
}
combined_imputed = ddply(combined_imputed, .(RoofMatl), .fun = ext1_mode_func)
combined_imputed$Exterior1st[is.na(combined_imputed$Exterior1st) == 1] = combined_imputed$Ext1Mode[is.na(combined_imputed$Exterior1st) == 1]

ext2_mode_func = function(x)
{
  mode2_tbl = table(x$Exterior2nd)
  x$Ext2Mode = rep(names(mode2_tbl)[which.max(mode2_tbl)], nrow(x))
  x
}
combined_imputed = ddply(combined_imputed, .(RoofMatl), .fun = ext2_mode_func)
combined_imputed$Exterior2nd[is.na(combined_imputed$Exterior2nd) == 1] = combined_imputed$Ext2Mode[is.na(combined_imputed$Exterior2nd) == 1]

#filter(combined_imputed, is.na(BsmtFinSF1) == 1)
#combined_imputed$BsmtFinSF1[combined_imputed$BsmtFinType1 == "None"] = 0
#combined_imputed$BsmtFinSF2[combined_imputed$BsmtFinType2 == "None"] = 0
#combined_imputed$BsmtUnfSF[is.na(combined_imputed$BsmtUnfSF) == 1] = 0
#combined_imputed$TotalBsmtSF[is.na(combined_imputed$TotalBsmtSF) == 1] = 0

#filter(combined_imputed, is.na(Electrical) == 1)

combined_imputed$Electrical[is.na(combined_imputed$Electrical)] = names(sort(-table(combined_imputed$Electrical)))[1]
combined_imputed$KitchenQual[is.na(combined_imputed$KitchenQual)] = names(sort(-table(combined_imputed$KitchenQual)))[1]

#filter(combined_imputed, is.na(GarageCars) == 1)

#combined_imputed$GarageCars[is.na(combined_imputed$GarageCars) == 1] = 0
#combined_imputed$GarageArea[is.na(combined_imputed$GarageArea) == 1] = 0

#filter(combined_imputed, is.na(SaleType) == 1)
#filter(combined_imputed, SaleType == 'New')
combined_imputed$YrHeld = ifelse(combined_imputed$YrSold <= combined_imputed$YearBuilt,0,combined_imputed$YrSold - combined_imputed$YearBuilt)
#combined_imputed$YrHeld = combined_imputed$YrSold - combined_imputed$YearBuilt
#filter(combined_imputed, YrHeld <= 1 & SaleType == 'New')
combined_imputed$SaleType[is.na(combined_imputed$SaleType)] = names(sort(-table(combined_imputed$SaleType)))[1]

for (i in 1:nrow(combined_imputed)){
  if(is.na(combined_imputed$LotFrontage[i])){
    combined_imputed$LotFrontage[i] = as.integer(median(combined_imputed$LotFrontage[combined_imputed$Neighborhood==combined_imputed$Neighborhood[i]], na.rm=TRUE))
  }
}


# Dropping useless features
combined_imputed = subset(combined_imputed, select = -c(Utilities, ZoneMode, FuncMode, Ext1Mode, Ext2Mode))



# Generate chart of missing values
#missing_sum = sapply(combined_imputed, function(x) sum(is.na(x) | x == ""))
#missing_df = data.frame(missing_sum)

combined_factored = mutate_if(combined_imputed, is.character, as.factor)
combined_factored = mutate_at(combined_factored, vars(MSSubClass, MoSold, YrSold), as.factor)
#str(combined_factored)


plot_density(combined_factored)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Observation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

png("graphs/SalePrice_corr.png", width = 350, height = 400)
corr_var(combined_factored, SalePrice, top = 20)
dev.off()


png("graphs/FullSalePrice_corr.png", width = 350, height = 2000)
corr_var(combined_factored, SalePrice, top = 260)
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Regression Implementation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Linear regression model building based on numeric attributes

# Replace ranking attributes like Quality and Condition with numbers
ranking_list = c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")
trans_table = c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

# Function to convert ranking attributes to numbers
rank2value = function(df, rank_list, trans_tbl) {
  for (rank_var in rank_list) {
    df[rank_var] = as.vector(trans_tbl[df[,rank_var]])
  }
  return(df)
}
combined_factored = rank2value(combined_factored, ranking_list, trans_table)

# Separate processed train and test data for model building
train_factored = subset(combined_factored,combined_factored$Id <= 1460)
test_factored = subset(combined_factored,combined_factored$Id > 1460)

# Display the correlation coefficient to select variables
train_num_var = train_factored[names(which(sapply(train_factored, is.numeric)))]
num_cor = cor(train_num_var)
cor_head = names(which(abs(sort(num_cor["SalePrice",], decreasing = TRUE)) > 0.5))
corrplot(num_cor[cor_head, cor_head], type = "upper", method = "color",
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.6)

# Attributes around ¡À0.6 cor are selected

# GrLivArea and TotRmsAbvGrd, GarageCars and GarageArea are highly correlated,
# Therefore only one of each is selected
linear_model = lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + ExterQual
                  + KitchenQual + BsmtQual + X1stFlrSF , data = train_num_var)

LmPrediction = predict(linear_model, test_factored)

## Addition of categorical values

train_factor_var = train_factored[names(which(sapply(train_factored, is.factor)))]
train_factor_var$SalePrice = train_factored$SalePrice

factor_model = lm(SalePrice~ ., data = train_factor_var)

#summary(factor_model) to see which predictors are more significant

factor_model = lm(SalePrice ~ MSZoning + LotConfig + Neighborhood
                  + HouseStyle + RoofMatl + BsmtExposure + BsmtFinType1
                  + CentralAir  + GarageFinish, data = train_factor_var)

FmPrediction = predict(factor_model, test_factored)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Alternate algorithms~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Random Forest
RForest_model = randomForest(SalePrice~., data = train_factored)
varImpPlot(RForest_model)

RFPrediction = predict(RForest_model, test_factored)

# One hot encoding for tranforming factors into boolean variables
dummy = dummyVars(" ~ .", data = combined_factored)
combined_1he = data.frame(predict(dummy, newdata = combined_factored))
train_1he = subset(combined_1he, is.na(SalePrice) == 0)
test_1he = subset(combined_1he, is.na(SalePrice) == 1)

set.seed(1)
control = trainControl(method = "cv", number = 5)




train_DM = xgb.DMatrix(data = as.matrix(train_1he), label = train_1he$SalePrice)
test_DM = xgb.DMatrix(data = as.matrix(test_1he))

caret_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.2, 0.1, 0.05, 0.025),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3, 4, 5),
  subsample = 1
)

#

# XGBoost linear regression

# Takes 15 minutes to hypertune params
# Keep commented

# caret_bst = train(x = train_1he,
#                   y = combined_1he$SalePrice[is.na(combined_1he$SalePrice) == 0],
#                   method = 'xgbTree',
#                   trControl = control,
#                   tuneGrid = caret_grid)
# caret_bst$bestTune

# Hypertined param results
#   nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 7    1000         3 0.025     0                1                2         1


# xgb_param = list(booster = "gbtree",
#              objective = "reg:squarederror",
#              subsample = 1,
#              max_depth = 3,
#              colsample_bytree = 1,
#              eta = 0.025,
#              min_child_weight = 2)
#
# xgb_cv = xgb.cv(data = train_DM,
#                 params = xgb_param,
#                 nrounds = 1000,
#                 nfold = 5,
#                 print_every_n = 50,
#                 early_stopping_rounds = 20)
#
# bst = xgboost(data = as.matrix(train_1he), label = train_1he$SalePrice, nrounds = 500)
# bst_DM = xgb.train(data = train_DM, nthread = 10, nrounds = 500, params = xgb_param)
# xgb_Pred = predict(bst, test_1he$SalePrice)
# xgb_DM_Pred = predict(bst_DM, test_DM)
# #xgbPred = exp(xgbPred)
# head(xgb_Pred)
# head(xgb_DM_Pred)
#head(LmPrediction)





test_factored$SalePrice = exp(0.03*LmPrediction + 0.02*FmPrediction + 0.95*RFPrediction)


output = test_factored[c("Id", "SalePrice")]
output = output[order(output$Id),]





write.csv(output, "Output.csv", row.names = FALSE)
