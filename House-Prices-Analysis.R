library(tidyverse)
library(caret)

getwd()
list.files()

train_house <- read_csv('../train.csv')
test_house <- read_csv('../test.csv')

train_house <- train_house %>% 
  mutate(Notes = 'Train')
test_house <- test_house %>% 
  mutate(Notes = 'Test')

df_full <- train_house %>% 
  bind_rows(test_house)

df_full <- df_full %>% 
  mutate(YearRemodAdd = if_else(YearRemodAdd < YearBuilt, YearBuilt, YearRemodAdd),
         GarageYrBlt = if_else((!is.na(GarageYrBlt) & GarageYrBlt < YearBuilt), YearBuilt, GarageYrBlt))

df_full<- df_full %>% 
  mutate(Remodel = if_else(YearBuilt == YearRemodAdd, 'Y', 'N'))

df_full<- df_full %>%
  mutate(MSSubClass = as.factor(MSSubClass),
         OverallQual = as.factor(OverallQual),
         OverallCond = as.factor(OverallCond))



df_full <- df_full %>% 
  mutate(HouseAgeOld = YrSold - YearBuilt,
         HouseAgeNew = YrSold - YearRemodAdd)

summary(df_full)


# Replace NA --------------------------------------------------------------

is.na(df_full) %>% colSums()

## LotFrontage ===

df_full$LotFrontage[is.na(df_full$LotFrontage)] <- 0

## Alley ===

df_full$Alley[is.na(df_full$Alley)] <- 'None'

## MasVnrArea ===

df_full %>% 
  select(MasVnrType, MasVnrArea) %>% 
  filter(MasVnrType == 'None', MasVnrArea != 0)

df_full$MasVnrArea[df_full$MasVnrType == 'None'] <- 0

df_full %>% filter(is.na(MasVnrArea)| is.na(MasVnrType)) %>% select(MasVnrType, MasVnrArea) %>% view()
## Basement ===

(df_full %>% 
  select(contains('Bsmt')))[!complete.cases(df_full %>% 
                                              select(contains('Bsmt'))),] %>% view()

df_full <- df_full %>%
  mutate(BsmtQual = if_else((BsmtFinSF1 == 0 & BsmtUnfSF == 0), 'None', BsmtQual),
         BsmtCond = if_else((BsmtFinSF1 == 0 & BsmtUnfSF == 0), 'None', BsmtCond),
         BsmtExposure = if_else((BsmtFinSF1 == 0 & BsmtUnfSF == 0), 'None', BsmtExposure),
         BsmtFinType1 = if_else((BsmtFinSF1 == 0 & BsmtUnfSF == 0), 'None', BsmtFinType1),
         BsmtFinType2 = if_else((BsmtFinSF2 == 0 & BsmtUnfSF == 0), 'None', BsmtFinType2),
         BsmtFullBath = if_else((BsmtFinType1 == 'None' & BsmtFinType2 == 'None'), 0, BsmtFullBath),
         BsmtHalfBath = if_else((BsmtFinType1 == 'None' & BsmtFinType2 == 'None'), 0, BsmtHalfBath))

df_full <- df_full %>%
  mutate(BsmtQual = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 'None', BsmtQual),
         BsmtCond = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 'None', BsmtCond),
         BsmtExposure = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 'None', BsmtExposure),
         BsmtFinType1 = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 'None', BsmtFinType1),
         BsmtFinSF1 = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, BsmtFinSF1),
         BsmtFinType2 = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 'None', BsmtFinType2),
         BsmtFinSF2 = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, BsmtFinSF2),
         BsmtUnfSF = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, BsmtUnfSF),
         BsmtFullBath = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, BsmtFullBath),
         BsmtHalfBath = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, BsmtHalfBath),
         TotalBsmtSF = if_else((is.na(BsmtFinType1) & is.na(BsmtFinType2)), 0, TotalBsmtSF))

df_full <- df_full %>%
  mutate(BsmtFinSF1 = if_else(BsmtFinType1 == 'None', 0, BsmtFinSF1),
         BsmtFinType2 = if_else(BsmtFinType1 == 'None', 'None', BsmtFinType2),
         BsmtFinSF2 = if_else(BsmtFinType1 == 'None', 0, BsmtFinSF2),
         BsmtUnfSF = if_else(BsmtFinType1 == 'None', 0, BsmtUnfSF),
         BsmtFullBath = if_else(BsmtFinType1 == 'None', 0, BsmtFullBath),
         BsmtHalfBath = if_else(BsmtFinType1 == 'None', 0, BsmtHalfBath),
         TotalBsmtSF = if_else(BsmtFinType1 == 'None', 0, TotalBsmtSF))

df_bsmt <- df_full %>% 
  select(contains('Bsmt')) %>% 
  mutate_if(is.character, as.factor)

TrainDummBsmt <- dummyVars(~., df_bsmt)
df_bsmt_dumm <- predict(TrainDummBsmt, df_bsmt)

preprocess_bsmt <- preProcess(df_bsmt_dumm, method = 'bagImpute')
df_bsmt_imputed <- predict(preprocess_bsmt, df_bsmt_dumm)

## Fireplace ===

df_full %>% 
  filter(is.na(FireplaceQu)) %>% 
  select(Fireplaces, FireplaceQu) %>% count(Fireplaces)

df_full <- df_full %>% 
  mutate(FireplaceQu = if_else(Fireplaces == 0, 'None', FireplaceQu))

## Garage ===

(df_full %>% 
    select(contains('Garage')))[!complete.cases(df_full %>% 
                                                select(contains('Garage'))),] %>% view()

df_full <- df_full %>%
  mutate(GarageType = if_else((GarageCars == 0 | GarageArea == 0), 'None', GarageType),
         GarageYrBlt = if_else((GarageCars == 0 | GarageArea == 0), 0, GarageYrBlt),
         GarageFinish = if_else((GarageCars == 0 | GarageArea == 0), 'None', GarageFinish),
         GarageCars = if_else((GarageCars == 0 | GarageArea == 0), 0, GarageCars),
         GarageArea = if_else((GarageCars == 0 | GarageArea == 0), 0, GarageArea),
         GarageQual = if_else((GarageCars == 0 | GarageArea == 0), 'None', GarageQual),
         GarageCond = if_else((GarageCars == 0 | GarageArea == 0), 'None', GarageCond))

df_full <- df_full %>%
  mutate(GarageType = if_else(is.na(GarageArea), 'None', GarageType),
         GarageYrBlt = if_else(is.na(GarageArea), 0, GarageYrBlt),
         GarageFinish = if_else(is.na(GarageArea), 'None', GarageFinish),
         GarageCars = if_else(is.na(GarageArea), 0, GarageCars),
         GarageArea = if_else(is.na(GarageArea), 0, GarageArea),
         GarageQual = if_else(is.na(GarageArea), 'None', GarageQual),
         GarageCond = if_else(is.na(GarageArea), 'None', GarageCond))

df_full %>% 
  filter(is.na(GarageYrBlt)|gar) %>% 
  select(contains('Garage'), YearBuilt, YearRemodAdd)

## Kitchen

df_full %>% 
  select(contains('Kitchen'))


# Explore -----------------------------------------------------------------

# MSSubClass ---

table(df_full$MSSubClass)

train_house %>% 
  ggplot(aes(as.factor(MSSubClass), SalePrice)) +
  geom_boxplot()

anova_massubclass <- aov(lm(SalePrice ~ as.factor(MSSubClass), train_house))
summary(anova_massubclass)
TukeyHSD(anova_massubclass) %>% plot()

# MSZoning ---

table(df_full$MSZoning, useNA = 'ifany')

train_house %>% 
  ggplot(aes(as.factor(MSZoning), SalePrice)) +
  geom_boxplot()

aov(lm(SalePrice ~ as.factor(MSZoning), train_house)) %>% summary()

# Lot Frontage ---

