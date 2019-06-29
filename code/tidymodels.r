
man_data <- readr::read_csv(
    'https://www.jaredlander.com/data/manhattan_Train.csv'
    )

View(man_data)

library(rsample)

set.seed(123)
man_split <- initial_split(data=man_data, 
                           prop=0.9,
                           strata='TotalValue'
)
man_split

man_train <- training(man_split)
man_test <- testing(man_split)

library(parsnip)
library(recipes)

base_formula <- TotalValue ~ FireService +
    ZoneDist1 + ZoneDist2 + 
    Class + OwnerType + 
    LotArea + BldgArea + 
    NumFloors + UnitsTotal
base_formula

mod1 <- lm(base_formula, data=man_train)
summary(mod1)
library(coefplot)
coefplot(mod1, sort='magnitude')
coefplot(mod1, sort='magnitude', lwdInner=2, lwdOuter=1)

base_recipe <- recipe(base_formula, data=man_train)
base_recipe

tail(head(iris, n=4), n=1)
iris %>% head(n=4) %>% tail(n=1)

man_recipe <- base_recipe %>% 
    step_nzv(all_predictors()) %>% 
    step_log(TotalValue, base=10) %>% 
    step_center(all_numeric(), -all_outcomes()) %>% 
    step_scale(all_numeric(), -all_outcomes()) %>% 
    step_other(all_nominal()) %>% 
    step_dummy(all_nominal(), one_hot=TRUE)
man_recipe

man_prepped <- man_recipe %>% prep(data=man_train, retain=FALSE)
man_prepped
class(man_prepped)
man_prepped$steps

train_baked <- man_prepped %>% juice()
train_baked <- man_prepped %>% bake(new_data=man_train)
test_baked <- man_prepped %>% bake(new_data=man_test)

pryr::object_size(man_prepped)

xgmod <- boost_tree(mode='regression') %>% 
    set_engine('xgboost')
xgmod

mod2 <- xgmod %>% 
    fit(TotalValue ~ ., data=train_baked)
mod2
mod2$fit

library(xgboost)

mod2$fit %>% 
    xgb.plot.multi.trees()

mod2$fit %>% 
    xgb.importance(model=.) %>% 
    `[`(1:15) %>% 
    xgb.plot.importance()

library(RXKCD)
getXKCD(927)

mod3 <- boost_tree(mode='regression', 
                   trees=130,
                   tree_depth=4,
                   learn_rate=0.25
) %>% 
    set_engine('xgboost') %>% 
    fit(TotalValue ~ ., data=train_baked)

rand_forest
linear_reg
logistic_reg

preds3 <- predict(mod3, new_data=test_baked)
preds3

truepred3 <- test_baked %>% 
    dplyr::select(TotalValue) %>% 
    dplyr::bind_cols(preds3)

library(yardstick)

rmse(truepred3, truth=TotalValue, estimate=.pred)
