library(readxl)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)

nut_df <- read_excel("Chick-fil-A Nutrition Information.xlsx")
nut_df <- nut_df %>% rename(Meal = Breakfast)

num_rows <- nrow(nut_df)
num__of_breakfast_rows <- which(nut_df['Meal'] == 'Breakfast Breads')

prices = c(3.55, 3.85, 4.55, 4.85, 4.79, 4.79, 4.45, 3.79, 3.79, 4.65, 
           3.99, 3.99, 1.55, 4.75, 3.99, 1.59, 5.75, 4.85, 5.55, 5.25, 
           5.95, 6.49, 8.19, 4.85, 5.69, 7.19, 7.79, 9.49, 9.69, 9.69, 
           2.35, 3.99, 3.19, 3.85, 7.99, 2.35, 2.09, 2.29)

nut_df <- nut_df %>% mutate(Price = prices)

model <- MIPModel()

model <- model %>% 
  add_variable(x[i], i = 1:num_rows, type='binary')

model <- model %>%
  set_objective(sum_over(nut_df$Price[i] * x[i], i = 1:num_rows), "min")

# diet may not provide more than 3000 calories
model <- model %>% 
  add_constraint(sum_over(nut_df$CALORIES[i] * x[i], i= 1:num_rows) <= 3000)

# diet may not provide less than 2000 calories
# may lower constraint to 1810
model <- model %>% 
  add_constraint(sum_over(nut_df$CALORIES[i] * x[i], i= 1:num_rows) >= 2000)

# diet may not provide more than 100g of total fat
model <- model %>% 
  add_constraint(sum_over(nut_df$`FAT (G)`[i] * x[i], i= 1:num_rows) <= 100)

# diet may not provide more than 20g of saturated fat
model <- model %>% 
  add_constraint(sum_over(nut_df$`SAT. FAT (G)`[i] * x[i], i = 1:num_rows) <= 20)

# diet must provide no more than 500g of carbohydrates
model <- model %>%
  add_constraint(sum_over(nut_df$`CARBOHYDRATES (G)`[i]*x[i], i=1:num_rows) <= 500)

# diet must provide no more than 3000mg of sodium
# may increase to 3190 to find a soltion
model <- model %>% 
  add_constraint(sum_over(nut_df$`SODIUM (MG)`[i]*x[i], i=1:num_rows) <= 3000)

# diet must provide at least 30g of fiber
model <- model %>% 
  add_constraint(sum_over(nut_df$`FIBER (G)`[i] * x[i], i = 1:num_rows) >= 20)

# diet must provide no more than 50g of sugar
# may increase to 63 to find a feasible solution
model <- model %>%
  add_constraint(sum_over(nut_df$`SUGAR (G)`[i] * x[i], i = 1:num_rows) <= 50)

# diet must provide at least 80g of protein
model <- model %>% 
  add_constraint(sum_over(nut_df$`PROTEIN (G)`[i] * x[i], i= 1:num_rows) >= 80)

# the breakfast must provide at least 500 calories
model <- model %>% 
  add_constraint(sum_over(nut_df$CALORIES[i] * x[i], i = 1:num__of_breakfast_rows) >= 500)

# the breakfast must provide no more than 1000 calories
model <- model %>% 
  add_constraint(sum_over(nut_df$CALORIES[i] * x[i], i = 1:num__of_breakfast_rows) <= 1000)


result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

solution <- result %>%
  get_solution(x[i]) %>%
  filter(value > 0) %>%
  select(i)

solution <- solution %>% mutate(i = case_when(i == 1 ~ num_df$Meal[1], i == 2 ~ num_df$Meal[2], i == 3 ~ num_df$Meal[3], i == 4 ~ num_df$Meal[4],
                                i == 5 ~ num_df$Meal[5], i == 6 ~ num_df$Meal[6], i == 7 ~ num_df$Meal[7], i == 8 ~ num_df$Meal[8],
                                i == 9 ~ num_df$Meal[9], i == 10 ~ num_df$Meal[10], i == 11 ~ num_df$Meal[11], i == 12 ~ num_df$Meal[12],
                                i == 13 ~ num_df$Meal[13], i == 14 ~ num_df$Meal[14], i == 15 ~ num_df$Meal[15], i == 16 ~ num_df$Meal[16],
                                i == 17 ~ num_df$Meal[17], i == 18 ~ num_df$Meal[18], i == 19 ~ num_df$Meal[19], i == 20 ~ num_df$Meal[20],
                                i == 12 ~ num_df$Meal[21], i == 22 ~ num_df$Meal[22], i == 23 ~ num_df$Meal[23], i == 24 ~ num_df$Meal[24],
                                i == 25 ~ num_df$Meal[25], i == 26 ~ num_df$Meal[26], i == 27 ~ num_df$Meal[27], i == 28 ~ num_df$Meal[28],
                                i == 29 ~ num_df$Meal[29], i == 30 ~ num_df$Meal[30], i == 31 ~ num_df$Meal[31], i == 32 ~ num_df$Meal[32],
                                i == 33 ~ num_df$Meal[33], i == 34 ~ num_df$Meal[34], i == 35 ~ num_df$Meal[35], i == 36 ~ num_df$Meal[36],
                                i == 37 ~ num_df$Meal[37], i == 38 ~ num_df$Meal[38]))
