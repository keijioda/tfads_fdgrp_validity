
# Set up ------------------------------------------------------------------

# Packages
pacs <- c("tidyverse", "readxl", "lubridate")
sapply(pacs, require, character.only = TRUE)

# Function to search variables
search_var <- function(df, pattern, ...) {
  loc <- grep(pattern, names(df), ...)
  if (length(loc) == 0) warning("There are no such variables")
  else return(data.frame(loc = loc, varname = names(df)[loc]))
}

# Food Report data --------------------------------------------------------

# 6141 entries from n = 71 subjects
fr <- read_excel("./data/TFADS_Food Records_Food file.xlsx")

# Number of distinct ID: n = 71
fr %>% select(`Participant/Recipe/Menu ID`) %>% n_distinct()

# How many days of reporting?
fr %>% 
  group_by(`Participant/Recipe/Menu ID`) %>% 
  summarize(ndays = n_distinct(`Date of Intake`)) %>% 
  group_by(ndays) %>% 
  tally()

# Delete some intake days
dates_to_del <- read_excel("./data/ID Intake date of those reported more than 6 days.xlsx") %>% 
  filter(Delete == 1) %>% 
  select(id, Date)

# Anti-join by ID and Date
fr <- fr %>% 
  anti_join(dates_to_del, by = c(`Participant/Recipe/Menu ID` = "id", `Date of Intake` = "Date"))

# Calculate the total energy by ID & date
fr_kcal <- fr %>% 
  group_by(`Participant/Recipe/Menu ID`, `Date of Intake`) %>% 
  summarize(kcal = sum(`Energy (kcal)`, na.rm = TRUE)) %>% 
  arrange(`Participant/Recipe/Menu ID`, `Date of Intake`) %>% 
  mutate(ID = `Participant/Recipe/Menu ID`, Date = as_date(`Date of Intake`)) %>% 
  ungroup() %>% 
  select(ID, Date, kcal)

# Checking legumes
fr %>% 
  filter(`NCC Database Food Group ID` == 72) %>% 
  distinct(`Food ID`, `Food Name`, `Food Name`, `NCC Database Food Group ID`) %>% 
  arrange(`Food ID`) %>% 
  print(n = Inf)

# Read food group definition file
# Based on NCC Food Group ID
fg_def <- read_excel("./data/NCCFGIDs and Food Groups.xlsx", sheet = 1)

# Based on Food ID
fg_fi_def <- read_excel("./data/NCCFGIDs and Food Groups.xlsx", sheet = 2)

# Calculate servings for each food eaten
# Merge with Food report data
fr2 <- fr %>% 
  select(2:18) %>% 
  mutate(Date = as_date(`Date of Intake`),
         FoodID = as.numeric(`Food ID`),
         servings = `Gram Amount of Food` / `FDA Serving Size (Grams)`) %>% 
  select(1, Date, everything(), -`Date of Intake`) %>% 
  filter(!is.na(`NCC Database Food Group ID`)) %>% 
  left_join(fg_def, by = c("NCC Database Food Group ID" = "NCCFGID")) %>% 
  left_join(fg_fi_def, by = c("FoodID" = "FoodID")) %>% 
  mutate(FoodGroups = ifelse(!is.na(FoodGroups.y), FoodGroups.y, FoodGroups.x)) %>% 
  select(-FoodGroups.x, -FoodGroups.y)

# Check legumes
fr2 %>% 
  filter(`NCC Database Food Group ID` == 72) %>% 
  distinct(`Food Name`, `Food ID`, `NCC Database Food Group ID`, FoodGroups)
fr2 %>% 
  filter(FoodGroups == "Legumes") %>% 
  distinct(`Food Name`, `Food ID`, `NCC Database Food Group ID`) %>% 
  print(n = Inf)

# Check avocado
fr2 %>% 
  filter(FoodGroups == "Avocado") %>% 
  distinct(FoodGroups, `Food Name`, `Food ID`, `NCC Database Food Group ID`) %>% 
  print(n = Inf)

fr2 %>% 
  filter(grepl("avocado|guacamole", `Food Name`)) %>% 
  distinct(FoodGroups, `Food Name`, `Food ID`, `NCC Database Food Group ID`) %>% 
  print(n = Inf)

fr2 %>% 
  filter(`Food Name` == "Chipotle, guacamole") %>% 
  distinct(`Food Name`, `Food ID`, `NCC Database Food Group ID`, `Gram Amount of Food`) %>% 
  print(n = Inf)

# Missing food groups -- Mostly pizzas. Ignore them for now.
# fr2 %>% 
#   filter(is.na(FoodGroups)) %>% 
#   select(`NCC Database Food Group ID`, `Food ID`, FoodGroups, `Food Name`) %>% 
#   arrange(`NCC Database Food Group ID`) %>% 
#   View()

fr2 %>% 
  filter(is.na(FoodGroups)) %>% 
  select(`NCC Database Food Group ID`, FoodGroups) %>% 
  distinct() %>% 
  arrange(`NCC Database Food Group ID`)

# Unique food group IDs
# fr %>% 
#   group_by(`NCC Database Food Group ID`) %>% 
#   tally() %>% 
#   print(n = Inf)

# There are 84 unique food groups
# unique_fg_fr <- fr %>% 
#   distinct(`NCC Database Food Group ID`) %>% 
#   inner_join(nds_fg) %>% 
#   arrange(`NCC Database Food Group ID`)


# Sum up by food group, date of intake, and ID
fr_subj_by_fg_long <- fr2 %>%   
  mutate(ID = `Participant/Recipe/Menu ID`) %>% 
  select(-`Participant/Recipe/Menu ID`) %>% 
  group_by(ID, Date, FoodGroups) %>% 
  summarize(servings = sum(servings, na.rm = TRUE)) %>% 
  filter(!is.na(FoodGroups)) 

# Convert to wide format
# Attach total energy intake
# 426 records x 38 variables
fr_subj_by_fg_wide <- fr_subj_by_fg_long %>% 
  arrange(FoodGroups) %>% 
  pivot_wider(names_from = FoodGroups, values_from = servings, values_fill = 0) %>% 
  arrange(ID, Date) %>% 
  inner_join(fr_kcal, by = c("ID", "Date")) %>% 
  select(ID, Date, kcal, everything()) %>% 
  ungroup()

fr_subj_by_fg_wide2 <- fr_subj_by_fg_wide %>% 
  rowwise() %>% 
  mutate(MeatPoultFish = sum(Meat, Poultry, Fish, ProcessedMeat),
         Dairy         = sum(MilknMilkbased, Cheese, SweetenedDairy, Yogurt, Cream),
         MilkDairySub  = sum(MilkSubs, CheeseSubs, CreamSubs),
         Fruits        = sum(FreshFruits, Avocado, DryFruits, HundredpercentFruitJuice, MixedFruits),
         FreshFruits   = sum(FreshFruits, Avocado),
         BrdGrnCrls    = sum(Breads, Grains, Cereals),
         Snacks        = sum(PopcornNChips, GranolaFruitDietBars, CakesCookies, CandySyrupPreserves),
         Beverages     = sum(Water, Soda, FruitJuiceFlavorDrinks, CoffeeTea)) %>% 
  ungroup()

fr_unique_id <- fr_subj_by_fg_wide2 %>% 
  distinct(ID)

# Check total energy intake
summary(fr_subj_by_fg_wide$kcal)
fr_subj_by_fg_wide %>% 
  ggplot(aes(x = kcal)) + geom_histogram(bins = 15)

fr_subj_by_fg_wide %>% 
  filter(kcal < 500) %>% 
  select(ID, Date, kcal)

# Output to CSV
# write_csv(fr_subj_by_fg_wide2, "FR Subject by Food Group.csv")

# Check % of zero intake
selected_foods <- c("MeatPoultFish", "MeatSubs", "Dairy", "MilkDairySub", "Eggs", "NutsNutbutter", 
                    "FatsOils", "Fruits", "Vegetables", "Legumes", "BrdGrnCrls", "Snacks", "Beverages")

fr_subj_by_fg_wide2 %>% 
  select(-(ID:kcal)) %>% 
  map(function(x) round(mean(x == 0), 2)) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  rownames_to_column("fg") %>% 
  filter(fg %in% selected_foods)

drop_id <- fr_subj_by_fg_wide2 %>% 
  filter(kcal < 500) %>% 
  distinct(ID)

pct_0 <- fr_subj_by_fg_wide %>%
  select(-(ID:kcal)) %>%
  map(function(x) mean(x == 0)) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  arrange(1)

pct_0 %>%
  filter(V1 < 0.7)

# FFQ Recipe File ---------------------------------------------------------

# 680 entries
ffq_recipe <- read_excel("./data/TFADSFFQ_Food group_varnames.xlsx") %>% 
  select(1:6) %>% 
  mutate(food_var = `Participant/Recipe/Menu ID`,
         `Participant/Recipe/Menu ID` = NULL ) %>% 
  select(food_var, everything())

ffq_recipe %>% 
  filter(grepl("avocado|guacamole", `Food Name`)) 

# from 151 foods (variables)
ffq_recipe_foods <- unique(ffq_recipe$food_var)
length(ffq_recipe_foods)

# Define food groups
# Calculate serving size
# Ignore avocado food group for FFQ
ffq_recipe_fg <- ffq_recipe %>%
  left_join(fg_def, by = c(`NCC Database Food Group ID` = "NCCFGID")) %>% 
  mutate(FoodID = as.numeric(`Food ID`)) %>% 
  left_join(fg_fi_def %>% filter(FoodGroups != "Avocado"), by = "FoodID") %>% 
  mutate(FoodGroups = ifelse(!is.na(FoodGroups.y), FoodGroups.y, FoodGroups.x),
         servings = `Gram Amount of Food` / `FDA Serving Size (Grams)`) %>% 
  select(-FoodGroups.x, -FoodGroups.y)

ffq_recipe_fg %>% 
  filter(FoodGroups == "Avocado")
ffq_recipe_fg %>% 
  filter(FoodGroups == "Legumes")  %>% 
select(food_var, `Food Name`, `Food ID`, `NCC Database Food Group ID`, FoodGroups)
ffq_recipe_fg %>% 
  filter(`NCC Database Food Group ID` == 72) %>% 
  select(food_var, `Food Name`, `Food ID`, `NCC Database Food Group ID`, FoodGroups)

# Check missing food groups -- all of them are 135: Industry additives. Ignore them.
ffq_recipe_fg %>% 
  filter(is.na(FoodGroups)) %>% 
  print(n = Inf)

# Sum by food groups
ffq_recipe_fg_long <- ffq_recipe_fg %>% 
  filter(!is.na(FoodGroups)) %>% 
  group_by(food_var, FoodGroups) %>% 
  summarize(servings = sum(servings, na.rm = TRUE))

# Convert to wide
ffq_recipe_fg_wide <- ffq_recipe_fg_long %>% 
  arrange(FoodGroups) %>% 
  pivot_wider(values_from = servings, names_from = FoodGroups, values_fill = 0) %>% 
  arrange(food_var)

# Output to CSV
# write_csv(ffq_recipe_fg_wide, "FFQ Food by Food Group.csv")

# FFQ Intake Data File ----------------------------------------------------

# 601 subjects, 166 variables (160 foods)
ffq <- read_excel("./data/TFADS FFQ_freq.xls") %>% 
  rename(BCOFFSOY = BCOFFBSOY,
         BOMLKA = BOMLKAL)

# Remove demog vars and foods ending with _T
# Ends up with 151 foods
ffq0 <- ffq %>% 
  select(-(ID:School)) %>% 
  select(-ends_with("_T"))

# Compare variable names
data.frame(x = names(ffq0)[order(names(ffq0))],
           y = ffq_recipe_fg_wide$food_var) %>% 
  mutate(flag = ifelse(x == y, 0, 1))

# Rearrange columns by alphabetical order
ffq1 <- ffq0 %>% 
  select(names(ffq0)[order(names(ffq0))])

# Matrix multiplication
# Check if matrices are conformable
dim(as.matrix(ffq1))
dim(as.matrix(ffq_recipe_fg_wide[-1]))

ffq_subj_by_fg <- as.matrix(ffq1) %*% as.matrix(ffq_recipe_fg_wide[-1]) %>% 
  as.data.frame() %>% 
  bind_cols(ffq %>% select(ID:School)) %>% 
  select(ID:School, everything()) %>% 
  as_tibble()

# CreamSubs is not present on FFQ
ffq_subj_by_fg_all <- ffq_subj_by_fg %>% 
  rowwise() %>% 
  mutate(MeatPoultFish = sum(Meat, Poultry, Fish, ProcessedMeat),
         Dairy         = sum(MilknMilkbased, Cheese, SweetenedDairy, Yogurt, Cream),
         MilkDairySub  = sum(MilkSubs, CheeseSubs),
         Fruits        = sum(FreshFruits, DryFruits, HundredpercentFruitJuice, MixedFruits),
         BrdGrnCrls    = sum(Breads, Grains, Cereals),
         Snacks        = sum(PopcornNChips, GranolaFruitDietBars, CakesCookies, CandySyrupPreserves),
         Beverages     = sum(Water, Soda, FruitJuiceFlavorDrinks, CoffeeTea)) %>% 
  ungroup()

# Add kcal from FFQ nutrient intake
ffq_nutr <- read_csv("./data/TFADS_FFQ_nutrient_intake.csv") %>% 
  select(ID:energy) %>% 
  rename(kcal = energy)

ffq_subj_by_fg_all2 <- ffq_subj_by_fg_all %>% 
  inner_join(ffq_nutr %>% select(ID, kcal), by = "ID") %>% 
  select(ID:School, kcal, everything())

# Output to CSV
# write_csv(ffq_subj_by_fg_all2, "FFQ Subject by Food Group All FFQ Subj.csv", na = "")


# Apply inclusion/exclusion criteria --------------------------------------

# Inclusion-exclusion criteria
filtered_ffq <- ffq_nutr %>% 
  filter(Gender == 1 & between(kcal, 900, 3500) | Gender == 2 & between(kcal, 1000, 4500))

# Match ID with validation subjects
ffq_subj_by_fg_validity <- ffq_subj_by_fg_all2 %>% 
  semi_join(filtered_ffq, by = "ID") %>% 
  semi_join(fr_unique_id, by = "ID")

# Output to CSV
# write_csv(ffq_subj_by_fg_validity, "FFQ Subject by Food Group Validity Subj.csv", na = "")

# Do the same for Food report
fr_subj_by_fg_validity <- fr_subj_by_fg_wide2 %>% 
  semi_join(filtered_ffq, by = "ID")

# Check the number of unique ID, n = 62
fr_subj_by_fg_validity %>% 
  distinct(ID) %>% nrow()

# Cluster analysis --------------------------------------------------------
library(factoextra)

# Cluster analysis using average linkage
hc <- ffq_subj_by_fg_all %>% 
  select(Meat, FreshFruits, Cereals, Eggs, Cheese, Breads, FruitJuiceFlavorDrinks, Grains, Water,
         Vegetables, Legumes, MilknMilkbased, MeatSubs) %>% 
  na.omit() %>% 
  t() %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "average")

fviz_dend(hc, rect = TRUE, horiz = TRUE, k = 4, k_colors = "nejm", rect_border = "nejm",
          color_labels_by_k = TRUE, rect_fill = TRUE, ggtheme = theme_gray(),
          main = "Cluster dendrogram using FFQ food group intake data")

pdf("Cluster dendrogram FFQ food group 4.pdf", height = 6, width = 11)
hc
dev.off()

hc <- fr_subj_by_fg_wide2 %>% 
  select(Meat, FreshFruits, Cereals, Eggs, Cheese, Breads, FruitJuiceFlavorDrinks, Grains, Water,
         Vegetables, Legumes) %>% 
  na.omit() %>% 
  t() %>% 
  scale() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "average")

fviz_dend(hc, rect = TRUE, horiz = TRUE, k = 6, k_colors = "nejm", rect_border = "nejm",
          color_labels_by_k = TRUE, rect_fill = TRUE, ggtheme = theme_gray(),
          main = "Cluster dendrogram using FR food group intake data")


# Energy adjustment -------------------------------------------------------

# Function for energy-adjustment with zero partition
kcal_adjust <- function(var, kcal, log=TRUE){
  df <- data.frame(y = var, ea_y = var, kcal = kcal)
  count_negative <- sum(df$y < 0, na.rm=TRUE)
  if (count_negative > 0)
    warning("There are negative values in variable.")
  if(log) df$y[df$y > 0 & !is.na(df$y)] <- log(df$y[df$y > 0 & !is.na(df$y)])
  mod <- lm(y ~ kcal, data=df[df$y != 0, ])
  if(log){
    ea <- exp(resid(mod) + mean(df$y[df$y != 0], na.rm=TRUE))
    df$ea_y[!is.na(df$y) & df$y != 0] <- ea
  }
  else{
    ea <- resid(mod) + mean(df$y[df$y != 0], na.rm=TRUE)
    df$ea_y[!is.na(df$y) & df$y != 0] <- ea
  }
  return(df$ea_y)
}

ffq_ea <- ffq_subj_by_fg_validity %>% 
  select(Breads:Beverages) %>% 
  lapply(kcal_adjust, ffq_subj_by_fg_validity$kcal) %>% 
  bind_cols(ffq_subj_by_fg_validity %>% select(ID:kcal)) %>% 
  select(ID:kcal, everything())

fr_ea <- fr_subj_by_fg_validity %>% 
  select(Breads:Beverages) %>% 
  lapply(kcal_adjust, fr_subj_by_fg_validity$kcal) %>% 
  bind_cols(fr_subj_by_fg_validity %>% select(ID:kcal)) %>% 
  select(ID:kcal, everything())

# fr_ea %>% 
#   mutate(avoc = ifelse(Avocado > 0, 1, 0)) %>% 
#   filter(avoc == 1) %>% 
#   summarise(n = n_distinct(ID))
# 
# fr_ea %>% 
#   summarise(n = n_distinct(ID))

# write_csv(ffq_ea, "FFQ Subject by Food Group Validity Subj Kcal Adjusted.csv")
# write_csv(fr_ea, "FR Subject by Food Group Validity Subj Kcal Adjusted.csv")


# ffq_ea2 <- ffq_ea %>% arrange(ID)
# fr_ea2 <- fr_ea %>% 
#   group_by(ID) %>% 
#   summarize(across(kcal:Beverages, mean)) %>% 
#   arrange(ID)
# 
# ffq_ea2 %>% 
#   inner_join(fr_ea2, by = "ID", suffix = c("_ffq", "_fr")) %>% 
#   write.csv("Final validity data.csv", row.names = FALSE)


# Deattenuated correlation ------------------------------------------------

# Function for calculating deattenuated correlation
deatten <- function(y, x){
  m <- length(x)
  crude.cor <- cor(y, apply(x, 1, mean))
  VarWI <- mean(apply(x, 1, var))
  VarBW <- var(apply(x, 1, mean)) - VarWI / m
  deatten.cor <- crude.cor * sqrt(1 + VarWI / m / VarBW)
  out <- data.frame(crude.cor = crude.cor, VarWI = VarWI, 
                    VarBW=VarBW, deatten.cor = deatten.cor)
  return(out)
}

# Function for zero-skewness log-transformation
library(e1071)
skew.score <- function(c, x) (skewness(log(x + c)))^2

# Convert to wide for a specified food group before passing to deatten 
deatten_fg <- function(var){
  y <- fr_ea %>% 
    arrange(ID, Date) %>% 
    group_by(ID) %>% 
    mutate(Day = 1:n()) %>% 
    select(ID, Day, {{var}}) %>% 
    pivot_wider(names_from = Day, names_prefix = "Day", values_from = {{var}}) %>% 
    ungroup()
  x <- ffq_ea %>% select(ID, {{var}})
  best.c <- optimise(skew.score, c(0, 1), x = pull(x, {{var}}))$minimum
  d <- inner_join(x, y, by = "ID") %>% 
    mutate_at(vars(-ID), function(x) log(x + best.c))
  return(deatten(d[,2], d[, 3:8]))
}

# Run correlation
fg_needed <- c("MeatPoultFish", "MeatSubs", "Dairy", "MilkDairySub", "Eggs", "NutsNutbutter", "FatsOils",
               "Fruits", "Vegetables", "Legumes", "BrdGrnCrls", "Snacks", "Beverages")

# fg_needed <- c("MeatPoultFish", "MeatSubs", "Dairy", "MilkDairySub", "Eggs", "NutsNutbutter", "FatsOils",
#                "Fruits", "Vegetables", "Legumes", "BrdGrnCrls", "Snacks", "Beverages", "Avocado")
fg1a <- fg_needed %>% 
  map(deatten_fg) %>% 
  do.call(rbind, .) %>% 
  round(3)

all_fg <- ffq_ea %>% 
  select(Breads:Beverages, -IceCreamSubs) %>% 
  names()

all_fg %>% 
  map(deatten_fg) %>% 
  do.call(rbind, .) %>% 
  round(3) %>% 
  arrange(-deatten.cor)

# Bootstrap CI ------------------------------------------------------------

library(boot)

# Transform data for bootstrap
# Use zero-skewness log transformation on FFQ variable
boot_data <- function(var){
  y <- fr_ea %>% 
    arrange(ID, Date) %>% 
    group_by(ID) %>% 
    mutate(Day = 1:n()) %>% 
    select(ID, Day, {{var}}) %>% 
    pivot_wider(names_from = Day, names_prefix = "Day", values_from = {{var}}) %>% 
    ungroup()
  x <- ffq_ea %>% select(ID, {{var}})
  best.c <- optimise(skew.score, c(0, 1), x = pull(x, {{var}}))$minimum
  d <- inner_join(x, y, by = "ID") %>% 
    mutate_at(vars(-ID), function(x) log(x + best.c))
  return(d)
}

# Function for bootstrap
# Returns Fisher's z transformation
boot_dc <- function(data, i){
  d <- data[i,] 
  r <- deatten(d[, 2], d[, 3:8])$deatten.cor
  return(log((1 + r) / (1 -r)) / 2)
}

# Function to back-transform Fisher's z
back_trans_z <- function(z) (exp(2 * z) - 1) / (exp(2 * z) + 1)

# Main function for bootstrap
run_boot <- function(var){
  data <- boot_data({{var}})
  out <- boot(data, boot_dc, R = 2000)
  ci  <- c(deat_cor = boot.ci(out)$t, lwr = boot.ci(out)$bca[4], upr = boot.ci(out)$bca[5])
  return(back_trans_z(ci))
}

set.seed(123)
fg1b <- fg_needed %>% 
  map(run_boot) %>% 
  do.call(rbind, .) %>% 
  as.data.frame(row.names = fg_needed) %>% 
  round(3)

cbind(fg1a, fg1b[-1]) %>% 
  arrange(-deatten.cor)

# Descriptive table -------------------------------------------------------

# Table 1
library(tableone)

ffq_ea2 <- ffq_ea %>% 
  mutate(Gender = factor(Gender, levels = 1:2, labels = c("Girl", "Boy")),
         Site  = ifelse(School %in% c("LLA", "LSA", "MSA", "RAA"), 1, 2),
         Site  = factor(Site, levels = 1:2, labels = c("CA", "MI")))

table_vars <- c("Gender", "Age", "Site")
temp <- CreateTableOne(vars = table_vars, data = ffq_ea2)
print(temp, showAllLevels = TRUE)

# Compare FFQ and FR ------------------------------------------------------

funs <- c(median = median, IQR = IQR)

compare <- function(var){
  df <- ffq_ea %>% 
    rename(FFQ = {{var}}) %>% 
    select(ID, FFQ) %>% 
    inner_join(fr_ea %>% 
                 group_by(ID) %>%
                 rename(fg = {{var}}) %>% 
                 summarize(FR = mean(fg)), by = "ID")
  pval <- wilcox.test(df$FFQ, df$FR, paired = TRUE)$p.value
  out <- df %>% 
    select(2:3) %>% 
    map_dbl(median)
  out2 <- df %>% 
    select(2:3) %>% 
    rename_all(paste0, "_IQR") %>% 
    map_dbl(IQR)
  return(c(out[1], out2[1], out[2], out2[2], pval = pval))
}

fg_needed %>% 
  map_df(compare) %>% 
  bind_cols(fg = fg_needed) %>% 
  column_to_rownames(var = "fg") %>% 
  round(4)

data_trans <- function(var){
  ffq_ea %>% 
    select(ID, !!sym(var)) %>% 
    mutate(Data = "FFQ") %>% 
    bind_rows(fr_ea %>% 
                group_by(ID) %>% 
                summarize(!!sym(var) := mean(!!sym(var))) %>% 
                mutate(Data = "FR"))
}

fg_dens <- fg_needed %>% 
  map(data_trans) %>% 
  reduce(left_join, by = c("ID", "Data")) %>% 
  select(Data, ID, everything()) %>% 
  pivot_longer(MeatPoultFish:Beverages, names_to = "Food", values_to = "Servings") %>% 
  mutate(Food = factor(Food, levels = fg_needed)) %>% 
  ggplot(aes(x = Servings, fill = Data, color = Data)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Food, scales = "free_y", ncol = 5) +
  theme(legend.position = "bottom") +
  labs(color = NULL, fill = NULL, y = "Density")

# pdf("Food group density plots.pdf", width = 9, height = 6)
fg_dens
# dev.off()

# Network plot of correlations --------------------------------------------

# Check % of zero intake
selected_foods <- c("Meat", "eggs", "Cheese", "FreshFruits", "Cereals", "Breads", 
                    "Grains", "Water", "Vegetables")

# Convert correlations to an undirected graph object
# Requires...
library(corrr)
library(ggraph)
library(igraph)

# For Food report data
fg_fr_cors <- fr_ea %>% 
  select(any_of(selected_foods)) %>% 
  correlate(method = "spearman") %>% 
  stretch() %>% 
  filter(abs(r) > .08) %>% 
  graph_from_data_frame(directed = FALSE)

# Plot
p1 <- ggraph(fg_fr_cors) +
  geom_edge_arc(aes(edge_alpha = abs(r), edge_width = abs(r), color = r), strength = .5) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-.51, .51), name = "Spearman r",
                              colors = c("firebrick4", "firebrick2", "white", "dodgerblue2", "dodgerblue4")) +
  geom_node_point(color = "lightgray", size = 8) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()

cairo_pdf("eaFR Corr Network.pdf", width = 6, height = 3.5)
p1
dev.off()

# For FFQ data
fg_ffq_cors <- ffq_ea %>% 
  select(any_of(selected_foods)) %>% 
  correlate(method = "spearman") %>% 
  stretch() %>% 
  filter(abs(r) > .14) %>% 
  graph_from_data_frame(directed = FALSE)

# Plot
p2 <- ggraph(fg_ffq_cors) +
  geom_edge_arc(aes(edge_alpha = abs(r), edge_width = abs(r), color = r), strength = .5) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-.51, .51), name = "Spearman r",
                              colors = c("firebrick4", "firebrick2", "white", "dodgerblue2", "dodgerblue4")) +
  geom_node_point(color = "lightgray", size = 8) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()

# cairo_pdf("eaFFQ Corr Network.pdf", width = 6, height = 3.5)
p2
# dev.off()

# fg_cors <- fr_subj_by_fg_wide %>% 
#   select(any_of(selected_foods)) %>% 
#   correlate(method = "spearman") %>% 
#   stretch() %>% 
#   filter(abs(r) > .1) %>% 
#   graph_from_data_frame(directed = FALSE)

# Plot
# p1 <- ggraph(fg_cors) +
#   geom_edge_arc(aes(edge_alpha = abs(r), edge_width = abs(r), color = r), strength = .5) +
#   guides(edge_alpha = "none", edge_width = "none") +
#   scale_edge_colour_gradientn(limits = c(-.51, .51), 
#                               colors = c("firebrick4", "firebrick2", "white", "dodgerblue2", "dodgerblue4")) +
#   geom_node_point(color = "lightgray", size = 8) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_graph() +
#   labs(title = "Network plot of correlations between food groups")
# p1
