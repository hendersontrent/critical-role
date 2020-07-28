#---------------------------------------
# This script sets out to produce some
# statistical models
#
# NOTE: This script requires setup.R to
# have been run first
#---------------------------------------

#----------------------------------------
# Author: Trent Henderson, 27 July 2020
#----------------------------------------

# Load data

load("data/clean.Rda")

if (!exists(keepers)) {
  keepers <- c("keepers", "clean")
} else {
  keepers <- union(keepers, "clean")
}

# Prep data

model_data <- clean %>%
  filter(character %in% the_nein) %>%
  mutate(character = case_when(
    character == "Nott" ~ "Veth/Nott",
    character == "Veth" ~ "Veth/Nott",
    TRUE                ~ character)) %>%
  mutate(total_value = case_when(
    total_value == 101                                          ~ "Nat1",
    total_value == 120                                          ~ "Nat20",
    TRUE                                                        ~ "Other Totals")) %>%
  mutate(total_value = factor(total_value, levels = c("Other Totals", "Nat1", "Nat20")))

# Split data into test and train sets

sample_size <- floor(0.8 * nrow(model_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(model_data)), size = sample_size)
train <- model_data[train_ind,]
test <- model_data[-train_ind,]

#---------------------PROBABILITY MODELLING-------------------------

#----------
# TRAINING
#----------

# Training the multinomial logit model

model <- nnet::multinom(total_value ~ character, data = train)

# Checking the model

summary(model)

# Exponentiate coefficients to get odds

exp(coef(model))

# Graph it

theme_set(theme_sjplot())

the_mod <- plot_model(model, sort.est = TRUE, transform = "plogis", show.values = TRUE, value.offset = .3,
           title = "Multinomial logistic regression model of roll value by character (relative to other value rolls)", 
           colors = c("#FD62AD"))
print(the_mod)

plot(Effect("character", model), multiline = T)

#----------
# ACCURACY
#----------

# Train set

train$predicted <- predict(model, newdata = train, "class")

ctable <- table(train$total_value, train$predicted)

round((sum(diag(ctable))/sum(ctable))*100, 2)

# Test set

test$predicted <- predict(model, newdata = test, "class")

ctable <- table(test$total_value, test$predicted)

round((sum(diag(ctable))/sum(ctable))*100, 2)

#---------------------AUTOCORRELATION MODELLING---------------------

# Prep data and fill in blanks

acf_data <- clean %>%
  filter(character %in% the_nein) %>%
  mutate(character = case_when(
    character == "Nott" ~ "Veth/Nott",
    character == "Veth" ~ "Veth/Nott",
    TRUE                ~ character)) %>%
  filter(total_value == 120) %>%
  group_by(episode) %>%
  summarise(counts = n()) %>%
  ungroup()

acf_miss <- data.frame(episode = c(14,27,33,48,54,61,89,92,97),
                       counts = c(0,0,0,0,0,0,0,0,0))

acf_final <- bind_rows(acf_data, acf_miss) %>%
  arrange(episode)

# Make ACF visualisation

p1 <- ggAcf(acf_final$counts) +
  labs(title = "Autocorrelation Function of Nat20 Wildemount rolls",
       x = "Lag",
       y = "ACF") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p1)

#---------------------EXPORTS---------------------------------------

CairoPNG("output/statistical-model.png", 1000, 600)
print(the_mod)
dev.off()
