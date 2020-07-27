#---------------------------------------
# This script sets out to produce some
# statistical models
#
# NOTE: This script requires setup.R to
# have been run first
#---------------------------------------

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
  mutate(total_value = case_when(
    total_value > 100 & total_value != 101 & total_value != 120 ~ "Other Nats",  
    total_value == 101                                          ~ "Nat1",
    total_value == 120                                          ~ "Nat20",
    TRUE                                                        ~ "Other Totals")) %>%
  mutate(total_value = factor(total_value, levels = c("Nat1", "Nat20", "Other Nats",
                                                      "Other Totals")))

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

multinom.fit <- multinom(total_value ~ character, data = train)

# Checking the model

summary(multinom.fit)

# Exponentiate coefficients to get odds

exp(coef(multinom.fit))

# Graph it

plot(Effect("character", multinom.fit), multiline = T)

#----------
# ACCURACY
#----------

# Train set

train$predicted <- predict(multinom.fit, newdata = train, "class")

ctable <- table(train$total_value, train$predicted)

round((sum(diag(ctable))/sum(ctable))*100, 2)

# Test set

test$predicted <- predict(multinom.fit, newdata = test, "class")

ctable <- table(test$total_value, test$predicted)

round((sum(diag(ctable))/sum(ctable))*100, 2)

#---------------------AUTOCORRELATION MODELLING---------------------

# Prep data and fill in blanks

acf_data <- clean %>%
  filter(character %in% the_nein) %>%
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