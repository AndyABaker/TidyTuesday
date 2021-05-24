library(tidytuesdayR)
library(tidyverse)
library(showtext)
font_add_google("Rubik")
showtext_auto()
library(tidymodels)
library(textrecipes)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
survey <- tuesdata$survey %>% 
  filter(currency == "USD") %>% 
  mutate(
    pay = annual_salary + as.numeric(other_monetary_comp),
    gender = factor(gender)
  ) %>% 
  select(pay, gender, industry, job_title) %>% 
  filter(!is.na(pay))

## Gender splits
survey_men <- subset(survey, gender == "Man")
survey_women <- subset(survey, gender == "Woman")
# Removed "other" as sample too small


# Model -------------------------------------------------------------------
# Lots of code taken this excellent resource: https://smltar.com/

run_model <- function(data) {
  ## Splits for cross validation
  set.seed(1234)
  s_split <- data %>%
    initial_split()
  
  s_train <- training(s_split)
  s_test <- testing(s_split)
  s_folds <- vfold_cv(s_train)
  
  
  ## Preprocess data
  s_rec <- recipe(pay ~ job_title + industry, data = s_train) %>%
    step_tokenize(job_title, industry, token = "ngrams", 
                  options = list(n = 5, n_min = 1)) %>%
    step_tokenfilter(job_title, industry, max_tokens = 1e3) %>%
    step_tfidf(job_title, industry) %>%
    step_normalize(all_predictors())
  
  
  ## Model specification
  svm_spec <- svm_linear() %>%
    set_mode("regression") %>%
    set_engine("LiblineaR")
  
  
  ## Model workflow
  s_wf <- workflow() %>%
    add_recipe(s_rec) %>% 
    add_model(svm_spec)
  
  
  ## Fit to training data
  # svm_fit <- s_wf %>%
  #   fit(data = s_train)
  
  
  ## See training fit coefficients
  # svm_fit %>%
  #   pull_workflow_fit() %>%
  #   tidy() %>%
  #   arrange(-estimate)
  
  
  ## Model evaluation
  set.seed(123)
  svm_rs <- fit_resamples(
    s_wf,
    s_folds,
    control = control_resamples(save_pred = TRUE)
  )
  #collect_metrics(svm_rs)
  
  
  final_fitted <- last_fit(
    s_wf,
    s_split
  )
  #collect_metrics(final_fitted)
  final_fitted
}

model_men <- run_model(survey_men)
model_women <- run_model(survey_women)

fit_men <- pull_workflow_fit(model_men$.workflow[[1]]) %>% 
  tidy() %>% 
  mutate(gender = "Men")
fit_women <- pull_workflow_fit(model_women$.workflow[[1]]) %>% 
  tidy() %>% 
  mutate(gender = "Women")


# Visualisation -----------------------------------------------------------

p_men <- fit_men %>%
  filter(term != "Bias") %>%
  group_by(sign = estimate > 0) %>%
  slice_max(abs(estimate), n = 25) %>%
  ungroup() %>%
  mutate(
    term = str_remove(term, "tfidf_job_title_"),
    term = str_replace(term, "tfidf_industry_", 
                       paste0("Industry: ")),
    sign = if_else(sign, "Higher pay for men", 
                   "Lower pay for men")
  ) %>%
  ggplot(aes(abs(estimate), fct_reorder(term, abs(estimate)), fill = sign)) +
  geom_col(alpha = 0.8, show.legend = FALSE) + 
  scale_fill_manual(values = c("#118392", "#19B3C8")) + 
  facet_wrap(vars(sign), ncol = 1, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        text = element_text(family = "Rubik", size = 40),
        strip.text = element_text(face = "bold", size = 40),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(lineheight = 0.2, size = 40),
        plot.margin = margin(10, 10, 10, 10))

p_women <- fit_women %>%
  filter(term != "Bias") %>%
  group_by(sign = estimate > 0) %>%
  slice_max(abs(estimate), n = 25) %>%
  ungroup() %>%
  mutate(
    term = str_remove(term, "tfidf_job_title_"),
    term = str_replace(term, "tfidf_industry_", 
                       paste0("Industry: ")),
    sign = if_else(sign, "Higher pay for women", 
                   "Lower pay for women")
  ) %>%
  ggplot(aes(abs(estimate), fct_reorder(term, abs(estimate)), fill = sign)) +
  geom_col(alpha = 0.8, show.legend = FALSE) + 
  scale_fill_manual(values = c("#4D194D", "#8080B3")) + 
  facet_wrap(vars(sign), ncol = 1, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        text = element_text(family = "Rubik", size = 40),
        strip.text = element_text(face = "bold", size = 40),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(lineheight = 0.2, size = 40),
        plot.margin = margin(10, 10, 10, 10))

p <- (p_men | p_women) + 
  plot_annotation(
    title = "Job titles and industries most predictive of pay in managers",
    caption = "For words and phrases in job titles and industries in the USA.
    The coefficients from the linear SVM are on the x-axis.
    Source: Ask a Manager | Visualisation: @Andy_A_Baker") & 
  theme(plot.title = element_text(family = "Rubik", face = "bold", 
                                  size = 85, hjust = 0.5),
        plot.caption = element_text(family = "Rubik", 
                                    size = 30, lineheight = 0.3))
p

ggsave(dpi = 300, width = 14, height = 10, units = "in", 
       filename = "2021_week21_askamanagersalarysurvey.jpeg", device = "jpeg")

