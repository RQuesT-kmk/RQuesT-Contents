
# About --------------------------------------------------------------------

#' Name: Data analysis of simulated data of general social survey
#' Objective: To estimate the odds of happiness by marital status
#' Date: 2026 Jan 17

# LIbraries ---------------------------------------------------------------

pacman::p_load(tidyverse,
               finalfit,
               rio,
               labelled)

# Load data ---------------------------------------------------------------

rm(list = ls())
df <- read_rds("general_social_survey_MarriedvsHappy_2022_simulated.rds")
glimpse(df)
summary(df)
df_revised <- df |> 
    mutate(happy = if_else(happiness == "Not Too Happy", 0,1)) |> 
    rename(married = marital_status, child = having_children) |> 
    mutate(child = if_else(child == "Yes","Have children","Have no children",
                           ptype = factor(levels = c("Have no children","Have children")))) |> 
    mutate(happiness = str_to_sentence(as.character(happiness))) |> 
    mutate(happiness = factor(happiness, 
                              ordered = TRUE)) |> 
    mutate(married = factor(married,
                            levels = c("Unmarried","Married"),
                            ordered = F),
           sex = factor(sex,
                        levels = c("Male","Female")),
           happy = factor(happy,
                          levels = c(0,1),
                          labels = c("Not happy", "Happy")))


png(filename = "Men's happiness bar.png", units = "px", 
    height = 1080, width = 1080)
df_revised |> 
    filter(sex == "Male") |> 
    ggplot()+
    geom_bar(mapping = aes(x = child, fill = happiness), position = "fill")+
    scale_fill_manual(values = c("red","skyblue","forestgreen"), name = "Happiness level")+
    scale_y_continuous(labels = scales::percent_format(), name = "Percentage")+
    scale_x_discrete(name = "")+
    theme_classic()+
    facet_wrap(~married, nrow =1)+
    labs(title = "Men's happiness by marital status and Children, Age-18-55",
         caption = "Source:Simulated data of General social survey 2022")+
    theme(text = element_text(size = 20),
          legend.position = "bottom")
dev.off()


png(filename = "Women's happiness bar.png", units = "px", 
    height = 1080, width = 1080)
df_revised |> 
    filter(sex == "Female") |> 
    ggplot()+
    geom_bar(mapping = aes(x = child, fill = happiness), position = "fill")+
    scale_fill_manual(values = c("red","skyblue","forestgreen"), name = "Happiness level")+
    scale_y_continuous(labels = scales::percent_format(), name = "Percentage")+
    scale_x_discrete(name = "")+
    theme_classic()+
    facet_wrap(~married, nrow =1)+
    labs(title = "Women's happiness by marital status and Children, Age-18-55",
         caption = "Source:Simulated data of General social survey 2022")+
    theme(text = element_text(size = 20),
          legend.position = "bottom")
dev.off()

glimpse(df_revised)
logit1 <- glm(happy ~ married, 
              data = df_revised, 
              family = binomial(link = "logit"))
summary(logit1)


logit2 <- glm(happy ~ married+sex+child, 
              data = df_revised, 
              family = binomial(link = "logit"))
summary(logit2)
mStats::logit(logit2)

finalfit.glm(.data = df_revised,
                  dependent = "happy",
                  explanatory = c("married","sex","child")) |> 
             knitr::kable(row.names = F)

png("orplot.png",width = 480,height = 480)
or_plot(.data = df_revised,
        dependent = "happy",
        explanatory = c("married","sex","child"))
dev.off()


new_data <- expand.grid(
    married = c("Unmarried", "Married"),
    sex = c("Male","Female"),
    child = c("Have children", "Have no children")
)

preds <- predict(logit2, newdata = new_data, type = "link", se.fit = TRUE)
new_data <- new_data |> 
    mutate(
        # Convert log-odds to probability
        prob  = plogis(preds$fit),
        # Calculate 95% Confidence Intervals
        lower = plogis(preds$fit - (1.96 * preds$se.fit)),
        upper = plogis(preds$fit + (1.96 * preds$se.fit))
    )

png("probability of hapiness.png")
ggplot(new_data, aes(x = married, y = prob, fill = child)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1),
                       breaks = seq(0,1,0.1)) +
    scale_fill_manual(values = c("skyblue", "forestgreen")) +
    labs(
        title = "Probability of Being 'Happy' (Women)",
        subtitle = "Based on Logistic Regression Model (logit2)",
        x = "Marital Status",
        y = "Predicted Probability (%)",
        fill = "Parental Status"
    ) +
    theme_classic()+
    facet_wrap(~sex)
dev.off()



# 1. Create a summary data frame for the Odds Ratio
# Assuming your model provided these values (Estimate, Lower CI, Upper CI)
or_data <- data.frame(
    variable = "Married vs Unmarried",
    odds_ratio = 1.89,
    lower_ci = 1.71, # Example CI
    upper_ci = 2.08  # Example CI
)

# 2. Create the Forest Plot
png("odds of happiness.png", width = 1080, height = 1080)
ggplot(or_data, aes(x = odds_ratio, y = variable)) +
    # Add a vertical reference line at 1.0 (The "No Effect" line)
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    
    # Add the error bars (Confidence Interval)
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, size = 1) +
    
    # Add the point (The Odds Ratio)
    geom_point(size = 4, color = "darkblue") +
    
    # Labeling
    scale_x_continuous(breaks = seq(0, 2.5, 0.5), limits = c(0, 2.5)) +
    labs(
        title = "Married people have 89% higher odds of being happy \ncompared to unmarried people, holding other variables constant.",
        x = "Odds Ratio (log scale usually, but shown linearly here)",
        y = ""
    ) +
    theme_minimal() +
    # Add text label for the exact OR value
    annotate("text", x = 1.89, y = 1.1, label = "OR = 1.89", fontface = "bold", size =8)+
    theme(text = element_text(size = 24))
dev.off()
