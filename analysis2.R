library(ggpubr)
library(readxl)
library(mgcv)
library(afex)
library(emmeans)
library(dplyr)
library(tidyr)
library(car)
library(ez)
library(ggplot2)
library(multcomp)


cape <- read.csv("CAPE5.csv")
consent <- read.csv("Consent5.csv")
sifi_embodiment_long3 <- read.csv("sifi_embodiment_long3.csv")


sifi_embodiment_long3$ID <- as.factor(sifi_embodiment_long3$ID)
sifi_embodiment_long3$Stimulus_Type <- as.factor(sifi_embodiment_long3$Stimulus_Type)
sifi_embodiment_long3$Perspective <- as.factor(sifi_embodiment_long3$Perspective)
sifi_embodiment_long3$Group <- as.factor(sifi_embodiment_long3$Group)
anova_result <- ezANOVA(
  data = sifi_embodiment_long3,
  dv = Embodiment_Score,
  wid = ID,
  within = .(Stimulus_Type, Perspective),
  between = Group,
  type = 3,  # Type III sums of squares
  detailed = TRUE
)
print(anova_result)

# Calculate means and standard errors for each combination of factors
summary_data <- sifi_embodiment_long3 %>%
  group_by(Group, Stimulus_Type, Perspective) %>%
  summarize(
    mean_Embodiment_Score = mean(Embodiment_Score, na.rm = TRUE),
    sd_Embodiment_Score = sd(Embodiment_Score, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se_Embodiment_Score = sd_Embodiment_Score / sqrt(n))
group_summary <- summary_data %>%
  group_by(Group) %>%
  summarize(
    mean_Embodiment_Score = mean(mean_Embodiment_Score),
    se_Embodiment_Score = sqrt(sum((se_Embodiment_Score)^2 / n()))
  )
ggplot(group_summary, aes(x = Group, y = mean_Embodiment_Score, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_Embodiment_Score - se_Embodiment_Score, ymax = mean_Embodiment_Score + se_Embodiment_Score), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Main Effect of Group", x = "Group", y = "Mean Embodiment Score") +
  theme_minimal()
#post-hoc
model <- aov(Embodiment_Score ~ Group  , data = sifi_embodiment_long3)
post_hoc_group <- glht(model, linfct = mcp(Group = "Tukey"))
summary(post_hoc_group)

stimulus_summary <- summary_data %>%
  group_by(Stimulus_Type) %>%
  summarize(
    mean_Embodiment_Score = mean(mean_Embodiment_Score),
    se_Embodiment_Score = sqrt(sum((se_Embodiment_Score)^2 / n()))
  )

# Main effects plot for Stimulus_Type
ggplot(stimulus_summary, aes(x = Stimulus_Type, y = mean_Embodiment_Score, fill = Stimulus_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_Embodiment_Score - se_Embodiment_Score, ymax = mean_Embodiment_Score + se_Embodiment_Score), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Main Effect of Stimulus_Type", x = "Stimulus Type", y = "Mean Embodiment Score") +
  theme_minimal()
anova_model <- aov(Embodiment_Score ~ Perspective * Stimulus_Type * Group, data = sifi_embodiment_long3)
summary(anova_model)
#post-hoc
model <- aov(Embodiment_Score ~ Stimulus_Type  , data = sifi_embodiment_long3)
post_hoc_group <- glht(model, linfct = mcp(Stimulus_Type = "Tukey"))
summary(post_hoc_group)
#
model <- aov(Embodiment_Score ~ Group*Stimulus_Type*Perspective  , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Group * Stimulus_Type*Perspective)
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Summarize for Perspective only
perspective_summary <- summary_data %>%
  group_by(Perspective) %>%
  summarize(
    mean_Embodiment_Score = mean(mean_Embodiment_Score),
    se_Embodiment_Score = sqrt(sum((se_Embodiment_Score)^2 / n()))
  )
# Main effects plot for Perspective
ggplot(perspective_summary, aes(x = Perspective, y = mean_Embodiment_Score, fill = Perspective)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_Embodiment_Score - se_Embodiment_Score, ymax = mean_Embodiment_Score + se_Embodiment_Score), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Main Effect of Perspective", x = "Perspective", y = "Mean Embodiment Score") +
  theme_minimal()
#post-hoc
model <- aov(Embodiment_Score ~ Perspective  , data = sifi_embodiment_long3)
post_hoc_group <- glht(model, linfct = mcp(Perspective = "Tukey"))
summary(post_hoc_group)

interaction_summary <- summary_data %>%
  group_by(Group, Stimulus_Type, Perspective) %>%
  summarize(
    mean_Embodiment_Score = mean(mean_Embodiment_Score),
    se_Embodiment_Score = sqrt(sum((se_Embodiment_Score)^2 / n()))
  )

# Interaction plot
ggplot(interaction_summary, aes(x = Interaction, y = mean_Embodiment_Score, fill = Interaction)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_Embodiment_Score - se_Embodiment_Score, ymax = mean_Embodiment_Score + se_Embodiment_Score), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Interaction Effects", x = "Interaction", y = "Mean Embodiment Score") +
  theme_minimal()

# Boxplot for Group:Stimulus_Type
ggplot(sifi_embodiment_long3, aes(x = Stimulus_Type, y = Embodiment_Score, fill = Group)) +
  geom_boxplot() +
  labs(title = "Interaction Effect: Group vs. Stimulus Type", x = "Stimulus Type", y = "Embodiment Score") +
  theme_minimal()
#post hoc
model <- aov(Embodiment_Score ~ Group*Stimulus_Type  , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Group * Stimulus_Type)
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))



# Boxplot for Group:Perspective
ggplot(sifi_embodiment_long3, aes(x = Perspective, y = Embodiment_Score, fill = Group)) +
  geom_boxplot() +
  labs(title = "Interaction Effect: Group vs. Perspective", x = "Perspective", y = "Embodiment Score") +
  theme_minimal()
#post hoc
model <- aov(Embodiment_Score ~ Group*Perspective  , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Group * Perspective)
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Boxplot for Stimulus_Type:Perspective
ggplot(sifi_embodiment_long3, aes(x = Stimulus_Type, y = Embodiment_Score, fill = Perspective)) +
  geom_boxplot() +
  labs(title = "Interaction Effect: Stimulus Type vs. Perspective", x = "Stimulus Type", y = "Embodiment Score") +
  theme_minimal()
#post hoc
model <- aov(Embodiment_Score ~ Perspective * Stimulus_Type , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Perspective * Stimulus_Type  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Boxplot for Group:Stimulus_Type:Perspective
ggplot(sifi_embodiment_long3, aes(x = Stimulus_Type, y = Embodiment_Score, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Perspective) +
  labs(title = "Interaction Effect: Group vs. Stimulus Type with Perspective", x = "Stimulus Type", y = "Embodiment Score") +
  theme_minimal()

#post hoc
model <- aov(Embodiment_Score ~ Perspective * Stimulus_Type*Group , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Perspective * Stimulus_Type*Group  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))



#SYNCHRONOUS
synchronous_subset <- sifi_embodiment_long3 %>%
  filter(Stimulus_Type == "Synchronous")

model_sync  <- lm(Embodiment_Score ~ Perspective*Group, data = synchronous_subset)
ggqqplot(residuals(model_sync))
shapiro.test(residuals(model_sync))

anova_result <- ezANOVA(
  data = synchronous_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(Perspective),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result)

model <- aov(Embodiment_Score ~ Group*Perspective , data = synchronous_subset)
emm <- emmeans(model, ~ Group*Perspective  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))
# Plot
synchronous_subset$Embodiment_Score <- as.numeric(synchronous_subset$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(synchronous_subset, aes(x = Perspective, y = Embodiment_Score, fill = Perspective)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "Perspective", y = "Embodiment Score", fill = "Perspective") +
  ggtitle("Boxplot of Embodiment Score by Perspective (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)


#CAPE
mean_cape_score <- mean(synchronous_subset$cape_score, na.rm = TRUE)
synchronous_subset$cape_group <- ifelse(synchronous_subset$cape_score > mean_cape_score, "High cape", "Low cape")
synchronous_subset$cape_group <- as.factor(synchronous_subset$cape_group)

model_sync  <- lm(Embodiment_Score ~ cape_group*Group, data = synchronous_subset)
ggqqplot(residuals(model_sync))
shapiro.test(residuals(model_sync))
View(synchronous_subset)

anova_result <- ezANOVA(
  data = synchronous_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(cape_group),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result)


model <- aov(Embodiment_Score ~ Group*cape_group , data = synchronous_subset)
emm <- emmeans(model, ~ Group*cape_group  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Plot
synchronous_subset$Embodiment_Score <- as.numeric(synchronous_subset$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(synchronous_subset, aes(x = cape_group, y = Embodiment_Score, fill = cape_group)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "cape_group", y = "Embodiment Score", fill = "cape_group") +
  ggtitle("Boxplot of Embodiment Score by cape_group (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)

#ASYNCHRONOUS
asynchronous_subset <- sifi_embodiment_long3 %>%
  filter(Stimulus_Type == "Asynchronous")

model_async  <- lm(Embodiment_Score ~ Perspective*Group, data = synchronous_subset)
ggqqplot(residuals(model_async))
shapiro.test(residuals(model_async))

anova_result_a <- ezANOVA(
  data = asynchronous_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(Perspective),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result_a)

model <- aov(Embodiment_Score ~ Group*Perspective , data = asynchronous_subset)
emm <- emmeans(model, ~ Group*Perspective  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Plot
asynchronous_subset$Embodiment_Score <- as.numeric(asynchronous_subset$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(asynchronous_subset, aes(x = Perspective, y = Embodiment_Score, fill = Perspective)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "Perspective", y = "Embodiment Score", fill = "Perspective") +
  ggtitle("Boxplot of Embodiment Score by Perspective (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)





#1PP
Fpp_subset <- sifi_embodiment_long3 %>%
  filter(Perspective == "First Person")

model_fpp  <- lm(Embodiment_Score ~ Stimulus_Type*Group, data = Fpp_subset)
ggqqplot(residuals(model_fpp))
shapiro.test(residuals(model_fpp))

anova_result_a <- ezANOVA(
  data = Fpp_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(Stimulus_Type),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result_a)

model <- aov(Embodiment_Score ~ Group*Stimulus_Type , data = Fpp_subset)
emm <- emmeans(model, ~ Group*Stimulus_Type  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Plot
Fpp_subset$Embodiment_Score <- as.numeric(Fpp_subset$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(Fpp_subset, aes(x = Perspective, y = Embodiment_Score, fill = Perspective)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "Perspective", y = "Embodiment Score", fill = "Perspective") +
  ggtitle("Boxplot of Embodiment Score by Perspective (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)


#CAPE
asynchronous_subset$Embodiment_Score <- as.numeric(asynchronous_subset$Embodiment_Score)
mean_cape_score <- mean(asynchronous_subset$cape_score, na.rm = TRUE)
asynchronous_subset$cape_group <- ifelse(asynchronous_subset$cape_score > mean_cape_score, "High cape", "Low cape")
asynchronous_subset$cape_group <- as.factor(asynchronous_subset$cape_group)

model_sync  <- lm(Embodiment_Score ~ cape_group*Group, data = asynchronous_subset)
ggqqplot(residuals(model_sync))
shapiro.test(residuals(model_sync))
View(asynchronous_subset)

anova_result <- ezANOVA(
  data = asynchronous_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(cape_group),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result)


model <- aov(Embodiment_Score ~ Group*cape_group , data = asynchronous_subset)
emm <- emmeans(model, ~ Group*cape_group  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

#不管同步还是异步
sifi_embodiment_long3$Embodiment_Score <- as.numeric(sifi_embodiment_long3$Embodiment_Score)
mean_cape_score <- mean(sifi_embodiment_long3$cape_score, na.rm = TRUE)
sifi_embodiment_long3$cape_group <- ifelse(sifi_embodiment_long3$cape_score > mean_cape_score, "High cape", "Low cape")
sifi_embodiment_long3$cape_group <- as.factor(sifi_embodiment_long3$cape_group)

model_sync  <- lm(Embodiment_Score ~ cape_group*Group, data = sifi_embodiment_long3)
ggqqplot(residuals(model_sync))
shapiro.test(residuals(model_sync))


anova_result <- ezANOVA(
  data = sifi_embodiment_long3,
  dv = Embodiment_Score,
  wid = ID,
  within = .(cape_group),
  between = Group,
  type = 3,
  detailed = TRUE
)
print(anova_result)


model <- aov(Embodiment_Score ~ Group*cape_group , data = sifi_embodiment_long3)
emm <- emmeans(model, ~ Group*cape_group  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))



# Plot
sifi_embodiment_long3$Embodiment_Score <- as.numeric(sifi_embodiment_long3$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(sifi_embodiment_long3, aes(x = cape_group, y = Embodiment_Score, fill = cape_group)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "cape_group", y = "Embodiment Score", fill = "cape_group") +
  ggtitle("Boxplot of Embodiment Score by cape_group (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)
#perspective

model_sync  <- lm(Embodiment_Score ~ cape_group*Perspective, data = synchronous_subset)
ggqqplot(residuals(model_sync))
shapiro.test(residuals(model_sync))


anova_result <- ezANOVA(
  data = synchronous_subset,
  dv = Embodiment_Score,
  wid = ID,
  within = .(Perspective),
  between = cape_group,
  type = 3,
  detailed = TRUE
)
print(anova_result)


model <- aov(Embodiment_Score ~ Perspective*cape_group , data = synchronous_subset)
emm <- emmeans(model, ~ Perspective*cape_group  )
pairwise_comparisons <- pairs(emm)
print(summary(pairwise_comparisons))

# Plot
asynchronous_subset$Embodiment_Score <- as.numeric(asynchronous_subset$Embodiment_Score)
my_colors <- c("#E41A1C", "#377EB8")
boxplot <- ggplot(asynchronous_subset, aes(x = cape_group, y = Embodiment_Score, fill = cape_group)) +
  geom_boxplot(color = "black", alpha = 0.7) +  # Add black borders and reduce fill transparency
  scale_fill_manual(values = my_colors) +  # Use custom color palette
  labs(x = "cape_group", y = "Embodiment Score", fill = "cape_group") +
  ggtitle("Boxplot of Embodiment Score by cape_group (Faceted by Group)") +
  facet_wrap(~ Group, nrow = 1) +  # Wrap facets by Group in a single row
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
print(boxplot)
