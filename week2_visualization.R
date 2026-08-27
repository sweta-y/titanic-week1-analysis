# ============================================================
# Week 2 Task: Data Visualization and Insight Communication using R
# Dataset: Titanic Passenger Dataset (continued from Week 1)
# ============================================================

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# ---- Reapply Week 1 cleaning steps (recap) ----
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
mode_embarked <- names(sort(table(titanic$Embarked[titanic$Embarked != ""]), decreasing = TRUE))[1]
titanic$Embarked[titanic$Embarked == ""] <- mode_embarked
titanic$Cabin_Known <- ifelse(titanic$Cabin == "", 0, 1)
titanic$Cabin <- NULL
cap_val <- quantile(titanic$Fare, 0.99)
titanic$Fare_capped <- ifelse(titanic$Fare > cap_val, cap_val, titanic$Fare)
titanic$Pclass   <- factor(titanic$Pclass, levels = c(1,2,3), labels = c("1st","2nd","3rd"))
titanic$Survived <- factor(titanic$Survived, levels = c(0,1), labels = c("No","Yes"))
titanic$AgeGroup <- cut(titanic$Age, breaks = c(0,12,18,35,60,100),
                         labels = c("Child","Teen","Young Adult","Adult","Senior"))

# ============================================================
# Visualization 1: Bar Chart - Survival Count by Passenger Class
# Purpose: Compare categorical frequencies across groups
# ============================================================
png("plots_wk2/v1_bar_class_survival.png", width = 650, height = 480)
tab1 <- table(titanic$Pclass, titanic$Survived)
barplot(tab1, beside = TRUE, col = c("#4C72B0","#DD8452","#55A868"),
        main = "Survival Count by Passenger Class",
        xlab = "Survived", ylab = "Number of Passengers",
        legend.text = rownames(tab1), args.legend = list(title = "Class", x = "topright"))
dev.off()

# ============================================================
# Visualization 2: Stacked Bar Chart - Survival Proportion by Sex
# Purpose: Show proportional composition, not just raw counts
# ============================================================
png("plots_wk2/v2_stacked_sex_survival.png", width = 650, height = 480)
tab2 <- prop.table(table(titanic$Sex, titanic$Survived), margin = 1)
barplot(t(tab2), col = c("#DD8452","#55A868"),
        main = "Proportion of Survival by Sex",
        ylab = "Proportion", xlab = "Sex",
        legend.text = c("No","Yes"), args.legend = list(title = "Survived", x = "topright"))
dev.off()

# ============================================================
# Visualization 3: Histogram - Age Distribution with Density Curve
# Purpose: Show shape/spread of a continuous numeric variable
# ============================================================
png("plots_wk2/v3_hist_age_density.png", width = 650, height = 480)
hist(titanic$Age, breaks = 25, freq = FALSE, col = "#8172B2",
     main = "Age Distribution with Density Curve", xlab = "Age")
lines(density(titanic$Age), col = "darkred", lwd = 2)
dev.off()

# ============================================================
# Visualization 4: Boxplot - Fare Distribution by Passenger Class
# Purpose: Compare spread/outliers of a numeric variable across categories
# ============================================================
png("plots_wk2/v4_box_fare_class.png", width = 650, height = 480)
boxplot(Fare_capped ~ Pclass, data = titanic, col = c("gold","lightblue","salmon"),
        main = "Fare Distribution by Passenger Class (capped)",
        xlab = "Passenger Class", ylab = "Fare")
dev.off()

# ============================================================
# Visualization 5: Scatter Plot - Age vs Fare, colored by Survival
# Purpose: Explore relationship between two numeric vars + a category
# ============================================================
png("plots_wk2/v5_scatter_age_fare_survival.png", width = 650, height = 480)
plot(titanic$Age, titanic$Fare_capped,
     col = ifelse(titanic$Survived == "Yes", "#55A868", "#DD8452"),
     pch = 19, main = "Age vs Fare, Colored by Survival",
     xlab = "Age", ylab = "Fare (capped)")
legend("topright", legend = c("Survived","Did Not Survive"), col = c("#55A868","#DD8452"), pch = 19)
dev.off()

# ============================================================
# Visualization 6: Line Chart - Survival Rate by Age Group
# Purpose: Show a trend across an ordered category
# ============================================================
png("plots_wk2/v6_line_survival_agegroup.png", width = 650, height = 480)
rate_by_age <- tapply(titanic$Survived == "Yes", titanic$AgeGroup, mean, na.rm = TRUE)
plot(rate_by_age, type = "o", col = "#4C72B0", lwd = 2, pch = 16,
     xaxt = "n", main = "Survival Rate Trend Across Age Groups",
     xlab = "Age Group", ylab = "Survival Rate")
axis(1, at = 1:length(rate_by_age), labels = names(rate_by_age))
dev.off()

# ============================================================
# Visualization 7: Pie Chart - Port of Embarkation Share
# Purpose: Show simple part-to-whole composition
# ============================================================
png("plots_wk2/v7_pie_embarked.png", width = 600, height = 480)
tab_emb <- table(titanic$Embarked)
pct <- round(100 * tab_emb / sum(tab_emb), 1)
pie(tab_emb, labels = paste0(names(tab_emb), " (", pct, "%)"),
    col = c("#4C72B0","#DD8452","#55A868"),
    main = "Passenger Share by Port of Embarkation")
dev.off()

# ============================================================
# Visualization 8: Mosaic Plot - Class vs Survival (categorical x categorical)
# Purpose: Show relationship between two categorical variables at once
# ============================================================
png("plots_wk2/v8_mosaic_class_survival.png", width = 650, height = 480)
mosaicplot(table(titanic$Pclass, titanic$Survived), color = c("#DD8452","#55A868"),
           main = "Mosaic Plot: Class vs Survival", xlab = "Class", ylab = "Survived")
dev.off()

cat("All 8 visualizations saved to plots_wk2/\n")
