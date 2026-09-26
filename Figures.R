rm(list = ls())

library(tidyverse)
library(nlme)
library(sjPlot)
library(arsenal)
library(survival)
library(survminer)
library(grid)
library(gridExtra)
library(car)
library(DT)

# Project-relative output directory.
output_dir <- file.path("output", "Thea")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# TMAO - NFL  -------------------------------------------------------------
dd <- read_csv("Baseline.csv")

dd <- dd %>%
  mutate(tmao_q = factor(ntile(tmao, 5)),
         tmao_log = log1p(tmao))

#### Boxplot TMAO quintiles and NFL ####
ddq <- dd %>%
  pivot_longer(cols=c(tmao_q),
               names_to = "TMAO",values_to = "Quintiles")%>%
  pivot_longer(cols = c(nfl),
               names_to = "nfl",values_to = "Volumes")

ddq$Quintiles <- factor(ddq$Quintiles, 
                        levels = c("1", "2", "3", "4", '5'))
ddq <- ddq %>%
  filter(!is.na(Quintiles)) %>%
  filter(!is.na(Volumes))

a<- ggplot(ddq, aes(x=Quintiles,y=Volumes,fill=Quintiles))+
  geom_boxplot()+
  xlab("") +
  ylab("") +
  ggtitle("") +
  scale_fill_brewer(palette="YlOrRd") +
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  labs(
    x = "TMAO Quintiles",
    y = "Neurofilament level",
    title = ""
  ) +
  coord_cartesian(ylim = c(0, 200))
  
a

ggsave(filename = "output/Thea/TMAO-NFL Boxplot.jpg", height = 4, width = 6, 
       plot =a, quality = 100)

#### Forest plot ####
##### Model #####
m1 <- lme(nfl ~ tmao_q + age.bl + pat.sex,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

m2 <- lme(nfl ~ tmao_q + age.bl + pat.sex +
            bmi + prev.hypertonie,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

m3 <- lme(nfl ~ tmao_q + age.bl + pat.sex +
            bmi + prev.hypertonie
          + prev.coronary.heart.disease + prev.stroke.tia,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

m4a <- lme(nfl ~ tmao_q + age.bl + pat.sex +
            bmi + prev.hypertonie
          + prev.coronary.heart.disease + prev.stroke.tia
          + creatinine.gfr,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

m4b <- lme(nfl ~ tmao_q + age.bl + pat.sex +
            bmi + prev.hypertonie
          + prev.coronary.heart.disease + prev.stroke.tia
          + prev.diabetes ,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

m5 <- lme(nfl ~ tmao_q + age.bl + pat.sex +
            bmi + prev.hypertonie
          + prev.coronary.heart.disease + prev.stroke.tia
          + prev.diabetes + creatinine.gfr,
          random = ~1 | center, data=dd,
          na.action=na.exclude)

tab_model(m1, m2, m3, m4a, m4b, m5, auto.label = TRUE, digits = 5)

##### Visualization #####
tmao_nfl_fp <- read_csv("tmao nfl fp.csv")

tmao_nfl_fp$model <- factor(tmao_nfl_fp$model,
                            levels = c("Model 5","Model 4B","Model 4A", "Model 3", "Model 2", "Model 1"))

tmao_nfl_fp$quintile <- factor(tmao_nfl_fp$quintile,
                               levels = c("Q5", "Q4", "Q3", "Q2"))

# Define custom colors for the quintiles
custom_colors <- c(
  "Q5" = "indianred4",
  "Q4" = "indianred2",
  "Q3" = "orange2",
  "Q2" = "goldenrod1")

a<-ggplot(tmao_nfl_fp, aes(x = mean, y = model, color = quintile)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Forest plot for association of TMAO and NFL",
       x = "Beta-coefficients",
       y = "",
       color = "Quintiles") +
  scale_color_manual(values = custom_colors) +
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),        
    axis.title = element_text(size = 14)) +
  theme(panel.grid.major.y = element_blank())

a

ggsave(filename = "output/Thea/TMAO-NFL Forest plot.jpg", height = 5, width = 6, 
       plot =a, quality = 100)

ggsave(
  filename = "output/Thea/TMAO-NFL Forest plot-new.jpg",
  plot = a,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300,
  quality = 100
)


# TMAO - Hippocampus ------------------------------------------------------

#### Boxplot ####
ddq <- dd %>%
  pivot_longer(cols=c(tmao_q),
               names_to = "TMAO",values_to = "Quintiles")%>%
  pivot_longer(cols = c(HVraw, AI, LHV, RHV, sbtiv.vol),
               names_to = "HV",values_to = "Volumes")

ddq$Quintiles <- factor(ddq$Quintiles, 
                        levels = c("1", "2", "3", "4", '5'))

ddq <- ddq %>%
  filter(!is.na(Quintiles)) %>%
  filter(!is.na(Volumes))

hvraw <- ddq %>%
  filter(HV == "HVraw")

a<- ggplot(hvraw, aes(x=Quintiles,y=Volumes,fill=Quintiles))+
  geom_boxplot()+
  xlab("TMAO Quintiles")+
  ylab("Bilateral hippocampal volume")+
  scale_fill_brewer(palette="YlOrRd") +
  theme_bw()+
  theme(
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a

ggsave(filename = "output/Thea/TMAO-HV Boxplot.jpg", height = 4, width = 6, 
       plot =a, quality = 100)

#### Forest plot ####
TMAO_HV <- read_csv("TMAO HV.csv")

TMAO_HV$model <- factor(TMAO_HV$model,
                            levels = c("Model 5","Model 4B","Model 4A", "Model 3", "Model 2", "Model 1"))

TMAO_HV$quintile <- factor(TMAO_HV$quintile,
                               levels = c("Q5", "Q4", "Q3", "Q2"))

# Define custom colors for the quintiles
custom_colors <- c(
  "Q5" = "indianred4",
  "Q4" = "indianred2",
  "Q3" = "orange2",
  "Q2" = "goldenrod1")

a<-ggplot(TMAO_HV, aes(x = mean, y = model, color = quintile)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Forest plot for association of TMAO and bilateral hippocampal volume",
       x = "Beta-coefficients",
       y = "",
       color = "Quintiles") +
  scale_color_manual(values = custom_colors) +
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),        
    axis.title = element_text(size = 14)) +
  theme(panel.grid.major.y = element_blank())

a

ggsave(filename = "output/Thea/TMAO-HV Forest plot-new.jpg", height = 5, width = 7, 
       plot =a, quality = 100)

ggsave(
  filename = "output/Thea/TMAO-HV Forest plot-new.jpg",
  plot = a,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300,
  quality = 100
)


# Scatterplot - Cognitive function over time ------------------------------
dd <- read_csv("Fu5.csv")
dd$tmao_log <- log(dd$tmao + 1)
dd$tmao_q <- as.factor(dd$tmao_q)
dd$center <- as.factor(dd$center)

dd <- dd %>%
  mutate(pat.id = as.factor(pat.id),
         visit.name = as.factor(visit.name),
         pat.sex = as.factor(pat.sex),
         vhf.typ.aktuell.bl = as.factor(vhf.typ.aktuell.bl),
         rauchen = as.factor(rauchen),
         stop.grund = as.factor(stop.grund),
         prev.hypertonie = as.factor(prev.hypertonie),
         prev.diabetes = as.factor(prev.diabetes),
         prev.stroke.tia = as.factor(prev.stroke.tia),
         prev.coronary.heart.disease = as.factor(prev.coronary.heart.disease),
         sport = as.factor(sport),
         highest.education.level.groups = as.factor(highest.education.level.groups),
         type_of_lesion = as.factor(type_of_lesion),
         dementia = as.factor(dementia),
         cog_impairment = as.factor(cog_impairment),
         MoCA_change = as.factor(MoCA_change),
         CoCo_change = as.factor(CoCo_change),
         DSST_change = as.factor(DSST_change),
         SF_change = as.factor(SF_change),
         TMTA_change = as.factor(TMTA_change),
         TMTB_change = as.factor(TMTB_change))

#### Plot ####
m <- lme(MoCA ~ splines::ns(tmao, df = 3) ,
         random = ~1 | center,
         data = dd,
         na.action = na.exclude)

new_data <- data.frame(
  tmao = seq(min(dd$tmao, na.rm = TRUE),
              max(dd$tmao, na.rm = TRUE), 
              length.out = 100),
  center = factor(levels(dd$center)[1], levels = levels(dd$center))
)

# Build the design matrix for the fixed effects exactly as in the model
X <- model.matrix(~ splines::ns(tmao, df = 3),
                  data = new_data)

# Extract fixed effects estimates and the variance-covariance matrix from the model
beta <- fixef(m)
vc <- vcov(m)

# Compute fitted values (fixed effects only)
new_data$fit <- X %*% beta

# Compute the standard errors for the fitted values
new_data$se <- sqrt(diag(X %*% vc %*% t(X)))

# Calculate the 95% confidence interval
z <- qnorm(0.975)
new_data$lower <- new_data$fit - z * new_data$se
new_data$upper <- new_data$fit + z * new_data$se

# Plot the fitted spline curve with a confidence ribbon using ggplot2
a<-ggplot(new_data, aes(x = tmao, y = fit)) +
  geom_point(data = dd, aes(x = tmao, y = MoCA), 
             color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "indianred4", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "indianred3", alpha = 0.2) +
  labs(title = "Scatter plot of MoCA over time and TMAO level",
       x = "TMAO level",
       y = "MoCA score over time") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a
ggsave(filename = "output/Thea/TMAO MoCA overtime.jpg", height = 6, width = 8, 
       plot = a, quality = 100)

##### Baseline #####
dd1 <- dd %>%
  filter(visit.name == 'Baseline')

m <- lme(MoCA ~ splines::ns(tmao, df = 3) ,
         random = ~1 | center,
         data = dd1,
         na.action = na.exclude)

new_data <- data.frame(
  tmao = seq(min(dd1$tmao, na.rm = TRUE),
             max(dd1$tmao, na.rm = TRUE), 
             length.out = 100),
  center = factor(levels(dd1$center)[1], levels = levels(dd1$center))
)

# Build the design matrix for the fixed effects exactly as in the model
X <- model.matrix(~ splines::ns(tmao, df = 3),
                  data = new_data)

# Extract fixed effects estimates and the variance-covariance matrix from the model
beta <- fixef(m)
vc <- vcov(m)

# Compute fitted values (fixed effects only)
new_data$fit <- X %*% beta

# Compute the standard errors for the fitted values
new_data$se <- sqrt(diag(X %*% vc %*% t(X)))

# Calculate the 95% confidence interval
z <- qnorm(0.975)
new_data$lower <- new_data$fit - z * new_data$se
new_data$upper <- new_data$fit + z * new_data$se

# Plot the fitted spline curve with a confidence ribbon using ggplot2
a<-ggplot(new_data, aes(x = tmao, y = fit)) +
  geom_point(data = dd1, aes(x = tmao, y = MoCA), 
             color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "indianred4", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "indianred3", alpha = 0.2) +
  labs(title = "Scatter plot of MoCA and TMAO level (Baseline)",
       x = "TMAO level",
       y = "MoCA score") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a
ggsave(filename = "output/Thea/TMAO MoCA Baseline.jpg", height = 6, width = 8.5, 
       plot = a, quality = 100)


##### FU 1 #####
dd2 <- dd %>%
  filter(visit.name == 'Follow up 1')

m <- lme(MoCA ~ splines::ns(tmao, df = 3) ,
         random = ~1 | center,
         data = dd2,
         na.action = na.exclude)

new_data <- data.frame(
  tmao = seq(min(dd2$tmao, na.rm = TRUE),
             max(dd2$tmao, na.rm = TRUE), 
             length.out = 100),
  center = factor(levels(dd2$center)[1], levels = levels(dd2$center))
)

# Build the design matrix for the fixed effects exactly as in the model
X <- model.matrix(~ splines::ns(tmao, df = 3),
                  data = new_data)

# Extract fixed effects estimates and the variance-covariance matrix from the model
beta <- fixef(m)
vc <- vcov(m)

# Compute fitted values (fixed effects only)
new_data$fit <- X %*% beta

# Compute the standard errors for the fitted values
new_data$se <- sqrt(diag(X %*% vc %*% t(X)))

# Calculate the 95% confidence interval
z <- qnorm(0.975)
new_data$lower <- new_data$fit - z * new_data$se
new_data$upper <- new_data$fit + z * new_data$se

# Plot the fitted spline curve with a confidence ribbon using ggplot2
a<-ggplot(new_data, aes(x = tmao, y = fit)) +
  geom_point(data = dd2, aes(x = tmao, y = MoCA), 
             color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "indianred4", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "indianred3", alpha = 0.2) +
  labs(title = "Scatter plot of MoCA and TMAO level (Follow up 1)",
       x = "TMAO level",
       y = "MoCA score") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a
ggsave(filename = "output/Thea/TMAO MoCA FU1.jpg", height = 6, width = 8.5, 
       plot = a, quality = 100)


##### FU 2 #####
dd3 <- dd %>%
  filter(visit.name == 'Follow up 2')

m <- lme(MoCA ~ splines::ns(tmao, df = 3) ,
         random = ~1 | center,
         data = dd3,
         na.action = na.exclude)

new_data <- data.frame(
  tmao = seq(min(dd3$tmao, na.rm = TRUE),
             max(dd3$tmao, na.rm = TRUE), 
             length.out = 100),
  center = factor(levels(dd3$center)[1], levels = levels(dd3$center))
)

# Build the design matrix for the fixed effects exactly as in the model
X <- model.matrix(~ splines::ns(tmao, df = 3),
                  data = new_data)

# Extract fixed effects estimates and the variance-covariance matrix from the model
beta <- fixef(m)
vc <- vcov(m)

# Compute fitted values (fixed effects only)
new_data$fit <- X %*% beta

# Compute the standard errors for the fitted values
new_data$se <- sqrt(diag(X %*% vc %*% t(X)))

# Calculate the 95% confidence interval
z <- qnorm(0.975)
new_data$lower <- new_data$fit - z * new_data$se
new_data$upper <- new_data$fit + z * new_data$se

# Plot the fitted spline curve with a confidence ribbon using ggplot2
a<-ggplot(new_data, aes(x = tmao, y = fit)) +
  geom_point(data = dd3, aes(x = tmao, y = MoCA), 
             color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "indianred4", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "indianred3", alpha = 0.2) +
  labs(title = "Scatter plot of MoCA and TMAO level (Follow up 2)",
       x = "TMAO level",
       y = "MoCA score") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a
ggsave(filename = "output/Thea/TMAO MoCA FU2.jpg", height = 6, width = 8.5, 
       plot = a, quality = 100)

##### adjusted the plot, significant part #####
dds <- dd %>%
  filter(tmao < 55)
m <- lme(MoCA ~ splines::ns(tmao, df = 3) ,
         random = ~1 | center,
         data = dds,
         na.action = na.exclude)

new_data <- data.frame(
  tmao = seq(min(dds$tmao, na.rm = TRUE),
             max(dds$tmao, na.rm = TRUE), 
             length.out = 100),
  center = factor(levels(dds$center)[1], levels = levels(dds$center))
)

# Build the design matrix for the fixed effects exactly as in the model
X <- model.matrix(~ splines::ns(tmao, df = 3),
                  data = new_data)

# Extract fixed effects estimates and the variance-covariance matrix from the model
beta <- fixef(m)
vc <- vcov(m)

# Compute fitted values (fixed effects only)
new_data$fit <- X %*% beta

# Compute the standard errors for the fitted values
new_data$se <- sqrt(diag(X %*% vc %*% t(X)))

# Calculate the 95% confidence interval
z <- qnorm(0.975)
new_data$lower <- new_data$fit - z * new_data$se
new_data$upper <- new_data$fit + z * new_data$se

# Plot the fitted spline curve with a confidence ribbon using ggplot2
a<-ggplot(new_data, aes(x = tmao, y = fit)) +
  geom_point(data = dds, aes(x = tmao, y = MoCA), 
             color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "indianred4", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "indianred3", alpha = 0.2) +
  labs(title = "Scatter plot of MoCA over time and TMAO level",
       x = "TMAO level",
       y = "MoCA score over time") +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),        
    axis.title = element_text(size = 16)) +
  theme(legend.position = "right") +
  theme(strip.text = element_text(color = "black")) +
  theme(strip.text = element_text(color = "black")) 
a
ggsave(filename = "output/Thea/TMAO MoCA overtime 55.jpg", height = 6, width = 8, 
       plot = a, quality = 100)



# Scatter plot - again ----------------------------------------------------
#### Baseline ####
# 1) Fit raw TMAO as the response and MoCA as the predictor
m2 <- lme(
  tmao ~ MoCA,
  random    = ~1 | center,
  data      = dd,
  na.action = na.exclude
)

# 2) Build newdata grid over MoCA
newdat <- data.frame(
  MoCA = seq(
    from = min(dd$MoCA, na.rm=TRUE),
    to   = max(dd$MoCA, na.rm=TRUE),
    length.out = 100
  )
)

# 3) Design matrix & fixed‐effects vcov
X     <- model.matrix(~ MoCA, data = newdat)
beta  <- fixef(m2)
Vb    <- vcov(m2)

# 4) Compute fitted values and SE for each row
newdat$fit <- as.vector(X %*% beta)
newdat$se  <- sqrt( rowSums((X %*% Vb) * X) )

# 5) 95% CI
crit        <- qnorm(0.975)
newdat$lwr  <- newdat$fit - crit * newdat$se
newdat$upr  <- newdat$fit + crit * newdat$se

# 6) Plot (MoCA on x, raw TMAO on y)
a<-ggplot() +
  geom_point(
    data = dd,
    aes(x = MoCA, y = tmao),
    alpha = 0.6
  ) +
  geom_line(
    data = newdat,
    aes(x = MoCA, y = fit),
    size = 1
  ) +
  geom_ribbon(
    data        = newdat,
    aes(x = MoCA, ymin = lwr, ymax = upr),
    alpha       = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x     = "MoCA score",
    y     = "TMAO",
    title = "Association of TMAO and baseline MoCA score"
  ) +
  theme_minimal(base_size = 14)

a

ggsave(filename = "output/Thea/TMAO ori-MoCA Scatter plot.jpg", height = 4, width = 7, 
       plot =a, quality = 100)

#### Over-time ####
summary(dd$moca.total.sc.age.edy.delta.bl)

# 1) Fit raw TMAO as the response and MoCA as the predictor
m3 <- lme(
  tmao ~ moca.total.sc.age.edy.delta.bl,
  random    = ~1 | center,
  data      = dd,
  na.action = na.exclude
)

# 2) Build newdata grid over MoCA
newdat <- data.frame(
  moca.total.sc.age.edy.delta.bl = seq(
    from = min(dd$moca.total.sc.age.edy.delta.bl, na.rm=TRUE),
    to   = max(dd$moca.total.sc.age.edy.delta.bl, na.rm=TRUE),
    length.out = 100
  )
)

# 3) Design matrix & fixed‐effects vcov
X     <- model.matrix(~ moca.total.sc.age.edy.delta.bl, data = newdat)
beta  <- fixef(m3)
Vb    <- vcov(m3)

# 4) Compute fitted values and SE for each row
newdat$fit <- as.vector(X %*% beta)
newdat$se  <- sqrt( rowSums((X %*% Vb) * X) )

# 5) 95% CI
crit        <- qnorm(0.975)
newdat$lwr  <- newdat$fit - crit * newdat$se
newdat$upr  <- newdat$fit + crit * newdat$se

# 6) Plot 
a<-ggplot() +
  geom_point(
    data = dd,
    aes(x = moca.total.sc.age.edy.delta.bl, y = tmao),
    alpha = 0.6
  ) +
  geom_line(
    data = newdat,
    aes(x = moca.total.sc.age.edy.delta.bl, y = fit),
    size = 1
  ) +
  geom_ribbon(
    data        = newdat,
    aes(x = moca.total.sc.age.edy.delta.bl, ymin = lwr, ymax = upr),
    alpha       = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x     = "Delta change of MoCA score over time from baseline",
    y     = "TMAO",
    title = "Association of TMAO and change of MoCA score over time"
  ) +
  theme_minimal(base_size = 14)

a

ggsave(filename = "output/Thea/TMAO-oridelta MoCA Scatter plot.jpg", height = 4, width = 7, 
       plot =a, quality = 100)


# Kaplan-Meier plot -------------------------------------------------------

#### all Q with cleaned dataset #### 

my_pal <- c("#1b9e77", "#d95f02","#7570b3","#e7298a", "#66a61e")

dd_clean <- dd %>%
  filter(
    !is.na(cog_impairment),
    !is.na(tmao_q),
    !is.na(time_to_cog_impairment)
  )

d_pat <- dd_clean %>%
  group_by(pat.id) %>%
  arrange(time_to_cog_impairment, .by_group = TRUE) %>%
  slice(
    if (any(as.character(cog_impairment) == "1")) {
      which(as.character(cog_impairment) == "1")[1]
    } else {
      n()
    }
  ) %>%
  ungroup() %>%
  mutate(cogimpair.y = time_to_cog_impairment / 365.25)

km1 <- survfit(Surv(cogimpair.y, 
                    as.numeric(as.character(cog_impairment))) ~ tmao_q, data = d_pat)

p <- ggsurvplot(
  km1, data=d_pat,
  break.x.by   = 1,
  legend.title = "TMAO Quintiles",
  legend.labs  = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  xlab = "Time (years)",
  ylab = "Probability of cognitive-impairment-free survival",
  pval     = FALSE,
  conf.int  = TRUE,
  palette   = my_pal,
  ggtheme    = theme_classic()+
    theme(axis.text  = element_text(size = 12)),
  risk.table = TRUE,
  risk.table.y.text.col = TRUE
)

justplot <- p$plot
risktab  <- p$table

zoom_pal <- c("#1b9e77", "#d95f02","#7570b3","#e7298a", "#66a61e")
g_zoom <- ggsurvplot(
  km1, data=d_pat,
  xlim     = c(0, 5),
  ylim     = c(0.00, 0.30),
  palette  = zoom_pal,
  legend   = "none",
  conf.int = FALSE,
  ggtheme  = theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(size = 12),
      plot.background = element_rect(colour = "black")
    )
)$plot

# turn it into a grob
zoom_grob <- ggplotGrob(g_zoom)

# 3) add the inset into your main plot
#    (choose xmin/xmax/ymin/ymax so the little window sits where you like)
justplot2 <- justplot +
  annotation_custom(
    grob = zoom_grob,
    xmin = 0.5,    xmax = 5,    # ← adjust these
    ymin = 0.5,   ymax = 1.0  # ← and these to move/resize inset
  )

# 4) finally, draw the full + table side by side
lay <- rbind(c(1,1),
             c(1,1),
             c(2,2))
a<-grid.arrange(justplot2, risktab, layout_matrix = lay)
a
ggsave(filename = "output/Thea/TMAO-KM.jpg", height = 8, width = 10, 
       plot =a, quality = 100)

##### only Q4 and Q5 #####
d_pat_sub <- subset(d_pat, tmao_q %in% c("4","5"))

km1 <- survfit(Surv(cogimpair.y, 
                    as.numeric(as.character(cog_impairment))) ~ tmao_q, data = d_pat_sub)

p <- ggsurvplot(
  km1, data=d_pat_sub,
  break.x.by   = 1,
  legend.title = "TMAO Quintiles",
  legend.labs  = c("Q4", "Q5"),
  xlab = "Time (years)",
  ylab = "Probability of patient free from cognitive impairment",
  pval     = FALSE,
  conf.int  = TRUE,
  palette   = c("red3", "red4"),
  ggtheme    = theme_classic()+
    theme(axis.text  = element_text(size = 12)),
  risk.table = TRUE,
  risk.table.y.text.col = TRUE
)

justplot <- p$plot
risktab  <- p$table

zoom_pal <- c("red3", "red4")
g_zoom <- ggsurvplot(
  km1, data=d_pat_sub,
  xlim     = c(0, 5),
  ylim     = c(0.00, 0.30),
  palette  = zoom_pal,
  legend   = "none",
  conf.int = FALSE,
  ggtheme  = theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(size = 12),
      plot.background = element_rect(colour = "black")
    )
)$plot

# turn it into a grob
zoom_grob <- ggplotGrob(g_zoom)

# 3) add the inset into your main plot
#    (choose xmin/xmax/ymin/ymax so the little window sits where you like)
justplot2 <- justplot +
  annotation_custom(
    grob = zoom_grob,
    xmin = 0.5,    xmax = 5,    # ← adjust these
    ymin = 0.5,   ymax = 1.0  # ← and these to move/resize inset
  )

# 4) finally, draw the full + table side by side
lay <- rbind(c(1,1),
             c(1,1),
             c(2,2))
a<-grid.arrange(justplot2, risktab, layout_matrix = lay)
a
ggsave(filename = "output/Thea/TMAO-KM Q4-5.jpg", height = 8, width = 10, 
       plot =a, quality = 100)

#### uncleaned all ####
dd_clean <- dd_clean %>%
  mutate(cogimpair.y = time_to_cog_impairment/365.25)

km1 <- survfit(Surv(cogimpair.y, 
                    as.numeric(as.character(cog_impairment))) ~ tmao_q, data = dd_clean)

p <- ggsurvplot(
  km1, data=dd_clean,
  break.x.by   = 1,
  legend.title = "TMAO Quintiles",
  legend.labs  = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  xlab = "Time (years)",
  ylab = "Probability of patient free from cognitive impairment",
  pval     = FALSE,
  conf.int  = TRUE,
  palette   = c("lightcoral", "tomato1", "tomato3", "red3", "red4"),
  ggtheme    = theme_classic()+
    theme(axis.text  = element_text(size = 12)),
  risk.table = TRUE,
  risk.table.y.text.col = TRUE
)

justplot <- p$plot
risktab  <- p$table

zoom_pal <- c("lightcoral", "tomato1", "tomato3", "red3", "red4")
g_zoom <- ggsurvplot(
  km1, data=dd_clean,
  xlim     = c(0, 5),
  ylim     = c(0.00, 0.30),
  palette  = zoom_pal,
  legend   = "none",
  conf.int = FALSE,
  ggtheme  = theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(size = 12),
      plot.background = element_rect(colour = "black")
    )
)$plot

# turn it into a grob
zoom_grob <- ggplotGrob(g_zoom)

# 3) add the inset into your main plot
#    (choose xmin/xmax/ymin/ymax so the little window sits where you like)
justplot2 <- justplot +
  annotation_custom(
    grob = zoom_grob,
    xmin = 0.5,    xmax = 5,    # ← adjust these
    ymin = 0.5,   ymax = 1.0  # ← and these to move/resize inset
  )

# 4) finally, draw the full + table side by side
lay <- rbind(c(1,1),
             c(1,1),
             c(2,2))
a<-grid.arrange(justplot2, risktab, layout_matrix = lay)
a

#### Re-do with the new color ####
km1 <- survfit(Surv(cogimpair.y, 
                    as.numeric(as.character(cog_impairment))) 
               ~ tmao_q, 
               data = d_pat)

# 2. Main plot with new palette
p_main <- ggsurvplot(
  km1, data = d_pat,
  break.x.by   = 1,
  legend.title = "TMAO Quintiles",
  legend.labs  = c("Q1","Q2","Q3","Q4","Q5"),
  xlab         = "Time (years)",
  ylab         = "Probability free from cognitive impairment",
  pval         = FALSE,
  conf.int     = TRUE,
  palette      = my_pal,
  ggtheme      = theme_classic() +
    theme(axis.text = element_text(size = 12)),
  risk.table   = TRUE,
  risk.table.y.text.col = TRUE
)
main_plot <- p_main$plot
risk_table <- p_main$table

# 3. Zoomed‐in panel (no CI, no legend)
p_zoom <- ggsurvplot(
  km1, data = d_pat,
  xlim     = c(0,5),       # full x‐range shown
  ylim     = c(0, 0.30),   # focusing on lower survival
  palette  = my_pal,
  legend   = "none",
  conf.int = FALSE,
  ggtheme  = theme_classic() +
    theme(
      axis.title     = element_blank(),
      axis.text      = element_text(size = 12),
      plot.background= element_rect(colour="black")
    )
)$plot

# Turn the zoom panel into a grob
zoom_grob <- ggplotGrob(p_zoom)

# 4. Add the inset, enlarged by expanding its coordinate box
main_with_bigger_inset <- main_plot +
  annotation_custom(
    grob = zoom_grob,
    xmin = 0.5,    # move inset further left
    xmax = 5,      # extend it to the right edge
    ymin = 0.3,   # lower its bottom edge
    ymax = 1.05    # raise its top edge slightly above plot
  )

lay <- rbind(
  c(1,1),
  c(1,1),
  c(2,2)
)

combined <- arrangeGrob(
  main_with_bigger_inset,
  risk_table,
  layout_matrix = lay
)

# preview
grid.draw(combined)

# and to save:
ggsave(file.path(output_dir, "tmao_survival_big_inset.png"),
       plot   = combined,
       width  = 10, 
       height = 8,
       dpi    = 300)


# New Kaplan-Meier Plot ---------------------------------------------------

#### Dataset ####

library(dplyr)
library(ggplot2)

dd_clean <- dd %>%
  filter(
    !is.na(cog_impairment),
    !is.na(tmao),
    !is.na(time_to_cog_impairment)
  )

# Keep one row per patient:
# - first cognitive impairment event if event occurred
# - otherwise last follow-up
d_pat <- dd_clean %>%
  group_by(pat.id) %>%
  arrange(time_to_cog_impairment) %>%
  slice(
    if (any(as.character(cog_impairment) == "1")) {
      which(as.character(cog_impairment) == "1")[1]
    } else {
      n()
    }
  ) %>%
  ungroup() %>%
  mutate(
    cogimpair.y = time_to_cog_impairment / 365.25,
    event = as.numeric(as.character(cog_impairment))
  )


#### 2. median TMAO ####

tmao_median <- median(d_pat$tmao, na.rm = TRUE)

tmao_median

d_pat <- d_pat %>%
  mutate(
    TMAO_median = case_when(
      tmao <= tmao_median ~ "Below median",
      tmao >  tmao_median ~ "Above median"
    ),
    TMAO_median = factor(
      TMAO_median,
      levels = c("Below median", "Above median")
    )
  )

table(d_pat$TMAO_median)

# Median TMAO value
tmao_median


#### 4. Create total, women and men datasets ####


d_total <- d_pat

d_women <- d_pat %>%
  filter(pat.sex == "Female")

d_men <- d_pat %>%
  filter(pat.sex == "Male")


# Check numbers in each group
table(d_total$TMAO_median)

table(d_women$TMAO_median)

table(d_men$TMAO_median)

#### Function for making the KM plots ####
make_km_inset_plot <- function(data,
                               plot_title = NULL,
                               zoom_xlim = c(0, 5),
                               zoom_ylim = c(0, 0.30),
                               inset_xmin = 0.7,
                               inset_xmax = 5.0,
                               inset_ymin = 0.35,
                               inset_ymax = 0.95) {
  
  # Check that data are present
  if (nrow(data) == 0) {
    stop("Dataset contains zero rows. Check the sex coding.")
  }
  
  # KM model
  km_fit <- survfit(
    Surv(cogimpair.y, event) ~ TMAO_median,
    data = data
  )
  
  
  # Main plot

  
  p_main <- ggsurvplot(
    km_fit,
    data = data,
    
    break.x.by = 1,
    
    legend.title = "TMAO",
    legend.labs = c(
      "Below median",
      "Above median"
    ),
    
    xlab = "Time (years)",
    ylab = "Probability free from cognitive impairment",
    
    title = plot_title,
    
    pval = FALSE,
    conf.int = TRUE,
    
    palette = c("#1b9e77", "#d95f02"),
    
    ggtheme = theme_classic() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      ),
    
    risk.table = TRUE,
    risk.table.y.text.col = TRUE
  )
  
  
  main_plot <- p_main$plot
  
  risk_table <- p_main$table +
    theme(
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
  

  # Zoom inset

  
  p_zoom <- ggsurvplot(
    km_fit,
    data = data,
    
    palette = c("#1b9e77", "#d95f02"),
    
    legend = "none",
    conf.int = FALSE,
    
    ggtheme = theme_classic() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 9),
        
        plot.margin = margin(
          t = 5,
          r = 5,
          b = 5,
          l = 5
        ),
        
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          linewidth = 0.5
        )
      )
  )$plot
  
  
  # Zoom without dropping survival data
  p_zoom <- p_zoom +
    coord_cartesian(
      xlim = zoom_xlim,
      ylim = zoom_ylim
    )
  
  
  zoom_grob <- ggplotGrob(p_zoom)
  
  
  # Put inset into main KM plot
  
  main_with_inset <- main_plot +
    annotation_custom(
      grob = zoom_grob,
      xmin = inset_xmin,
      xmax = inset_xmax,
      ymin = inset_ymin,
      ymax = inset_ymax
    )
  
  

  # Main figure + risk table
  
  combined <- arrangeGrob(
    main_with_inset,
    risk_table,
    ncol = 1,
    heights = c(3.3, 1)
  )
  
  return(combined)
}

#### Generate the 3 plots ####

#### Total population ####

fig_total <- make_km_inset_plot(
  data = d_total,
  plot_title = "Total population"
)


#### Women ####

fig_women <- make_km_inset_plot(
  data = d_women,
  plot_title = "Women"
)


#### Men ####

fig_men <- make_km_inset_plot(
  data = d_men,
  plot_title = "Men"
)

grid.newpage()
grid.draw(fig_total)

grid.newpage()
grid.draw(fig_women)

grid.newpage()
grid.draw(fig_men)

ggsave(
  "output/Thea/KM_TMAO_total.jpg",
  plot = fig_total,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300
)

ggsave(
  "output/Thea/KM_TMAO_women.jpg",
  plot = fig_women,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300
)

ggsave(
  "output/Thea/KM_TMAO_men.jpg",
  plot = fig_men,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300
)

# Forest plot - Cox -------------------------------------------------------
TMAO_Cox <- read_csv("TMAO Cox.csv")

TMAO_Cox$model <- factor(TMAO_Cox$model,
                        levels = c("Model 5","Model 4B","Model 4A", "Model 3", "Model 2", "Model 1"))

TMAO_Cox$quintile <- factor(TMAO_Cox$quintile,
                           levels = c("Q5", "Q4", "Q3", "Q2"))

# Define custom colors for the quintiles
custom_colors <- c(
  "Q5" = "indianred4",
  "Q4" = "indianred2",
  "Q3" = "orange2",
  "Q2" = "goldenrod1")

a <-ggplot(TMAO_Cox, aes(x = mean, y = model, color = quintile)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  labs(title = "Forest plot of Hazard ratios of TMAO and mild cognitive impairment",
       x = "Hazard ratios",
       y = "",
       color = "Quintiles") +
  scale_color_manual(values = custom_colors) +
  theme_bw()+
  theme(
    axis.text = element_text(size = 12),        
    axis.title = element_text(size = 14)) +
  theme(panel.grid.major.y = element_blank())

a

ggsave(filename = "output/Thea/TMAO-Cox Forest plot.jpg", height = 5, width = 7, 
       plot =a, quality = 100)

ggsave(
  filename = "output/Thea/TMAO-Cox Forest plot-new.jpg",
  plot = a,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300,
  quality = 100
)


# Pie charts --------------------------------------------------------------
summary(dd$moca.merged)
dds <- dd %>%
  dplyr::select(pat.id, tmao_q, moca.merged, visit.name)

glimpse(dds)

cat_levels <- c(
  "1. stable normal", "2. stable MCI", "3. stable dementia",
  "4. normal → MCI",  "5. MCI → dementia", "6. normal → dementia"
)

my_cols <- c(
  "1. stable normal"    = "#56B4E9",  
  "2. stable MCI"       = "#009E73",  
  "3. stable dementia"  = "#999999",  
  "4. normal → MCI"     = "#F0E442",  
  "5. MCI → dementia"   = "#E69F00",  
  "6. normal → dementia"= "#D55E00"   
)

# 1. reshape to wide, keeping only Baseline & Follow up 5, 
#and rename that follow-up column
df_wide <- dds %>% 
  filter(visit.name %in% c("Baseline", "Follow up 5")) %>% 
  dplyr::select(pat.id, tmao_q, visit.name, moca.merged) %>% 
  pivot_wider(
    names_from  = visit.name,
    values_from = moca.merged,
    names_prefix= "MoCA_"
  ) %>% 
  rename(
    MoCA_FU5 = `MoCA_Follow up 5`
  )

# 2. classify into the six groups
df_grp <- df_wide %>%
  mutate(
    category = case_when(
      # 1. stable normal
      MoCA_Baseline >= 26 & MoCA_FU5     >= 26 ~ "1. stable normal",
      # 2. stable MCI
      MoCA_Baseline %in% 21:25 & MoCA_FU5 %in% 21:25 ~ "2. stable MCI",
      # 3. stable dementia
      MoCA_Baseline < 21  & MoCA_FU5     < 21  ~ "3. stable dementia",
      # 4. normal → MCI
      MoCA_Baseline >= 26 & MoCA_FU5 %in% 21:25 ~ "4. normal → MCI",
      # 5. MCI → dementia
      MoCA_Baseline %in% 21:25 & MoCA_FU5     < 21  ~ "5. MCI → dementia",
      # 6. normal → dementia
      MoCA_Baseline >= 26 & MoCA_FU5     < 21  ~ "6. normal → dementia",
      TRUE ~ NA_character_
    ),
    category = factor(category, levels = c(
      "1. stable normal", "2. stable MCI", "3. stable dementia",
      "4. normal → MCI",  "5. MCI → dementia", "6. normal → dementia"
    ))
  ) %>% 
  # drop any patients who didn’t fit one of the six
  filter(!is.na(category), !is.na(tmao_q))

# 3. count & compute percentages within each TMAO quintile
df_sum <- df_grp %>% 
  count(tmao_q, category) %>% 
  group_by(tmao_q) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

# 4. plot: one pie per quintile
a<-ggplot(df_sum, aes(x = "", y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ tmao_q, nrow = 1, strip.position = "bottom") +
  labs(
    title = "Cognitive trajectory proportions by TMAO quintile",
    subtitle = "Baseline→ Follow up 5",
    fill  = "Group"
  ) +
  scale_fill_manual(
    values = my_cols,
    drop   = FALSE         # keep all six levels + colours even if missing
  ) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5)
  )
a
ggsave(filename = "output/Thea/Pie chart FU5.jpg", height = 2, width = 7, 
       plot =a, quality = 100)

ggsave(
  filename = "output/Thea/Pie Chart FU5-new.jpg",
  plot = a,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300,
  quality = 100
)

print(df_sum, n = 21)

#### Q1 and Q4-5 ####
df_grp$tmao_q <- as.factor(df_grp$tmao_q)

df_grp145 <- df_grp %>%
  filter(tmao_q %in% c("1", "4", "5"))

df_sum145 <- df_grp145 %>% 
  count(tmao_q, category) %>% 
  group_by(tmao_q) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

a<-ggplot(df_sum145, aes(x = "", y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ tmao_q, nrow = 1, strip.position = "bottom") +
  labs(
    title = "Cognitive trajectory proportions by TMAO quintile",
    subtitle = "Baseline→ Follow up 5",
    fill  = "Group"
  ) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5)
  )
a

ggsave(filename = "output/Thea/Pie chart FU5-Q145.jpg", height = 3, width = 7, 
       plot =a, quality = 100)

#### FU3 ####
df_wide <- dds %>% 
  filter(visit.name %in% c("Baseline", "Follow up 3")) %>% 
  select(pat.id, tmao_q, visit.name, moca.merged) %>% 
  pivot_wider(
    names_from  = visit.name,
    values_from = moca.merged,
    names_prefix= "MoCA_"
  ) %>% 
  rename(
    MoCA_FU3 = `MoCA_Follow up 3`
  )

# 2. classify into the six groups
df_grp <- df_wide %>%
  mutate(
    category = case_when(
      # 1. stable normal
      MoCA_Baseline >= 26 & MoCA_FU3     >= 26 ~ "1. stable normal",
      # 2. stable MCI
      MoCA_Baseline %in% 21:25 & MoCA_FU3 %in% 21:25 ~ "2. stable MCI",
      # 3. stable dementia
      MoCA_Baseline < 21  & MoCA_FU3     < 21  ~ "3. stable dementia",
      # 4. normal → MCI
      MoCA_Baseline >= 26 & MoCA_FU3 %in% 21:25 ~ "4. normal → MCI",
      # 5. MCI → dementia
      MoCA_Baseline %in% 21:25 & MoCA_FU3     < 21  ~ "5. MCI → dementia",
      # 6. normal → dementia
      MoCA_Baseline >= 26 & MoCA_FU3     < 21  ~ "6. normal → dementia",
      TRUE ~ NA_character_
    ),
    category = factor(category, levels = c(
      "1. stable normal", "2. stable MCI", "3. stable dementia",
      "4. normal → MCI",  "5. MCI → dementia", "6. normal → dementia"
    ))
  ) %>% 
  # drop any patients who didn’t fit one of the six
  filter(!is.na(category), !is.na(tmao_q))

# 3. count & compute percentages within each TMAO quintile
df_sum <- df_grp %>% 
  count(tmao_q, category) %>% 
  group_by(tmao_q) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

a<-ggplot(df_sum, aes(x = "", y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ tmao_q, nrow = 1, strip.position = "bottom") +
  labs(
    title = "Cognitive trajectory proportions by TMAO quintile",
    subtitle = "Baseline→ Follow up 3",
    fill  = "Group"
  ) +
  scale_fill_manual(
    values = my_cols,
    drop   = FALSE         # keep all six levels + colours even if missing
  ) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5)
  )
a
ggsave(filename = "output/Thea/Pie chart FU3.jpg", height = 2, width = 7, 
       plot =a, quality = 100)

ggsave(
  filename = "output/Thea/Pie Chart FU3-new.jpg",
  plot = a,
  width = 170,
  height = 210,
  units = "mm",
  dpi = 300,
  quality = 100
)

print(df_sum, n = 28)

##### Q1 and Q4-5 #####
df_grp$tmao_q <- as.factor(df_grp$tmao_q)

df_grp145 <- df_grp %>%
  filter(tmao_q %in% c("1", "4", "5"))

df_sum145 <- df_grp145 %>% 
  count(tmao_q, category) %>% 
  group_by(tmao_q) %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  ungroup()

a<-ggplot(df_sum145, aes(x = "", y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ tmao_q, nrow = 1, strip.position = "bottom") +
  labs(
    title = "Cognitive trajectory proportions by TMAO quintile",
    subtitle = "Baseline → Follow up 3",
    fill  = "Group"
  ) +
  theme_void() +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text       = element_text(face = "bold"),
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5)
  )
a

ggsave(filename = "output/Thea/Pie chart FU3-Q145.jpg", height = 3, width = 7, 
       plot =a, quality = 100)


# Loss to FU ------------------------------------------------------
glimpse(dd)

summary(dd$stop.grund)

dd1 <- dd %>%
  filter(visit.name %in% c("Follow up 1"))
summary(dd1$stop.grund)

dd2 <- dd %>%
  filter(visit.name %in% c("Follow up 2"))
summary(dd2$stop.grund)

dd3 <- dd %>%
  filter(visit.name %in% c("Follow up 3"))
summary(dd3$stop.grund)

dd4 <- dd %>%
  filter(visit.name %in% c("Follow up 4"))
summary(dd4$stop.grund)

dd5 <- dd %>%
  filter(visit.name %in% c("Follow up 5"))
summary(dd5$stop.grund)

summary(dd5$moca.merged)

#loss fu 1
result1 <- dd1 %>%
  filter(! pat.id %in% dd2$pat.id)

summary(result1$stop.grund)

result2 <- dd2 %>%
  filter(! pat.id %in% dd3$pat.id)

summary(result2$stop.grund)

#loss fu3-4-5
result3 <- dd3 %>%
  filter(! pat.id %in% dd4$pat.id)

summary(result3$stop.grund)

result4 <- dd4 %>%
  filter(! pat.id %in% dd5$pat.id)

summary(result4$stop.grund)





# Fixing the stroke rmd ---------------------------------------------------
# list your candidate factor columns here
facs <- c("pat.sex","highest.education.level.groups",
          "gesundheitszustand","rauchen","prev.hypertonie",
          "sport","prev.coronary.heart.disease",
          "prev.diabetes", "creatinine.gfr")  # add or remove as needed

sapply(ddns[facs], function(x) {
  if (!is.factor(x)) return(NA)
  nlev <- length(levels(x))
  paste(nlev, "levels:", paste(levels(x), collapse=", "))
})

# build an lm() with the same fixed effects
lm0 <- lm(MoCA ~ time_since_first + tmao_q + time_since_first:tmao_q +
            age.bl + pat.sex + highest.education.level.groups +
            gesundheitszustand + gds.total + rauchen + bmi +
            prev.hypertonie + sport + total.alcohol.consumption +
            prev.coronary.heart.disease + prev.diabetes + creatinine.gfr,
          data=ddns, na.action=na.exclude)

vif(lm0)
alias(lm0)

lm_ext <- lm(MoCA ~ time_since_first + tmao_log
             + time_since_first:tmao_log
             + age.bl + pat.sex
             + highest.education.level.groups
             + gesundheitszustand + gds.total
             + rauchen + bmi + prev.hypertonie
             + sport + total.alcohol.consumption
             + prev.coronary.heart.disease
             + prev.diabetes + creatinine.gfr,
             data = ddns, na.action = na.exclude)

alias(lm_ext)
vif(lm_ext, type="predictor")

lm_noint <- lm(
  MoCA ~ time_since_first + tmao_log
  + age.bl + pat.sex
  + highest.education.level.groups
  + gesundheitszustand + gds.total
  + rauchen + bmi + prev.hypertonie
  + sport + total.alcohol.consumption
  + prev.coronary.heart.disease
  + prev.diabetes + creatinine.gfr,
  data = ddns,
  na.action = na.exclude
)

vif(lm_noint, type="predictor")


# Miscs -------------------------------------------------------------------
glimpse(dd)

dd <- dd %>%
  mutate(year = (dd$duration)/365)

summary(dd$year)

mean(dd$year, na.rm = TRUE)

sd(dd$year, na.rm = TRUE)

#### TMAO quintiles ####

# 1. Compute summary stats by quintile
summary_tbl <- dd %>%
  group_by(tmao_q) %>%
  summarize(
    mean    = mean(tmao, na.rm = TRUE),
    sd      = sd(tmao,   na.rm = TRUE),
    median  = median(tmao, na.rm = TRUE),
    IQR     = IQR(tmao,    na.rm = TRUE),
    .groups = "drop"
  )

# 2. Create formatted strings
summary_tbl <- summary_tbl %>%
  mutate(
    mean_sd    = sprintf("%.2f ± %.2f", mean, sd),
    median_iqr = sprintf("%.2f (%.2f)", median, IQR)
  )

# 3. (Optional) Merge back onto the original data.frame
dd_with_stats <- dd %>%
  left_join(
    summary_tbl %>% select(tmao_q, mean_sd, median_iqr),
    by = "tmao_q"
  )

# 4. View results
print(summary_tbl)

write_excel_csv(summary_tbl, file = file.path(output_dir, "tmaoquintile.csv"))


# TMAO latest questions -------------------------------------------------------
glimpse(dd)

dd$stop.grund <- as.factor(dd$stop.grund)
summary(dd$stop.grund)

#### New demographic table ####
dd <- dd %>%
  mutate(HV = HVraw/sbtiv.vol,
         lhv = LHV/sbtiv.vol,
         rhv = RHV/sbtiv.vol)

dd <- dd %>%
  mutate(essen.fruechte = as.factor(essen.fruechte),
         essen.gemuese = as.factor(essen.gemuese),
         essen.milch = as.factor(essen.milch),
         essen.fleisch = as.factor(essen.fleisch))

summary(dd$essen.fruechte)
summary(dd$essen.gemuese)
summary(dd$essen.milch)
summary(dd$essen.fleisch)

dd <- dd %>% 
  mutate(
    veg_daily_cat   = if_else(
      essen.gemuese %in% c("3 bis 4 Portionen/Tag", "5 und mehr Portionen/Tag"),
      "Gemüse > 2 Portionen/Tag",                # meets your criterion
      "Gemüse ≤ 2 Portionen/Tag",               # everybody else
      missing = NA_character_                   # echte NA / «keine Antwort»
    ),
    
    fruit_daily_cat = if_else(
      essen.fruechte %in% c("3 bis 4 Portionen/Tag", "5 und mehr Portionen/Tag"),
      "Früchte > 2 Portionen/Tag",
      "Früchte ≤ 2 Portionen/Tag",
      missing = NA_character_
    ),
    
    dairy_weekly_cat = if_else(
      essen.milch %in% c("4 Tage pro Woche", "5 Tage pro Woche",
                         "6 Tage pro Woche", "7 Tage pro Woche"),
      "Milch > 3 Tage/Woche",
      "Milch ≤ 3 Tage/Woche",
      missing = NA_character_
    ),
    
    meat_weekly_cat = if_else(
      essen.fleisch %in% c("4 Tage pro Woche", "5 Tage pro Woche",
                           "6 Tage pro Woche", "7 Tage pro Woche"),
      "Fleisch > 3 Tage/Woche",
      "Fleisch ≤ 3 Tage/Woche",
      missing = NA_character_
    )
  )


ctrl10 <- arsenal::tableby.control(
  digits       = 5,   # mean, SD, median, quartiles, range, etc.
  digits.p     = 3,    # keep p-values readable (change if you like)
  digits.count = 0,    # counts stay as integers
  digits.pct   = 1     # 1 decimal for percentages, or raise if you prefer
)

tab1 <- tableby(tmao_q ~ nfl + HV + lhv + rhv + AI + sbtiv.vol +
                  lncci.presence + snci.presence + mb.presence + wml.presence +
                  wml.vol + mod.faz + brain.volume.raw + brain.volume.normalized +
                  CoCo + MoCA + SF + DSST + TMTA + TMTB +
                  duration + stop.grund + vhf.typ.aktuell.bl + age.bl + pat.sex +
                  highest.education.level.groups + gesundheitszustand +
                  gds.total + rauchen + bmi + prev.hypertonie +
                  total.alcohol.consumption + essen.fruechte + fruit_daily_cat +
                  essen.gemuese + veg_daily_cat +
                  essen.fleisch + meat_weekly_cat + essen.milch.portion +
                  dairy_weekly_cat + sport +
                  prev.stroke.tia
                + prev.coronary.heart.disease + prev.diabetes + creatinine.gfr 
                + ldl + hdl + trigly + chol + chads
                + med.oak.name + med.antiplatelet.yn,
                data = dd,
                control = ctrl10)

tab1_df <- as.data.frame(
  summary(tab1, text = "html"),      
  stringsAsFactors = FALSE,
  text = "html"
)

DT::datatable(
  tab1_df,
  escape    = FALSE,      # ← THIS hides the &nbsp; codes
  caption   = htmltools::tags$caption(
    style = "caption-side: top; font-weight: bold;",
    "Demographic Table"),
  extensions = "Buttons",
  options   = list(
    dom        = "Bfrtip",
    buttons    = c("copy", "csv", "excel", "print"),
    scrollX    = TRUE,
    pageLength = 25
  )
)

#### Age- and Sex- match ####
dd$hv <- dd$HVraw
# --- Step 1 – keep Q1 and Q5 rows 
q1_data <- dd %>% filter(tmao_q == "1")
q5_data <- dd %>% filter(tmao_q == "5")

# --- Step 2 – helper: find Q5 rows within ±2 y and same sex 
match_age_sex <- function(age, sex, data, range = 2) {
  data %>% 
    filter(abs(age.bl - age) <= range,
           pat.sex == sex)
}

# --- Step 3 – loop over Q1 and keep Q5 matches with smaller HV 
matched_pairs <- lapply(seq_len(nrow(q1_data)), function(i) {
  
  age_i   <- q1_data$age.bl[i]
  sex_i   <- q1_data$pat.sex[i]
  hv_i    <- q1_data$hv[i]          # <- change ‘hv’ to your exact column name
  id_q1   <- q1_data$pat.id[i]
  
  # all age- & sex-compatible Q5 rows
  cand_q5 <- match_age_sex(age_i, sex_i, q5_data)
  
  # keep only Q5 candidates with smaller HV than the Q1 subject
  cand_q5 <- cand_q5 %>% filter(hv < hv_i)
  
  if (nrow(cand_q5) == 0) return(NULL)
  
  ## ► OPTION A – keep *all* qualifying Q5 matches
  #cand_q5 %>% 
   # transmute(pat_id_q1   = id_q1,
             # age.bl_q1   = age_i,
            #  sex_q1      = sex_i,
             # hv_q1       = hv_i,
            #  pat_id_q5   = pat.id,
             # age.bl_q5   = age.bl,
            #  sex_q5      = pat.sex,
             # hv_q5       = hv)
  
  ## ► OPTION B – keep just the *first* Q5 row (e.g. the closest in age)
  cand_q5 %>% 
  slice_min(abs(age.bl - age_i), n = 1) %>%   # choose one row
  transmute(pat_id_q1   = id_q1,
            age.bl_q1   = age_i,
            sex_q1      = sex_i,
            hv_q1       = hv_i,
            pat_id_q5   = pat.id,
            age.bl_q5   = age.bl,
            sex_q5      = pat.sex,
            hv_q5       = hv)
})

# --- Step 4 – combine everything 
matched_pairs_df <- bind_rows(matched_pairs)

print(matched_pairs_df)
tmao.matched.age <- matched_pairs_df

top50_TMAO <- tmao.matched.age %>% 
  mutate(hv_diff = hv_q1 - hv_q5) %>%   # compute the gap
  arrange(desc(hv_diff)) %>%            # biggest → smallest
  slice_head(n = 50)                    # keep the first 50

# quick peek
glimpse(top50_TMAO)

write.csv(tmao.matched.age, file = file.path(output_dir, "tmao.matched.age.full.csv"), row.names = FALSE)
write.csv(top50_TMAO, file = file.path(output_dir, "top50_TMAO.csv"), row.names = FALSE)
