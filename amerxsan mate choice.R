library(lme4)
library(ggplot2)
library(car)
library(emmeans)
library(factoextra)
library(MASS)
library(ggExtra)
library(viridis) 
library(plotly)
library(plyr)
library(export)
library(boot)
library(forcats)
####read data and courtship outcome----
hyb_amer=read.csv("amer_boris.csv")
summary(hyb_amer)
hyb_amer$female.sp = as.factor(hyb_amer$female.sp)
hyb_amer$male.sp = as.factor(hyb_amer$male.sp)
hyb_amer$pair = as.factor(hyb_amer$pair)
hyb_amer$hyb = as.factor(hyb_amer$hyb)
hyb_amer$scorer.initials = as.factor(hyb_amer$scorer.initials)

#Figure 2a----
ggplot(data=hyb_amer, aes(x=pair,fill = as.factor(cop)))+
  geom_bar(alpha = 0.5,size = 1,position="fill",color = "black")+
  theme_classic()+
  scale_fill_manual(values=c("white","black"))+
  geom_hline(yintercept=0.5, linetype = "dashed",size=1)+
  geom_text(stat='count', aes(label=..count..), position = "fill",vjust = -0.5)
graph2ppt(file = "amer_cop_bar.pptx",width = 7,height = 5)

#chi-sqr
#switch the numbers when looking at other pairs
pairA = c(8,23)
pairB = c(24,11)
tab = rbind(pairA,pairB)
chisq.test(tab)

### Figure 2b&c what predict copulation----
#(figure 2c)multimodal mean (did intro)
hyb_amer_clean = subset(hyb_amer, intro_mean < 100)
hyb_amer_clean = subset(hyb_amer_clean, sqrt(vib_mean) < 7.5)
hyb_amer_full_intro = subset(hyb_amer, intro_count>0)
hyb_amer_intro = subset(hyb_amer_clean, intro_count>0)
hyb_amer_vib = subset(hyb_amer_clean, vib_count>0)
boxplot(hyb_amer$intro_mean)
summary(hyb_amer_intro)
hyb_amer$pair = factor(hyb_amer$pair, levels = c("AA","AS","SS","SA"))
ggplot(data = hyb_amer_intro, aes(x = pair,y = vib_mean,fill = as.factor(cop)))+
  geom_boxplot(alpha = .7)+theme_classic()
graph2ppt(file = "amer_vibmeanintro_box.pptx",width = 7,height = 5)


#(Fig. 2b)visual mean (male did intro)
hyb_amer_intro$pair = factor(hyb_amer_intro$pair, levels = c("AA","AS","SA","SS"))

ggplot(data = hyb_amer_intro, aes(x = pair,y = intro_mean,fill = as.factor(cop)))+
  geom_boxplot(alpha = .7)+theme_classic()+ylim(0,100)

graph2ppt(file = "amer_didintro_box.pptx",width = 7,height = 5)

### Figure 2b and c stats: BOOTSTRAP----
#try bootstrap because the data is not normal but I do want to compare mean

#define the bootstrapping function
bootstrap_data <- function(data, strata_var1, strata_var2, numeric_var) {
  bootstrap_results_df <- data.frame(
    Pair = character(),
    Cop = character(),
    Mean = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  #find all combinations of the categorical variables
  combinations <- expand.grid(unique(data[[strata_var1]]), unique(data[[strata_var2]]))
  colnames(combinations) <- c(strata_var1, strata_var2)
  
  #function to calculate the mean of the resampled data
  mean_function <- function(data, indices) {
    d <- data[indices]
    mean(d)
  }
  
  #loop through all combinations of strata
  for(i in 1:nrow(combinations)) {
    subset_data <- subset(data, data[[strata_var1]] == combinations[i, strata_var1] & data[[strata_var2]] == combinations[i, strata_var2])
    if(nrow(subset_data) > 0) {
      boot_res <- boot(subset_data[[numeric_var]], statistic = mean_function, R = 1000)
      #95% confidence interval
      ci <- boot.ci(boot_res, type = c("perc"))
      
      #store in a data frame
      bootstrap_results_df <- rbind(bootstrap_results_df, data.frame(
        Pair = as.character(combinations[i, strata_var1]),
        Cop = as.character(combinations[i, strata_var2]),
        Mean = mean(subset_data[[numeric_var]]),
        CI_Lower = ci$perc[4],
        CI_Upper = ci$perc[5]
      ))
    }
  }

  return(bootstrap_results_df)
}


set.seed(77)
bootstrap_results_df <- bootstrap_data(hyb_amer_intro, "pair", "cop", "intro_mean")
bs_vib_mean =  bootstrap_data(hyb_amer_intro, "pair", "cop", "vib_mean")
#plot the results
plot_bootstrap_results <- function(bootstrap_results_df, measure) {
  
  ggplot(bootstrap_results_df, aes(x = Pair, y = Mean, linetype = Cop,shape = Cop)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +
    labs(x = "Cop", y = measure, title = "Mate Choice -- Bootstrapped Means and CIs") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic()
}

plot_bootstrap_results(bootstrap_results_df, "Visual Phase Duration (s)")
graph2ppt(file = "amer_introdur_bs.pptx",width = 7,height = 5)
plot_bootstrap_results(bs_vib_mean,"Multimodal Phase Duration (s)")
graph2ppt(file = "amer_vibdur_bs.pptx",width = 7,height = 5)


#### Fig. 1 compare amer and sans courtship----
hyb_amer_clean_cop = subset(hyb_amer_clean, cop == 1)#so that male got to display full time vib
summary(hyb_amer_clean_cop)
hist(hyb_amer_clean_cop$vib_mean)
ggplot(data = hyb_amer_clean_cop, aes(x = male.sp,y = vib_mean,fill = male.sp))+
  geom_boxplot(alpha = .7)+theme_classic()#bad visualization 
df <- ddply(hyb_amer_clean_cop, c("male.sp"), summarize, Mean = mean(vib_mean), SD = sd(vib_mean))
df
ggplot(df, aes(x = male.sp, y = Mean,fill = male.sp)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()
graph2ppt(file = "amer_vibdur_compare_bar.pptx",width = 7,height = 5)

mod_amer_vibcomp <- lm(sqrt(vib_mean)~male.sp, 
                           data = hyb_amer_clean_cop)
# Summary of the model
Anova(mod_amer_vibcomp ,type = "II")
qqPlot(resid(mod_amer_vibcomp ))
emmeans(mod_amer_vibcomp ,pairwise~male.sp,adjust = "tukey",type = "response")


###intro mean
ggplot(data = hyb_amer_clean_cop, aes(x = male.sp,y = intro_mean,fill = male.sp))+
  geom_boxplot(alpha = .7,outlier.shape = NA)+theme_classic()+ylim(0,110)
df <- ddply(hyb_amer_clean_cop, c("male.sp"), summarize, Mean = mean(intro_mean), SD = sd(intro_mean))
df
ggplot(df, aes(x = male.sp, y = Mean,fill = male.sp)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()
graph2ppt(file = "amer_introdur_compare_bar.pptx",width = 7,height = 5)


mod_amer_introcomp <- lm(intro_mean~male.sp, 
                       data = hyb_amer_clean_cop)
# Summary of the model
Anova(mod_amer_introcomp ,type = "II")
qqPlot(resid(mod_amer_introcomp ))
emmeans(mod_amer_introcomp ,pairwise~male.sp,adjust = "tukey",type = "response")

