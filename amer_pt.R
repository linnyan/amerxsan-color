library(lme4)
library(ggplot2)
library(car)
library(factoextra)
#library(DFA.CANCOR)
library(MASS)
library(lubridate)

paint = read.csv("pt_all.csv")
summary(paint)
paint$color = as.factor(paint$color)
paint$sp = as.factor(paint$sp)
paint$time = period_to_seconds(hm(paint$time))
### Figure 3: bar plot to see mating success----
ggplot(data=paint, aes(x=color,fill = as.factor(cop_count)))+
  geom_bar(alpha = 0.5,size = 1,position="fill",color = "black")+
  theme_classic()+
  scale_fill_manual(values=c("white","black"))+
  geom_hline(yintercept=0.5, linetype = "dashed",size=1)+
  geom_text(stat='count', aes(label=..count..), position = "fill",vjust = -0.5)+
  facet_wrap(~sp)#san 0.04
##chi-square

a = c(19,2)
b = c(12,8)
tab = rbind(a,b)
chisq.test(tab)

### Figure 4: compare courtship components----
paint_court1 = subset(paint, intro_count > 0)
df <- ddply(paint_court1, c("color","cop_count","sp"), summarize, Mean = mean(vib_mean), SD = sd(vib_mean))
df
ggplot(df, aes(x = color, y = Mean,fill = as.factor(cop_count))) +
  geom_bar(stat="identity",position = "dodge",alpha = .5,color = "black") + 
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.1,color = "grey40",
                position = position_dodge(width = 0.9))+
  theme_classic()+facet_wrap(~sp)

df <- ddply(paint_court1, c("color","cop_count","sp"), summarize, Mean = mean(intro_mean), SD = sd(intro_mean))
df
ggplot(df, aes(x = color, y = Mean,fill = as.factor(cop_count))) +
  geom_bar(stat="identity",position = "dodge",alpha = .5,color = "black") + 
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SD), width = 0.1,color = "grey40",
                position = position_dodge(width = 0.9))+
  theme_classic()+facet_wrap(~sp)
graph2ppt(file = "pt_introdur.pptx",width = 7,height = 5)
#Bootstrap----
paint_court1$pair = paste(paint_court1$color,paint_court1$sp,sep = " ")

bootstrap_data <- function(data, strata_var1, strata_var2, numeric_var) {
  bootstrap_results_df <- data.frame(
    Pair = character(),
    Cop = character(),
    Mean = numeric(),
    CI_Lower = numeric(),
    CI_Upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  combinations <- expand.grid(unique(data[[strata_var1]]), unique(data[[strata_var2]]))
  colnames(combinations) <- c(strata_var1, strata_var2)
  
  mean_function <- function(data, indices) {
    d <- data[indices]
    mean(d)
  }
  
  for(i in 1:nrow(combinations)) {
    subset_data <- subset(data, data[[strata_var1]] == combinations[i, strata_var1] & data[[strata_var2]] == combinations[i, strata_var2])
    
    if(nrow(subset_data) > 0) {
      boot_res <- boot(subset_data[[numeric_var]], statistic = mean_function, R = 1000)
      ci <- boot.ci(boot_res, type = c("perc"))
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
bootstrap_results_df <- bootstrap_data(paint_court1, "pair", "cop_count", "vib_mean")
bs_pt_intro_mean =  bootstrap_data(paint_court1, "pair", "cop_count", "intro_mean")
plot_bootstrap_results <- function(bootstrap_results_df, measure) {
  
  ggplot(bootstrap_results_df, aes(x = Pair, y = Mean, linetype = Cop,shape = Cop)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +
    labs(x = "Cop", y = measure, title = "Color Manipulation -- Bootstrapped Means and CIs") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic()
}

plot_bootstrap_results(bootstrap_results_df, "Multimodal Phase Duration (s)")
graph2ppt(file = "pt_vibdur_bs.pptx",width = 7,height = 5)
plot_bootstrap_results(bs_pt_intro_mean,"Visual Phase Duration (s)")
graph2ppt(file = "pt_introdur_bs.pptx",width = 7,height = 5)
