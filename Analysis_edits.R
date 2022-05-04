
rm(list=ls())
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(sf)
library(reldist)
library(GGally)
library(broom)
library(grid)
library(gridExtra)
library(plotly)
library(dineq)
library(car)
library(ggpubr)
library(mice)
library(ineq)


load("data/clean_data.rdata")
total_pop_msa<-dta %>% group_by(cbsa_name, cbsa) %>% 
  summarise(total_pop=sum(pop))


# first, calculate absolute ineq indicators by city
# Gap and ratio: need .9 and .1 weighted quantiles
# CV
# GINI from reldist package and mean log deviation from dineq

#we'll impute 1000 iterations using the mean le and se then calculate our measures, take median le and 2.5 & 97.5% 


#create var w/ mhi by decile (both 1-10 and 0-1 rescaled)
income_ineq_se<-dta %>% group_by(cbsa) %>% 
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  select(GEOID, decile_income, decile_income1, pop, cbsa)

#create 1000 iterations of the LE for each CT (using le and se)
imputed<-dta %>% group_by(GEOID) %>% 
  group_modify(~{
    dist=rnorm(1000, mean=.x$le, sd=.x$se)
    data.frame(
      le=dist, 
      iteration=1:1000)
  }) %>% left_join(income_ineq_se, by="GEOID") 

#calculate disparity measures 
absolute_inequities_long<-imputed %>% group_by(iteration, cbsa) %>% 
  group_modify(~{
    #.x<-imputed %>% filter(iteration==1, cbsa==33860)
    wtd.mean<-weighted.mean(.x$le, w=.x$pop)
    quants<-wtd.quantile(.x$le, q = c(.1, .9), weight = .x$pop)
    wtd.sd<-sum(.x$pop/sum(.x$pop) * (.x$le - wtd.mean)^2)
    gini<-gini(.x$le, weights=.x$pop)*100
    data.frame(dif=quants[2]-quants[1],
               ratio=quants[2]/quants[1],
               cv=wtd.sd/wtd.mean*100,
               gini=gini,
               mld.wt=mld.wtd(.x$le, weights=.x$pop)
               ) %>% as_tibble
  })

#find median, 97.5, 2.5 percentile 
summary<-absolute_inequities_long%>%
  group_by(cbsa)%>%
  summarize(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025)),
            med_cv=median(cv), 
            uci_cv=as.numeric(quantile(cv,probs=0.975)),
            lci_cv=as.numeric(quantile(cv,probs=0.025)),
            med_gini=median(gini),
            uci_gini=as.numeric(quantile(gini,probs=0.975)),
            lci_gini=as.numeric(quantile(gini,probs=0.025)),
            med_mld.wt=median(mld.wt), 
            uci_mld.wt=as.numeric(quantile(mld.wt,probs=0.975)),
            lci_mld.wt=as.numeric(quantile(mld.wt,probs=0.025)))%>%
  left_join(region)


jstr(summary)

#make longer 
absolute_ineq_long<-summary %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("med_dif", "uci_dif", "lci_dif",  "med_ratio", "uci_ratio", "lci_ratio" ,"med_cv", "uci_cv", "lci_cv", 
                                    "med_gini", "uci_gini", "lci_gini", "med_mld.wt", "uci_mld.wt", "lci_mld.wt"),
                     labels=c("Abs. Disparity", "97th% Abs. Disparity", "2.5% Abs. Disparity", "Rel. Disparity", "97th% Rel. Disparity", 
                              "2.5% Rel. Disparity", "Coefficient of Variation", "97th% Coefficient of Variation",
                              "2.5% Coefficient of Variation", "Gini", "97th% Gini" , "2.5% Gini",  "Mean Log Deviation", "97th% Mean Log Deviation", 
                              "2.5% Mean Log Deviation")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)

#limit to absolute and relative disparity measure for figure 1 
absolute_rel_ineq_long<-absolute_ineq_long%>%
  filter(type %in% c("Abs. Disparity", "97th% Abs. Disparity", "2.5% Abs. Disparity", "Rel. Disparity", "97th% Rel. Disparity", 
                     "2.5% Rel. Disparity"))%>%
  mutate(Region_Name=factor(Region_Name), 
         Region_Name=ordered(Region_Name, levels=c("Midwest", "South", "Northeast", "West")))

absolute_rel_ineq_long<-absolute_rel_ineq_long%>%
  arrange(value)


#find Coefficient of variation (CV) for each region and measure 
cv<-absolute_rel_ineq_long%>%
  group_by(type, Region_Name)%>%
  summarize(cv=sd(value) / mean(value) * 100)

totals<-absolute_rel_ineq_long%>%
group_by(type, Region_Name)%>%
 summarize(median=median(value))

##################################################################################
#####Repeat for income based inequities

#find the top/bottom ratio and top/bottom difference using MHI deciles, and the imputed (1000 iterations) data
decile_le<-imputed %>% group_by(iteration, cbsa, decile_income1) %>% 
  summarize(le=weighted.mean(le, w=pop))%>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)

#get the population pct by income and CBSA (and iteration) and cbsa mean Le and income mean le
between_group<-imputed%>%group_by(iteration, cbsa)%>%
  mutate(total_pop=sum(pop),
         mean_le=mean(le))%>%
  group_by(iteration, cbsa, decile_income)%>%
  mutate(income_pop=sum(pop), 
         pct_pop=income_pop/total_pop, 
         mean_le_inc=mean(le))%>%
  ungroup()
#calculate the between group variance 
BGV<-between_group%>%group_by(iteration, cbsa)%>%
  summarize(BGV=sum(pct_pop*(mean_le_inc-mean_le)^2))


#join BGV w /ratio/difference 
#find median, 97.5, and 2.5 percentile for the ratio/difference/BGV measures  
summary_income<-decile_le%>%
  left_join(BGV)%>%
  group_by(cbsa)%>%
  summarize(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025)), 
            med_BGV=median(BGV), 
            uci_BGV=as.numeric(quantile(BGV,probs=0.975)),
            lci_BGV=as.numeric(quantile(BGV,probs=0.025)))

#find SII and RII for income
income_ineq_v2<-imputed %>% group_by(cbsa, iteration) %>% 
  group_modify(~{
    #   if (.y$iteration==1) print(.y$cbsa)
    model_sii<-lm(le~decile_income1, data=.x) %>% tidy
    model_rii<-lm(log(le)~decile_income1, data=.x) %>% tidy
    bind_rows(model_sii %>% filter(term=="decile_income1") %>% select(estimate, std.error) %>% mutate(type="sii"),
              model_rii %>% filter(term=="decile_income1") %>% select(estimate, std.error) %>% mutate(type="rii"))
  })

#find median, 97.5, and 2.5 percentile for the SII and RII 
income_ineq_v3<-income_ineq_v2%>%
  group_by(cbsa, type) %>% 
  summarise(med=median(estimate), 
            uci=as.numeric(quantile(estimate,probs=0.975)), 
            lci=as.numeric(quantile(estimate,probs=0.025))) %>% 
  select(cbsa, type, med, lci, uci) %>% 
  mutate( med=ifelse(type=="rii", exp(med), med),
          uci=ifelse(type=="rii", exp(uci), uci), 
          lci=ifelse(type=="rii", exp(lci), lci))%>%
  pivot_wider(names_from=type, values_from=c("med", "uci", "lci"))

#merge the SII and RII w/ the top/bottom differnce/BGV and ratio
income_ineq<-summary_income%>%left_join(income_ineq_v3)%>%
  left_join(region)

#pivot longer 
income_ineq_long<-income_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("med_dif", "uci_dif", "lci_dif", "med_ratio", "uci_ratio", "lci_ratio", "med_BGV", "uci_BGV", "lci_BGV",  "med_sii","uci_sii", "lci_sii", "med_rii",  "uci_rii",  "lci_rii"),
                     labels=c("Top/Bottom Difference", "97th% Top/Bottom Difference", "2.5% Top/Bottom Difference", "Top/Bottom Ratio",
                              "97th% Top/Bottom Ratio", "2.5% Top/Bottom Ratio", "Between Group Variance", "97th% Between Group Variance", "2.5% Between Group Variance", 
                              "Slope Index of Inequality","97th% SII", "2.4% SII", "Relative Index of Inequality", "97th% RII", "2.5% RII")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)


#################################################################################
#repeat the absolute and relative disparity measures using life tables (conditional life expectancies)

#find pct of the pop
life_tables2<-life_tables1%>%
  mutate(total_pop=sum(pop),
         pct_pop=pop/total_pop)

#create mhi var for each age group 
income_ineq_se_lt<-life_tables2 %>% group_by(cbsa, age_grp) %>% 
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)), 
         decile_income1=(decile_income-1)/9)%>%
  select(GEOID, decile_income1, pop, cbsa, pct_pop, age_grp)

#impute for 1000 iterations for each ct by age group 
imputed_lt<-life_tables1 %>% group_by(GEOID, age_grp) %>% 
  group_modify(~{
    dist=rnorm(1000, mean=.x$le, sd=.x$se)
    data.frame(
      le=dist, 
      iteration=1:1000)
  }) %>% left_join(income_ineq_se_lt, by="GEOID") 

#find absolute disparity measures, for each age group (and each iteration)
absolute_inequities_long_test_lt<-imputed_lt %>% group_by(iteration, cbsa, age_grp) %>% 
  group_modify(~{
    quants<-wtd.quantile(.x$le, q = c(.1, .9), weight = .x$pop)
    wtd.mean<-weighted.mean(.x$le, w=.x$pop)
    wtd.sd<-sum(.x$pop/sum(.x$pop) * (.x$le - wtd.mean)^2)
    gini<-gini(.x$le, weights=.x$pop)*100
    wtd.mean<-weighted.mean(.x$le, w=.x$pop)
    data.frame(dif=quants[2]-quants[1],
               ratio=quants[2]/quants[1],
               cv=wtd.sd/wtd.mean*100,
               gini=gini,
               mld.wt=mld.wtd(.x$le, weights=.x$pop), 
               BGV=BGV) %>% as_tibble
  })

#find median, 97.5, and 2.5 percentile for each abs. disparity measure by cbsa and age-group 
summary_lt<-absolute_inequities_long_test_lt%>%
  group_by(cbsa, age_grp)%>%
  summarize(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025)),
            med_cv=median(cv), 
            uci_cv=as.numeric(quantile(cv,probs=0.975)),
            lci_cv=as.numeric(quantile(cv,probs=0.025)),
            med_gini=median(gini),
            uci_gini=as.numeric(quantile(gini,probs=0.975)),
            lci_gini=as.numeric(quantile(gini,probs=0.025)),
            med_mld.wt=median(mld.wt), 
            uci_mld.wt=as.numeric(quantile(mld.wt,probs=0.975)),
            lci_mld.wt=as.numeric(quantile(mld.wt,probs=0.025)))%>%
  left_join(region)


jstr(summary)

absolute_ineq_long_lt<-summary_lt %>% gather(type, value, -age_grp,-cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("med_dif", "uci_dif", "lci_dif",  "med_ratio", "uci_ratio", "lci_ratio" ,"med_cv", "uci_cv", "lci_cv", 
                                    "med_gini", "uci_gini", "lci_gini", "med_mld.wt", "uci_mld.wt", "lci_mld.wt"),
                     labels=c("Abs. Disparity", "97th% Abs. Disparity", "2.5% Abs. Disparity", "Rel. Disparity", "97th% Rel. Disparity", 
                              "2.5% Rel. Disparity", "Coefficient of Variation", "97th% Coefficient of Variation",
                              "2.5% Coefficient of Variation", "Gini", "97th% Gini" , "2.5% Gini",  "Mean Log Deviation", "97th% Mean Log Deviation", 
                              "2.5% Mean Log Deviation")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)

absolute_rel_ineq_long_lt<-absolute_ineq_long_lt%>%
  filter(type %in% c("Abs. Disparity", "97th% Abs. Disparity", "2.5% Abs. Disparity", "Rel. Disparity", "97th% Rel. Disparity", 
                     "2.5% Rel. Disparity"))%>%
  mutate(Region_Name=factor(Region_Name), 
         Region_Name=ordered(Region_Name, levels=c("Midwest", "South", "Northeast", "West")))

absolute_rel_ineq_long_lt<-absolute_rel_ineq_long_lt%>%
  arrange(value)

str(absolute_rel_ineq_long)

#find Coefficient of variation (CV) for each region and measure and age group
cv_lt<-absolute_rel_ineq_long_lt%>%
  group_by(age_grp, type, Region_Name)%>%
  summarize(cv=sd(value) / mean(value) * 100)

totals<-absolute_rel_ineq_long_lt%>%
  group_by(age_grp, type, Region_Name)%>%
  summarize(median=median(value))


#####################################################################################
#Repeat the life tables (conditional life expectancies) for income based disparities

#find top/bottom difference and ratio for each iteration of CT and cbsa 
decile_le_lt<-imputed_lt %>% group_by(iteration, cbsa, decile_income1, age_grp) %>% 
  summarize(le=weighted.mean(le, w=pop))%>% 
  filter(decile_income1%in%c(0, 1))%>%
  pivot_wider(names_from=decile_income1, names_prefix="le_decile", values_from="le")%>%
  mutate(dif=le_decile1-le_decile0, 
         ratio=le_decile1/le_decile0)


#Find the Between Group variance 
BGV<-imputed%>%group_by(iteration, cbsa, age_grp)%>% 
  group_modify(~{
    means_pops<-.x%>%
      mutate(total_pop=sum(pop),
            mean_le=mean(le),
      decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)))%>%
      group_by(decile_income)%>%
      mutate(income_pop=sum(pop), 
             pct_pop=income_pop/total_pop, 
             mean_le_inc=mean(le))%>%
      ungroup()
    BGV<-sum(means_pops$pct_pop*(means_pops$mean_le_inc-means_pops$mean_le)^2)
    data.frame(
      BGV=BGV
    )
  })

#join BGV w /ratio/difference 
#find median, 97.5, and 2.5 percentile for the ratio/difference/BGV measures for each cbsa and age group  
summary_income<-decile_le%>%
  left_join(BGV)%>%
  group_by(cbsa, age_grp)%>%
  summarize(med_dif=median(dif), 
            uci_dif=as.numeric(quantile(dif,probs=0.975)),
            lci_dif=as.numeric(quantile(dif,probs=0.025)),
            med_ratio=median(ratio),
            uci_ratio=as.numeric(quantile(ratio, probs=0.975)),
            lci_ratio=as.numeric(quantile(ratio, probs=0.025)), 
            med_BGV=median(BGV), 
            uci_BGV=as.numeric(quantile(BGV,probs=0.975)),
            lci_BGV=as.numeric(quantile(BGV,probs=0.025)))


#find sii and rii for each iteration and age group
income_ineq_v2_lt<-imputed_lt %>% group_by(cbsa, iteration, age_grp) %>% 
  group_modify(~{
    #   if (.y$iteration==1) print(.y$cbsa)
    model_sii<-lm(le~decile_income1, data=.x) %>% tidy
    model_rii<-lm(log(le)~decile_income1, data=.x) %>% tidy
    bind_rows(model_sii %>% filter(term=="decile_income1") %>% select(estimate, std.error) %>% mutate(type="sii"),
              model_rii %>% filter(term=="decile_income1") %>% select(estimate, std.error) %>% mutate(type="rii"))
  })

#find median, 97.5, and 2.5 percentile for rii and sii for each age group
income_ineq_v3_lt<-income_ineq_v2_lt%>%
  group_by(cbsa, type, age_grp) %>% 
  summarise(med=median(estimate), 
            uci=as.numeric(quantile(estimate,probs=0.975)), 
            lci=as.numeric(quantile(estimate,probs=0.025))) %>% 
  select(cbsa, type, med, lci, uci) %>% 
  mutate( med=ifelse(type=="rii", exp(med), med),
          uci=ifelse(type=="rii", exp(uci), uci), 
          lci=ifelse(type=="rii", exp(lci), lci))%>%
  pivot_wider(names_from=type, values_from=c("med", "uci", "lci"))

#combine top/bottom ratio/difference and SII and RII 
income_ineq_lt<-summary_income_lt%>%left_join(income_ineq_v3_lt)%>%
  left_join(region)

income_ineq_long_lt<-income_ineq_lt %>% gather(type, value, -age_grp,  -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("med_dif", "uci_dif", "lci_dif", "med_ratio", "uci_ratio", "lci_ratio", "med_BGV", "uci_BGV", "lci_BGV", "med_sii","uci_sii", "lci_sii", "med_rii",  "uci_rii",  "lci_rii"),
                     labels=c("Top/Bottom Difference", "97th% Top/Bottom Difference", "2.5% Top/Bottom Difference", "Top/Bottom Ratio",
                              "97th% Top/Bottom Ratio", "2.5% Top/Bottom Ratio","Between Group Variance", "97th% Between Group Variance", "2.5% Between Group Variance",
                              "Slope Index of Inequality","97th% SII", "2.4% SII", "Relative Index of Inequality", "97th% RII", "2.5% RII")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)

income_ineq_long_lt<-income_ineq_long_lt%>%
  filter(type %in% c("Top/Bottom Difference", "97th% Top/Bottom Difference", "2.5% Top/Bottom Difference", "Top/Bottom Ratio",
                     "97th% Top/Bottom Ratio", "2.5% Top/Bottom Ratio"))%>%
  mutate(Region_Name=factor(Region_Name), 
         Region_Name=ordered(Region_Name, levels=c("Midwest", "South", "Northeast", "West")))

income_ineq_long_lt<-income_ineq_long_lt%>%
  arrange(value)

str(absolute_rel_ineq_long)
#find Coefficient of variation (CV) for each region and measure 
cv_lt<-income_ineq_long_lt%>%
  group_by(age_grp, type, Region_Name)%>%
  summarize(cv=sd(value) / mean(value) * 100)

totals<-income_ineq_long_lt%>%
  group_by(age_grp, type, Region_Name)%>%
  summarize(median=median(value))


####################################################################################
#                       Figures/Tables                                            #
####################################################################################

#-----Figure 1-----

my_breaks <- function(y) { if (max(y) > 2) seq(0, 12.5, 2.5) else seq(1, 1.16, 0.2) }

f1a<-absolute_rel_ineq_long%>%
  filter(type=="Abs. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=0)+
  labs(x="",
       y="Value",
       title="Absolute Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
f1a
ggsave("results/figure1_ASM.pdf", f1a, width=20, height=7.5)

f1b<-absolute_rel_ineq_long%>%
  filter(type=="Rel. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=1)+
  scale_y_continuous(trans="log") +
  labs(x="",
       y="Value",
       title="Relative Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_blank(),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
f1b

library(gridExtra)

figure1<-grid.arrange(f1a,f1b,
                      ncol = 2, nrow = 1)
g <- arrangeGrob(f1a,f1b,  nrow=1) #generates g
ggsave(g, file="results/figure1.pdf", width=15, height=10) #saves g

#view specific MSAs
ggplotly(f1a)


#median LE disparities by region (figure 1)

medians<-absolute_rel_ineq_long%>%group_by(Region_Name, type)%>%
  summarize(median=median(value), 
            mean=mean(value),
            sd=sd(value), 
            cv=sd/mean*100)

############################################################################
#Table 1 -------

#create dataset of just the 25% and 97.5% of absolute disp and make wide format
ci_abs<-absolute_rel_ineq_long%>%
  filter(type %in% c( "97th% Abs. Disparity", "2.5% Abs. Disparity"))%>%
  pivot_wider(names_from=type, values_from=value)%>%
  rename(uci="97th% Abs. Disparity", lci="2.5% Abs. Disparity")%>%
  select(cbsa_name, uci, lci)

#join in the ci's, limit to cbsa w/ pop over 1 million, rank absolute disparities
summary_large_abs<-absolute_ineq_long%>% 
   filter(total_pop>=1000000) %>% 
  filter( type %in%c("Abs. Disparity")) %>% 
  arrange(desc(value)) %>% 
  group_by(type) %>%
  mutate(rank=row_number())%>%
  ungroup()%>%
  select(cbsa_name, Region_Name, rank, value)%>%
  left_join(ci_abs, by='cbsa_name')%>%
  rename(total_dif=value)%>%
  mutate(Region_Name=sub(" Region", "", Region_Name))%>%
  mutate(total_dif=format(total_dif, digits=1, nsmall=1))

ci_rel<-absolute_rel_ineq_long%>%
  filter(type %in% c( "97th% Rel. Disparity", "2.5% Rel. Disparity"))%>%
  pivot_wider(names_from=type, values_from=value)%>%
  rename(uci="97th% Rel. Disparity", lci="2.5% Rel. Disparity")%>%
  select(cbsa_name, uci, lci)

summary_large_rel<-absolute_ineq_long %>% 
  filter(total_pop>=1000000) %>% 
  filter( type=="Rel. Disparity") %>% 
  arrange(desc(value)) %>% 
  group_by(type) %>% 
  mutate(rank=row_number())%>%
  ungroup()%>%
  select(cbsa_name,Region_Name, rank, value )%>%
  left_join(ci_rel, by='cbsa_name')%>%
  rename(total_ratio=value)%>%
  mutate(Region_Name=sub(" Region", "", Region_Name))%>%
  mutate(total_ratio=format(total_ratio, digits=2, nsmall=2))

table1<-cbind(summary_large_abs, summary_large_rel)
fwrite(table1, file="results/table1_ASM.csv")


#################################################################################
#Figure 2----

#FINAL MAP NOT CREATED IN R- THE CODE BELOW IS  A SIMPLIFIED VERSION
full_dta<-absolute_ineq_long %>% select(cbsa, type, value, Region_Name) %>% 
  filter(type %in% c("Abs. Disparity", "Rel. Disparity"))%>%
  mutate(ineq="Total") %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Disparity", "Total: Rel. Disparity")))
shp<-read_sf("Data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
  mutate(cbsa=as.numeric(GEOID))
regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
  states<-read_sf("Data/cb_2013_us_state_20m/cb_2013_us_state_20m.shp")%>%
    subset(STATEFP%in% c(23, 55))

#export full_dta file as csv for map(but only absolute and relative)
dta_abs_rel<-full_dta%>%
  select(cbsa, Region_Name, type2, value)%>%
  pivot_wider(names_from=type2, values_from=value)

write.csv(dta_abs_rel,"Data/dta_abs_rel.csv", row.names = FALSE)

  
shp_with_data<-inner_join(shp, full_dta)
bbox_temp<-st_bbox(shp_with_data)
figure2<-ggplot()+
  geom_sf(data=shp_with_data, size=0,
          aes(geometry=geometry, color=rank, fill=rank))+
  geom_sf(data=regions, size=0.5, color="black", 
          fill=NA,
          aes(geometry=geometry))+
  geom_sf(data=states, color="black",fill="black")+
  scale_fill_binned(name="Rank", type="gradient",
                    show.limits=T,n.breaks=5,labels=round,
                    low="red", high="white")+
  scale_color_binned(name="Rank", type="gradient",
                     show.limits=T,n.breaks=5,labels=round,
                     low="red", high="white")+
  coord_sf(xlim = c(bbox_temp["xmin"], bbox_temp["xmax"]),
           ylim = c(bbox_temp["ymin"], bbox_temp["ymax"]), expand = T) +
  guides(alpha=F, size=F, color=F)+
  theme_void()+
  theme(plot.title = element_blank(),
        strip.text = element_text(size=10, face="bold", hjust=.5),
        panel.background = element_rect(fill = "grey", color=NA),
        legend.position="bottom")
figure2
ggsave("results/figure2.pdf", width=12, height=5)

#just to view which areas have largest disparities 
absolute_ineq_long1<-absolute_ineq_long%>%
  arrange(type, value)

##############################################################################
# Table 2

#view highest and lowest disp by region and level 
view<-le_by_decile%>%
  arrange(Region, decile_income, le)

#Table 2- Analysis examining Pop size and MHI as predictors of disparities 

#join the cbsa dataset (pop & MHI w/ absolute, relative inequities measure)
#the total_pop and pop differ-- more likely to trust pop since it's from tidycensus rather than sum of CT


cbsa_inequities<-absolute_inequities_long%>%
  right_join(region)%>%
  rename(GEOID=cbsa)%>%
  left_join(cbsa, by="GEOID")%>%
  mutate(poplog=log(pop),
         mhilog=log(mhi),
         mhi_cat=as.numeric(cut(mhi, breaks=c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000, 140000, 150000, 160000, 170000, 180000, 190000, 200000), right=T, 
                                labels=c(1, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 110000, 120000, 130000,140000, 150000, 160000, 170000, 180000, 190000))))%>%
  mutate(region=factor(Region, levels=c(4, 1, 2, 3)))



str(cbsa_inequities)

cbsa_abs<-cbsa_inequities%>%
  select(type=="dif")%>%
  mutate(region=factor(Region, levels=c(4, 1, 2, 3)))

summary(cbsa_abs$mhi)
summary(cbsa_abs)

#exploratory visualization (scatter plots)
absmhi<-ggplot(cbsa_inequities, aes(x=mhi, y=value))+geom_point()
absmhi
logmhi<-ggplot(cbsa_inequities, aes(x=mhilog, y=value))+geom_point()
logmhi
abspop<-ggplot(cbsa_inequities, aes(x=poplog, y=value))+geom_point()

#regress inequities on absolute disparities



#use rubin's rule for SE's
regressions<-cbsa_inequities %>% group_by(iteration) %>% 
  group_modify(~{
    model_pop<-lm(dif~log(pop), data=.x)%>% tidy
    model_mhi<-lm(dif~log(mhi), data=.x)%>% tidy
    model_region<-lm(dif~region, data=.x)%>% tidy
    bind_rows(model_pop %>% filter(term=="log(pop)") %>% select(estimate, std.error) %>% mutate(type="pop"),
              model_mhi %>% filter(term=="log(mhi)") %>% select(estimate, std.error) %>% mutate(type="mhi"), 
              model_region %>% filter(term=="region1")%>% select(estimate, std.error) %>% mutate(type="Northeast"), 
              model_region %>% filter(term=="region2")%>% select(estimate, std.error) %>% mutate(type="Midwest"), 
              model_region %>% filter(term=="region3")%>% select(estimate, std.error) %>% mutate(type="South"))
  })


regresssions_pooled<-regressions%>% group_by(type)%>%
   summarize(beta=mean(estimate), 
          var_wtn=mean(std.error^2),
          var_btw=var(estimate),
          se=sqrt(var_wtn+var_btw), 
          lci=beta-(1.96*se),
          uci=beta+(1.96*se))%>%
  select(type, beta, lci, uci)

#find the estimate for a 10% larger population
0.3197116*log(1.10)

###############################################################################
#-------Figure 3 ------


#use imputed data (iterations 1000) to find weighted mean, then take mean of that le by cbsa and decile income
le_by_decile<-imputed %>% group_by(iteration, cbsa, decile_income) %>% 
  summarize(le=weighted.mean(le, w=pop))%>%
  ungroup()%>%
  group_by(cbsa, decile_income)%>%
  summarize(le=mean(le))%>%
  left_join(total_pop_msa) %>% left_join(region)


#now find mean, sd, cv by decile for total population   
cv_decile_tot<-le_by_decile%>%
  group_by(decile_income)%>%
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(5))

#find mean, sd, cv, by decile for each region 
cv_decile<-le_by_decile %>% group_by(Region, decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(Region))          

cv_decile1<-cv_decile%>%
 bind_rows(cv_decile_tot)%>%
mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 

figure3cv<-cv_decile1%>%
  filter(type=="cv")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Coefficient of Variation",
        x="Decile of Median Household Income",
       y="CV of Life Expectancy", 
       color="Region")+
 # guides(color=F, fill=F)+
  scale_y_continuous(limits=c(0, 5), breaks=seq(0, 5, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())
figure3cv


figure3sd<-cv_decile1%>%
  filter(type=="sd")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region),show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
   # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Standard Deviation",
       x="Decile of Median Household Income",
       y="SD of Life Expectancy (years)", 
       color="Region")+
 scale_y_continuous(limits=c(0, 4), breaks=seq(0, 4, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  #  facet_grid(~type)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
            axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3sd 
  
figure3mean<-cv_decile1%>%
  filter(type=="mean")%>%
  ggplot( aes(x=decile_income, y=value, group=Region)) +
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom= "point",group=1)+
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom="line",group=1)+
    geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Mean",
       x="Decile of Median Household Income",
       y="Life Expectancy (years)", 
       color="Region")+
  guides(color=F, fill=F)+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3mean
  
library(ggpubr)
ggarrange(figure3mean,figure3sd, figure3cv, ncol = 1, nrow=3 )
 

ggsave("results/figure3.pdf", width=8, height=7.5)
ggplotly(figure3mean)
ggplotly(figure3cv)
ggplotly(figure3sd)



#view highest and lowest disp by region and level 
view<-le_by_decile%>%
  arrange(Region, decile_income, le)

##############################################################

#find the mean, sd, cv for each decile within regions
cv_decile<-le_by_decile %>% group_by(Region_Name, decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)

fwrite(cv_decile, "results/cv_decile.csv")

#repeat without grouping by region

cv_decile_tot<-le_by_decile %>% group_by(decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)

fwrite(cv_decile_tot, "results/cv_decile_tot.csv")

################################################################################
#Descriptives:  -----

#mean differences by income 
mean<-le_by_decile%>%
group_by(Region_Name, decile_income)%>%
  summarize(mean=mean(le), 
            sd=sd(le))

figure3<- ggplot()+ 
  geom_line(data=mean, aes(x=decile_income, y=mean))+
  geom_point(data=mean, aes(x=decile_income, y=mean))+
  facet_wrap(~Region_Name)

ggplotly(figure3)

## MSA per region 

prop.table(table(region$Region_Name))

#population per region
pop_region<-total_pop_msa%>%left_join(region)%>%ungroup()%>%
  mutate(pop=sum(total_pop))%>%
  group_by(Region_Name)%>%
 summarize(pop_region=sum(total_pop),
           pop_region_pct=pop_region/pop)

#for conclusion-- find 10% and 90th percentile for AMES iowa
ames<-dta%%
  filter(cbsa==11180)

wtd.quantile(ames$le, q = c(.1, .9), weight = ames$pop)

quantile(ames$le, probs = seq(.1, .9, by = .1))

#for results - find years of gap for san jose
sj<-dta%>%
  filter(cbsa==41940)%>%
  arrange(le)

wtd.quantile(sj$le, q = c(.1, .9), weight = sj$pop)

quantile(sj$le, probs = seq(.1, .9, by = .1))


########################################################################################################################################################
#-----------------------------Appendix ----------------------------------------------------------------------------------------------------------------
########################################################################################################################################################


####Appendix FIgure 1
## correlation between indicators
full_dta<-bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Total"),
                    income_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Income"))  %>%
  filter(type%in%c("Abs. Disparity", "Rel. Disparity",
                   "Coefficient of Variation", "Gini", "Mean Log Deviation", 
                   "Top/Bottom Difference", "Top/Bottom Ratio","Between Group Variance",
                   "Slope Index of Inequality", "Relative Index of Inequality"))%>%
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>%
  group_by(type2) %>%
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                      "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Deviation",
                                      "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio", "Income: Between Group Variance", 
                                      "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                      labels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                               "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.", 
                               "Income: Top/Bottom Difference", "Income:Top/Bottom Ratio","Income: BGV",
                               "Income: SII", "Income: RII"))) %>%
  left_join(region) %>%
  select(cbsa, type2, value, Region_Name) %>% 
  spread(type2, value)
cols<-c("Total: Abs. Disparity", "Total: Rel. Disparity",
        "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.",
        "Income: Top/Bottom Difference", "Income:Top/Bottom Ratio", "Income: BGV",
        "Income: SII", "Income: RII")
corrs<-ggpairs(data=full_dta, 
               columns = cols, upper = list(continuous = wrap("cor", size=6, color="black", stars=F))) +
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text = element_text(color="black", size=12, face="bold"),
        axis.text=element_text(color="black"))
ggsave(filename="results/Appendix_figure1.pdf", corrs, width=20, height=15)

###############################################################################
#Appendix Figure 2######
#Figure 1 repeated with income disparity measure 

absolute_rel_income_ineq_long<-income_ineq_long%>%
  filter(type %in% c("Top/Bottom Difference", "Top/Bottom Ratio"))%>%
  mutate(Region_Name=factor(Region_Name), 
         Region_Name=ordered(Region_Name, levels=c("Midwest", "South", "Northeast", "West")))

my_breaks <- function(y) { if (max(y) > 2) seq(0, 12.5, 2.5) else seq(1, 1.16, 0.2) }

appen_f1a<-absolute_rel_income_ineq_long%>%
  filter(type=="Top/Bottom Difference")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=0)+
  labs(x="",
       y="Value",
       title="Top/Bottom Difference")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
appen_f1a
ggsave("results/Appendixfigure1a_ASM.pdf", f1a, width=20, height=7.5)

appen_f1b<-absolute_rel_income_ineq_long%>%
  filter(type=="Top/Bottom Ratio")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=1)+
  scale_y_continuous(trans="log") +
  labs(x="",
       y="Value",
       title="Top/Bottom Ratio")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=18, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_blank(),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))
appen_f1b
library(gridExtra)

figure1<-grid.arrange(appen_f1a,appen_f1b,
                      ncol = 2, nrow = 1)
g <- arrangeGrob(appen_f1a,appen_f1b,  nrow=1) #generates g
ggsave(g, file="results/Appendix_figure2.pdf", width=15, height=10) #saves g

#view specific MSAs
ggplotly(f1a)

medians_inc<-absolute_rel_income_ineq_long%>%
  filter(type%in%c("Top/Bottom Difference", "Top/Bottom Ratio"))%>%
  group_by(Region_Name, type)%>%
  summarize(median=median(value), 
            mean=mean(value),
            sd=sd(value), 
            cv=sd/mean*100)


#################################################################################
####Conditional Life Expectancy
#APpendix Figure 3 
#absolute and relative disparity 

f1a_lt<-absolute_rel_ineq_long_lt%>%
  filter(type=="Abs. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=0)+
  labs(x="",
       y="Value",
       title="Absolute Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=14, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_text(face="bold", size=20),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))+
  facet_wrap(~age_grp)
f1a_lt
ggsave("results/figure1_ASM.pdf", f1a, width=20, height=7.5)

f1b_lt<-absolute_rel_ineq_long_lt%>%
  filter(type=="Rel. Disparity")%>%
  ggplot(aes(x=Region_Name, y=value))+
  geom_boxplot(aes(group=as.factor(Region_Name)), fill=NA, outlier.color = NA, width=0.5)+
  geom_jitter(aes(fill=as.factor(Region_Name), size=total_pop), 
              width=0.1, height=0, alpha=1,
              color="black", pch=21) +
  guides(color=F, fill=F, size=F)+
  scale_fill_discrete()+
  scale_colour_discrete()+
  geom_hline(lty=2, yintercept=1)+
  scale_y_continuous(trans="log") +
  labs(x="",
       y="Value",
       title="Relative Disparity")+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(50, "points"),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(size=14, color="black"),
        axis.text.y=element_text(size=18, color="black"),
        axis.title.y=element_blank(),
        strip.text =element_text(face="bold", size=20),
        strip.background = element_blank(),
        plot.title=element_text(size=18, hjust=0.5))+
  facet_wrap(~age_grp)
f1b_lt

library(gridExtra)

figure1<-grid.arrange(f1a_lt,f1b_lt,
                      ncol = 2, nrow = 2)
g_lt <- arrangeGrob(f1a_lt,f1b_lt,  nrow=2) #generates g
ggsave(g_lt, file="results/appendix_figure3_conditional.pdf", width=15, height=10) #saves g

###############################################################################
##APpendix figure 4
#map but w/ relative disparity


#######APPENDIX Figure 5
##############################################################################
#figure 3 repeated w/ mhi standardized across the full US, not MSA-- some cbsa's won't have observations in all deciles

#use imputed data (iterations 1000) to find weighted mean, then take mean of that le by cbsa and decile income
mhi_nation<-dta1 %>% 
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))%>%
  select(GEOID, decile_income, pop, cbsa, pct_pop)

#create 1000 iterations of the LE for each CT (using le and se)
imputed_mhi_nation<-dta %>% group_by(GEOID) %>% 
  group_modify(~{
    dist=rnorm(1000, mean=.x$le, sd=.x$se)
    data.frame(
      le=dist, 
      iteration=1:1000)
  }) %>% left_join(mhi_nation, by="GEOID") 


le_by_decile_v2<-imputed_mhi_nation %>% group_by(iteration, cbsa, decile_income) %>% 
  summarize(le=weighted.mean(le, w=pop))%>%
  ungroup()%>%
  group_by(cbsa, decile_income)%>%
  summarize(le=mean(le))%>%
  left_join(total_pop_msa) %>% left_join(region)

cv_decile_v2<-le_by_decile_v2 %>% group_by(Region, decile_income) %>% 
  summarize(mean=mean(le, na.rm=T), 
            sd=sd(le, na.rm=T), 
            cv=sd/mean*100, na.rm=T)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(Region)) 

cv_decile_tot_v2<-le_by_decile_v2%>%
  group_by(decile_income)%>%
  summarize(mean=mean(le, na.rm=T), 
            sd=sd(le, na.rm=T), 
            cv=sd/mean*100, na.rm=T)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(5))


cv_decile1_v2<-cv_decile_v2%>%
  bind_rows(cv_decile_tot_v2)%>%
  mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 

figure3cv_mhi<-cv_decile1_v2%>%
  filter(type=="cv")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Coefficient of Variation (CV)",
       x="Decile of Median Household Income",
       y="CV of Life Expectancy", 
       color="Region")+
  # guides(color=F, fill=F)+
  scale_y_continuous(limits=c(0, 5), breaks=seq(0, 5, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())
figure3cv_mhi


figure3sd_mhi<-cv_decile1_v2%>%
  filter(type=="sd")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region),show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Standard Deviation (SD)",
       x="Decile of Median Household Income",
       y="SD of Life Expectancy (years)", 
       color="Region")+
  scale_y_continuous(limits=c(0, 4), breaks=seq(0, 4, by=1))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  #  facet_grid(~type)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3sd_mhi 

figure3mean_mhi<-cv_decile1_v2%>%
  filter(type=="mean")%>%
  ggplot( aes(x=decile_income, y=value, group=Region)) +
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom= "point",group=1)+
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom="line",group=1)+
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Mean",
       x="Decile of Median Household Income",
       y="Life Expectancy (years)", 
       color="Region")+
  guides(color=F, fill=F)+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())

figure3mean_mhi

library(ggpubr)
ggarrange(figure3mean_mhi,figure3sd_mhi, figure3cv_mhi, ncol = 1, nrow=3 )


ggsave("results/appen_figure4_mhi_nation.pdf", width=8, height=7.5)
ggplotly(figure3mean)
ggplotly(figure3cv)
ggplotly(figure3sd)


###############################################################################
#APpendix Figure 6
#conditional life expectancies, repeating Figure 3 


le_by_decile_v2<-imputed_lt %>%  group_by(iteration, cbsa, age_grp)%>% 
  mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))%>%
  group_by(decile_income, cbsa, age_group)%>%
  summarise(le=weighted.mean(le, w=pop))%>%
  ungroup()%>%
  summarise(le=mean(le))%>%
  left_join(total_pop_msa)%>% left_join(region)


cv_decile_lt<-le_by_decile_lt %>% group_by(age_grp, Region, decile_income) %>% 
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(Region)) 

cv_decile_tot_lt<-le_by_decile%>%
  group_by(age_grp, decile_income)%>%
  summarize(mean=mean(le), 
            sd=sd(le), 
            cv=sd/mean*100)%>%
  pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
  mutate(Region=factor(5))


cv_decile1_lt<-cv_decile_lt%>%
  bind_rows(cv_decile_tot_lt)%>%
  mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 

figure3cv_con<-cv_decile1_lt%>%
  filter(type=="cv")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Coefficient of Variation",
       x="Decile of Median Household Income",
       y="CV of Life Expectancy", 
       color="Region")+
  # guides(color=F, fill=F)+
  #  scale_y_continuous(limits=c(0, 5), breaks=seq(0, 5, by=1))+
  #  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())+
  facet_wrap(~age_grp)
figure3cv_con


figure3sd_con<-cv_decile1_lt%>%
  filter(type=="sd")%>%
  ggplot(aes(x=decile_income, y=value, group=Region)) +
  geom_line(aes(color=Region),show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Standard Deviation",
       x="Decile of Median Household Income",
       y="SD of Life Expectancy (years)", 
       color="Region")+
  # scale_y_continuous(limits=c(0, 4), breaks=seq(0, 4, by=1))+
  #  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=1))+
  #  facet_grid(~type)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())+
  facet_wrap(~age_grp)

figure3sd_con

figure3mean_con<-cv_decile1_lt%>%
  filter(type=="mean")%>%
  ggplot( aes(x=decile_income, y=value, group=Region)) +
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom= "point",group=1)+
  stat_summary(aes(y = value,group=1), fun=mean, colour="black", geom="line",group=1)+
  geom_line(aes(color=Region), show.legend = F)+
  geom_point(aes(fill=Region), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  scale_fill_manual(name="Region", labels=c("Midwest", "South","Northeast", "West", "Overall"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  scale_colour_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF","Black"))+
  labs(title="Mean",
       x="Decile of Median Household Income",
       y="Life Expectancy (years)", 
       color="Region")+
  guides(color=F, fill=F)+
  #  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  #  scale_x_continuous(limits=c(1, 10), breaks=seq(1, 10 , by=2))+
  theme_bw() +
  theme(legend.position = "bottom", axis.text=element_text(color="black", size=14),
        axis.title.x = element_blank(),
        axis.title=element_text(color="black", face="bold", size=10),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())+
  facet_wrap(~age_grp)

figure3mean_con

library(ggpubr)
ggarrange(figure3mean_con,figure3sd_con, figure3cv_con, ncol = 1, nrow=3 )


ggsave("results/appendix_figure5_conditional.pdf", width=8, height=7.5)
ggplotly(figure3mean)
ggplotly(figure3cv)
ggplotly(figure3sd)




# App Data----

#Note for RAN-- I've updated table 1 (and all calculations) to include the life expectancy SE's. So for Table 1 I'm now
# including the 2.5 and 97.5%'s - can you please include in the app table 1? 

# ___Table 1 ----
{
  top1<-summary_absolute %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  bottom1<-summary_absolute %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  top2<-summary_income %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  bottom2<-summary_income %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
  table1<-bind_cols(bind_rows(top1%>% arrange(rank), 
                              data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                              bottom1 %>% arrange(rank)) ,
                    bind_rows(top2%>% arrange(rank), 
                              data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                              bottom2 %>% arrange(rank)) )
  
  summary_large<-bind_rows(absolute_ineq_long, income_ineq_long) %>% 
    # filter(total_pop>=1000000) %>% 
    arrange(type, value) %>% 
    group_by(type) %>% 
    mutate(rank=row_number()) %>% 
    group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
    summarise(rank=mean(rank)) %>% 
    arrange(desc(rank)) %>% 
    ungroup() %>% 
    mutate(rank=row_number())
  df_table1<-full_join(absolute_ineq %>% 
                         ungroup() %>%
                         rename(total_dif=dif,
                                total_ratio=ratio), 
                       income_ineq) %>% left_join(total_pop_msa) %>% 
    # filter(total_pop>=1000000) %>% 
    left_join(summary_large %>% select(cbsa, rank)) %>% 
    select(cbsa_name,total_pop, Region_Name, rank, 
           total_dif, total_ratio, cv, gini,mld.wt,
           dif, ratio, sii, rii) %>% arrange(rank) %>% 
    mutate(Region_Name=sub(" Region", "", Region_Name)) %>% 
    ### Formatting for dashboard table
    rename(Name=cbsa_name,
           Region=Region_Name,
           Rank=rank,
           abs_diff=total_dif,
           abs_ratio=total_ratio,
           abs_cv=cv,
           abs_gini=gini,
           abs_mld=mld.wt,
           income_diff = dif,
           income_ratio = ratio,
           income_sii= sii,
           income_rii = rii
    ) %>% 
    mutate(abs_mld =round(abs_mld,5),
           income_rii = round(income_rii,3)) %>% 
    mutate_at(vars(abs_diff,
                   abs_ratio,
                   abs_cv, abs_gini,
                   income_diff, income_ratio,
                   income_sii),
              ~round(.x,2)) %>% 
    select(-Rank) %>% 
    arrange(desc(total_pop))
  df_table1
}


# ___Figure 1 ----
{
  library(glue)
  df_absolute_ineq_long=absolute_ineq_long %>% ungroup()%>% 
    mutate(outcome = "total")
  df_income_ineq_long=income_ineq_long %>% ungroup() %>% 
    mutate(outcome = "income")
  df_fig1 = bind_rows(df_absolute_ineq_long, df_income_ineq_long)%>% 
    group_by(outcome,type) %>% 
    group_modify(~.x %>% 
                   mutate(value_rounded = round(value,3)) %>% 
                   mutate(value = round(value,4)) %>% 
                   group_by(Region_Name) %>% 
                   mutate(median = median(value)) %>% 
                   ungroup() %>% 
                   arrange(desc(median)) ) %>% 
    ungroup() %>% 
    mutate(tooltip = glue(
      '<b>{cbsa_name}</b>
      {type}: {value_rounded} 
      Population: {format(total_pop, big.mark = ",")} 
      ',
    ) %>% as.character()) %>% 
    select(outcome, type,Region_Name, value,total_pop, tooltip ) %>% 
    mutate(outcome = str_to_title(outcome)) 
}

# ___Figure 2 ----
{
  library(HatchedPolygons)
  ## Shapes Files
  sf_cbsa_raw<-read_sf("data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
    mutate(cbsa=as.numeric(GEOID))
  sf_cbsa = sf_cbsa_raw %>% 
    select(cbsa) %>% 
    filter(cbsa%in%(full_dta %>% ungroup() %>% pull(cbsa)))
  sf_regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
  sf_states<-read_sf("Data/cb_2013_us_state_20m/cb_2013_us_state_20m.shp") %>% mutate(state = STUSPS )
  get_hatch_holes = function(sf_state_tmp){
    state_tmp = unique(sf_state_tmp$state)
    sf_polygon_missing_all=st_cast(sf_state_tmp, "POLYGON")
    sf_polygon_missing = sf_polygon_missing_all %>% 
      mutate(area = st_area(sf_polygon_missing_all)) %>% 
      filter(area == max(area)) %>% 
      mutate(state = state_tmp)
    spa = as(sf_polygon_missing, "Spatial")
    res = HatchedPolygons::hatched.SpatialPolygons(spa,density = 2) 
    res %>% st_as_sf()
  }
  sf_ME_hatch = get_hatch_holes(sf_states %>% filter(STUSPS == "ME"))
  sf_WI_hatch = get_hatch_holes( sf_states %>% filter(STUSPS == "WI"))
  sf_fig2_missing_hatched =  bind_rows(sf_ME_hatch,sf_WI_hatch)
  sf_fig2_states_missing = sf_states %>% filter(STUSPS%in%c("ME","WI"))

  ## Data for map
  library(shiny)
  xwalk_region = df_absolute_ineq_long %>% select(cbsa, Region, Region_Name) %>% distinct()
  df_fig2 = bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                        mutate(ineq="Total"),
                      income_ineq_long %>% select(cbsa, type, value) %>% 
                        mutate(ineq="Income")) %>% 
    mutate(type2=paste0(ineq, ": ", type)) %>% 
    arrange(desc(value)) %>%
    group_by(type2) %>%
    mutate(rank=row_number()) %>% 
    mutate(type2=factor(type2, levels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                        "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Deviation",
                                        "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio",
                                        "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                        labels=c("Total: Abs. Disparity", "Total: Rel. Disparity",
                                 "Total: Coefficient of Variation", "Total: Gini", "Total: Mean Log Dev.",
                                 "Income: Top/Bottom Difference", "Income: Top/Bottom Ratio",
                                 "Income: SII", "Income: RII"))) %>%
    left_join(xwalk_region) %>% 
    ungroup() %>% 
    left_join(sf_cbsa_raw %>%
                as.data.frame() %>% 
                select(cbsa, NAME) %>% 
                distinct()) %>% 
    mutate(label = str_c("<b>",NAME,"</b><br>",
                         "<b>",ineq," ",type," :</b>", round(value,2),"<br>",
                         "<b>Rank :</b>",rank
    ) %>% 
      map(~HTML(.x))) %>% 
    mutate(type2 = paste0(ineq,": ", type))  

}

# ___Figure 3 ----
{ ## Figure 3 data
  le_by_decile<-dta %>% group_by(cbsa) %>% 
    group_modify(~{
      #.x<-dta %>% filter(cbsa==25940)
      .x<-.x %>% 
        mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))
      decile_le<-.x %>% group_by(decile_income) %>% 
        summarise(le=weighted.mean(le, w=pop))
      decile_le
    }) %>% left_join(total_pop_msa) %>% left_join(region) %>% ungroup() # %>%  filter(total_pop>=1000000)
  
  #Figure 3b
  
  cv_decile<-le_by_decile %>% group_by(Region, decile_income) %>% 
    summarize(mean=mean(le), 
              sd=sd(le), 
              cv=sd/mean*100)%>%
    pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
    mutate(Region=factor(Region)) %>% ungroup()
  
  cv_decile_tot<-le_by_decile%>%
    group_by(decile_income)%>%
    summarize(mean=mean(le), 
              sd=sd(le), 
              cv=sd/mean*100)%>%
    pivot_longer(cols=c("mean", "sd", "cv"), names_to="type", values_to="value")%>%
    mutate(Region=factor(5))
  
  
  df_fig3 <-cv_decile%>%
    bind_rows(cv_decile_tot)%>%
    mutate(Region=ordered(Region, levels=c(2, 3, 1, 4,5), labels=c("Midwest", "South", "Northeast", "West", "Overall"))) 
  
}

# ___UI elements -----
df_fig1_choices_type = df_fig1 %>% 
  count(outcome,type)%>% 
  mutate(outcome = str_to_title(outcome)) %>% 
  mutate(type2 = paste0(outcome,": ", type)) %>% 
  mutate(type = as.character(type)) %>% 
  select(-n)

df_intro_home = read.csv("App/introJS/df_intro_home.csv") %>% as.data.frame() %>% select(-X)
###  Save data for App
save(df_fig1_choices_type,
     file= "App/cleaned_le_income_cities_UIelements.rdata" )
save(
  df_table1,   ## Table 1 
  df_fig1,     ## Figure 1
  df_fig2,     ## Figure 2
  df_fig3,sf_fig2_missing_hatched,sf_fig2_states_missing,     ## Figure 3
  sf_regions,sf_cbsa, ## Shape Files
  file= "App/cleaned_le_income_cities_appBundle.rdata")

