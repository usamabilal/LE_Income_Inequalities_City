# Set up-----
rm(list=ls())
  library(data.table)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(cowplot)
  library(sf)
  library(reldist)
  library(GGally)
  library(broom)
  library(grid)
  library(gridExtra)
  load("data/clean_data.rdata")
  dta = dta %>% as_tibble()
  total_pop_msa<-dta %>% group_by(cbsa_name, cbsa) %>% 
    summarise(total_pop=sum(pop))

# Figure 1: LE Inequities -----
# first, lets calculate absolute ineq indicators by city
# Gap and ratio: need .9 and .1 weighted quantiles
# CV
# GINI from reldist package
absolute_ineq<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    quants<-wtd.quantile(.x$le, q = c(.1, .9), weight = .x$pop)
    wtd.mean<-weighted.mean(.x$le, w=.x$pop)
    wtd.sd<-sum(.x$pop/sum(.x$pop) * (.x$le - wtd.mean)^2)
    gini<-gini(.x$le, weights=.x$pop)*100
    data.frame(dif=quants[2]-quants[1],
               ratio=quants[2]/quants[1],
               cv=wtd.sd/wtd.mean*100,
               gini=gini) %>% as_tibble
  }) %>% left_join(region)
  
absolute_ineq_long<-absolute_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("dif", "ratio", "cv", "gini"),
                     labels=c("Abs. Difference", "Rel. Difference",
                              "Coefficient of Variation", "Gini")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)
# Gap and ratio: need LE at .9 and .1 weighted quantiles of income
# SII, RII
income_ineq<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    .x<-.x %>% 
      mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, probs=seq(0, 1, by=0.1)), include.lowest = T)))
    decile_le<-.x %>% group_by(decile_income) %>% 
      summarise(le=weighted.mean(le, w=pop)) %>% 
      filter(decile_income%in%c(1, 10)) %>% pull(le)
    mean_le<-weighted.mean(.x$le, w=.x$pop)
    model<-lm(le~decile_income, data=.x) %>% tidy
    sii<-model %>% filter(term=="decile_income") %>% pull(estimate)
    rii<-sii/mean_le
    data.frame(dif=decile_le[2]-decile_le[1],
               ratio=decile_le[2]/decile_le[1],
               sii=sii, 
               rii=rii) %>% as_tibble
  }) %>% left_join(region)

income_ineq_long<-income_ineq %>% gather(type, value, -cbsa, -Region, -Region_Name) %>% 
  mutate(type=factor(type, levels=c("dif", "ratio", "sii", "rii"),
                     labels=c("Abs. Difference", "Rel. Difference",
                              "Slope Index of Inequality", "Relative Index of Inequality")),
         Region_Name=sub(" Region", "", Region_Name)) %>% 
  left_join(total_pop_msa)
# Figure 2: Correlogram-----
## 
full_dta_fig2<-bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Total"),
                    income_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Income")) %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Difference", "Total: Rel. Difference",
                                      "Total: Coefficient of Variation", "Total: Gini",
                                      "Income: Abs. Difference", "Income: Rel. Difference",
                                      "Income: Slope Index of Inequality", "Income: Relative Index of Inequality"),
                      labels=c("Total: Abs. Difference", "Total: Rel. Difference",
                               "Total: Coefficient of Variation", "Total: Gini",
                               "Income: Abs. Difference", "Income: Rel. Difference",
                               "Income: SII", "Income: RII"))) %>% 
  left_join(region) %>% 
  select(cbsa, type2, value, Region_Name) %>% 
  spread(type2, value)

# ggsave(filename="results/figure2.pdf", corrs, width=20, height=15)

# Figure 3: Map -----
full_dta<-bind_rows(absolute_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Total"),
                    income_ineq_long %>% select(cbsa, type, value) %>% 
                      mutate(ineq="Income")) %>% 
  mutate(type2=paste0(ineq, ": ", type)) %>% 
  arrange(desc(value)) %>% 
  group_by(type2) %>% 
  mutate(rank=row_number()) %>% 
  mutate(type2=factor(type2, levels=c("Total: Abs. Difference", "Total: Rel. Difference",
                                      "Total: Coefficient of Variation", "Total: Gini",
                                      "Income: Abs. Difference", "Income: Rel. Difference",
                                      "Income: Slope Index of Inequality", "Income: Relative Index of Inequality")))

shp<-read_sf("data/cb_2013_us_cbsa_20m/cb_2013_us_cbsa_20m.shp") %>% 
  mutate(cbsa=as.numeric(GEOID))
regions<-read_sf("Data/cb_2013_us_region_20m/cb_2013_us_region_20m.shp")
shp_with_data<-inner_join(shp, full_dta)
bbox_temp<-st_bbox(shp_with_data)
 


# get LE at each decile
le_by_decile<-dta %>% group_by(cbsa) %>% 
  group_modify(~{
    #.x<-dta %>% filter(cbsa==25940)
    .x<-.x %>% 
      mutate(decile_income=as.numeric(cut(mhi, breaks=quantile(mhi, seq(0, 1, by=0.1)), include.lowest = T)))
    decile_le<-.x %>% group_by(decile_income) %>% 
      summarise(le=weighted.mean(le, w=pop))
    decile_le
  }) %>% left_join(total_pop_msa) %>% left_join(region) #%>%  filter(total_pop>=1000000)

ggplot(le_by_decile, 
       aes(x=decile_income, y=le, group=cbsa)) +
  geom_line(data=le_by_decile %>% mutate(Region_Name="Midwest Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="Northeast Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="South Region"), 
            color="gray", alpha=1)+
  geom_line(data=le_by_decile %>% mutate(Region_Name="West Region"), 
            color="gray", alpha=1)+
  geom_line(aes(color=Region_Name))+
  geom_point(aes(fill=Region_Name), size=2, color="black", pch=21)+
  # annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, arrow=arrow(type="closed"), color="darkgreen", size=2)+
  # annotate("segment", x=-Inf, xend=Inf, y=67.5, yend=67.5, arrow=arrow(type="closed"), color="darkblue", size=2)+
  # annotate("text", label="Higher Income", x=4, y=68, vjust=0, hjust=.5, color="darkblue", fontface="bold", size=5)+
  # annotate("text", label="Increased Longevity", x=-0.2, y=77.5, angle=90, vjust=0, hjust=.5, color="darkgreen", fontface="bold", size=5)+
  labs(x="Decile of Median Household Income",
       y="Life Expectancy (years)")+
  scale_y_continuous(limits=c(67, 85.8), breaks=seq(70, 85, by=5))+
  scale_x_continuous(limits=c(-0.2, 11), breaks=seq(0, 10 , by=2))+
  facet_wrap(~Region_Name)+
  guides(color=F, fill=F)+
  theme_bw() +
  theme(axis.text=element_text(color="black", size=14),
        axis.title=element_text(color="black", face="bold", size=16),
        strip.text=element_text(color="black", face="bold", size=16),
        strip.background = element_blank())
# ggsave("results/figure4.pdf", width=10, height=7.5)
# Tables  -----
summary_absolute<-absolute_ineq_long %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
summary_income<-income_ineq_long %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
# > 1M
summary_absolute_large<-absolute_ineq_long %>% 
  # filter(total_pop>=1000000) %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())
summary_income_large<-income_ineq_long %>% 
  # filter(total_pop>=1000000) %>% 
  arrange(type, value) %>% 
  group_by(type) %>% 
  mutate(rank=row_number()) %>% 
  group_by(cbsa_name, cbsa, total_pop, Region, Region_Name) %>% 
  summarise(rank=mean(rank)) %>% 
  arrange(desc(rank)) %>% 
  ungroup() %>% 
  mutate(rank=row_number())

# top 10 and bottom 10, total and income, all and >1M
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


top1<-summary_absolute_large %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
bottom1<-summary_absolute_large %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
top2<-summary_income_large %>% arrange(rank) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
bottom2<-summary_income_large %>% arrange(desc(rank)) %>% slice(1:10) %>% select(cbsa_name, Region_Name, rank)
table2<-bind_cols(bind_rows(top1%>% arrange(rank), 
                            data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                            bottom1 %>% arrange(rank)) ,
                  bind_rows(top2%>% arrange(rank), 
                            data.frame(cbsa_name="...", Region_Name="...", rank=NA),
                            bottom2 %>% arrange(rank)) )

# final table: list of all cities >1M sorted by their average rank in both total and income based inequalities
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
table1<-full_join(absolute_ineq %>% rename(total_dif=dif,
                                           total_ratio=ratio), 
                  income_ineq) %>% left_join(total_pop_msa) %>% 
  # filter(total_pop>=1000000) %>% 
  left_join(summary_large %>% select(cbsa, rank)) %>% 
  select(cbsa_name, Region_Name, rank, 
         total_dif, total_ratio, cv, gini,
         dif, ratio, sii, rii) %>% arrange(rank) %>% 
  mutate(Region_Name=sub(" Region", "", Region_Name)) %>% 
  mutate(total_dif=format(total_dif, digits=1, nsmall=1),
         total_ratio=format(total_ratio, digits=2, nsmall=2),
         cv=paste0(format(cv, digits=1, nsmall=1), "%"),
         gini=format(gini, digits=2, nsmall=2),
         dif=format(dif, digits=1, nsmall=1),
         ratio=format(ratio, digits=2, nsmall=2),
         sii=format(sii, digits=2, nsmall=2),
         rii=format(rii, digits=2, nsmall=2))
fwrite(table1, file="results/table1.csv")


# Save Data -----

absolute_ineq_long=absolute_ineq_long %>% ungroup()%>% 
  mutate(outcome = "total")
income_ineq_long=income_ineq_long %>% ungroup() %>% 
  mutate(outcome = "income")
tidy_ineq = bind_rows(absolute_ineq_long,
                    income_ineq_long)

save(tidy_ineq,
     full_dta_fig2,
     regions,shp_with_data,bbox_temp,
     table1,
     file= "../App (Dev)/cleaned_bundle.rdata")

# save(income_ineq_long ,
#      absolute_ineq_long ,
#      tidy_ineq
#      full_dta_fig2,
#      regions,shp_with_data,bbox_temp,
#      table1,
#      file= "cleaned_bundle.rdata")