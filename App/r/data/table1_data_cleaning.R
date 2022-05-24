#'################################################################################### 
#' This script cleans the data structures required for table 1 of dashboard
#' It is split into several sections
#'    @section-1: pulls the rank + value + CI of abs/rel metrics for LE and Income
#'                DS: df_le_diff + df_le_ratio
#'    @section-2: pulls other LE metrics: 1) CV, GINI, mean log dev, BGV
#'    @seciont-3: pulls other Income metrics: 1) SII 2) RII
#'    @seciont-4: compiles + formats table1 data structure for app
#'################################################################################### 


# # 1. Rank + CI -----
# { 
#   # __ 1.1 LE ----
#   {
#     # ___ 1.1a Diff ----
#     {
#       #create dataset of just the 25% and 97.5% of absolute disp and make wide format
#       ci_abs<-absolute_rel_ineq_long%>%
#         filter(type %in% c( "97th% Abs. Disparity", "2.5% Abs. Disparity"))%>%
#         pivot_wider(names_from=type, values_from=value)%>%
#         rename(uci="97th% Abs. Disparity", lci="2.5% Abs. Disparity")%>%
#         select(cbsa_name, uci, lci)
#       
#       #join in the ci's, limit to cbsa w/ pop over 1 million, rank absolute disparities
#       summary_large_abs<-absolute_ineq_long%>% 
#         # filter(total_pop>=1000000) %>% 
#         filter( type %in%c("Abs. Disparity")) %>% 
#         arrange(desc(value)) %>% 
#         group_by(type) %>%
#         mutate(rank=row_number())%>%
#         ungroup()%>%
#         select(cbsa_name, Region_Name, rank, value)%>%
#         left_join(ci_abs, by='cbsa_name')%>%
#         rename(total_dif=value)%>%
#         mutate(Region_Name=sub(" Region", "", Region_Name))%>%
#         mutate(total_dif=format(total_dif, digits=1, nsmall=1)) 
#       
#       df_le_diff = summary_large_abs %>% 
#         select(Name = cbsa_name, Region = Region_Name, 
#                abs_diff_rank = rank,
#                abs_diff = total_dif,
#                abs_diff_uci = uci,
#                abs_diff_lci = lci)
#     }
#     # ___ 1.1b Ratio ----
#     {
#       ci_rel<-absolute_rel_ineq_long%>%
#         filter(type %in% c( "97th% Rel. Disparity", "2.5% Rel. Disparity"))%>%
#         pivot_wider(names_from=type, values_from=value)%>%
#         rename(uci="97th% Rel. Disparity", lci="2.5% Rel. Disparity")%>%
#         select(cbsa_name, uci, lci)
#       
#       summary_large_rel<-absolute_ineq_long %>% 
#         # filter(total_pop>=1000000) %>% 
#         filter( type=="Rel. Disparity") %>% 
#         arrange(desc(value)) %>% 
#         group_by(type) %>% 
#         mutate(rank=row_number())%>%
#         ungroup()%>%
#         select(cbsa_name,Region_Name, rank, value )%>%
#         left_join(ci_rel, by='cbsa_name')%>%
#         rename(total_ratio=value)%>%
#         mutate(Region_Name=sub(" Region", "", Region_Name))%>%
#         mutate(total_ratio=format(total_ratio, digits=2, nsmall=2))
#       
#       df_le_ratio = summary_large_rel %>% 
#         select(Name = cbsa_name, Region = Region_Name, 
#                abs_ratio_rank = rank,
#                abs_ratio = total_ratio,
#                abs_ratio_uci = uci,
#                abs_ratio_lci = lci)
#     }
#   }
#   # __ 1.2 LE ----
#   {
#     # ___ 1.1a Diff ----
#     {
#       #create dataset of just the 25% and 97.5% of absolute disp and make wide format
#       ci_abs<-absolute_rel_ineq_long%>%
#         filter(type %in% c( "97th% Abs. Disparity", "2.5% Abs. Disparity"))%>%
#         pivot_wider(names_from=type, values_from=value)%>%
#         rename(uci="97th% Abs. Disparity", lci="2.5% Abs. Disparity")%>%
#         select(cbsa_name, uci, lci)
#       
#       #join in the ci's, limit to cbsa w/ pop over 1 million, rank absolute disparities
#       summary_large_abs<-absolute_ineq_long%>% 
#         # filter(total_pop>=1000000) %>% 
#         filter( type %in%c("Abs. Disparity")) %>% 
#         arrange(desc(value)) %>% 
#         group_by(type) %>%
#         mutate(rank=row_number())%>%
#         ungroup()%>%
#         select(cbsa_name, Region_Name, rank, value)%>%
#         left_join(ci_abs, by='cbsa_name')%>%
#         rename(total_dif=value)%>%
#         mutate(Region_Name=sub(" Region", "", Region_Name))%>%
#         mutate(total_dif=format(total_dif, digits=1, nsmall=1)) 
#       
#       df_le_diff = summary_large_abs %>% 
#         select(Name = cbsa_name, Region = Region_Name, 
#                abs_diff_rank = rank,
#                abs_diff = total_dif,
#                abs_diff_uci = uci,
#                abs_diff_lci = lci)
#     }
#     # ___ 1.1b Ratio ----
#     {
#       ci_rel<-absolute_rel_ineq_long%>%
#         filter(type %in% c( "97th% Rel. Disparity", "2.5% Rel. Disparity"))%>%
#         pivot_wider(names_from=type, values_from=value)%>%
#         rename(uci="97th% Rel. Disparity", lci="2.5% Rel. Disparity")%>%
#         select(cbsa_name, uci, lci)
#       
#       summary_large_rel<-absolute_ineq_long %>% 
#         # filter(total_pop>=1000000) %>% 
#         filter( type=="Rel. Disparity") %>% 
#         arrange(desc(value)) %>% 
#         group_by(type) %>% 
#         mutate(rank=row_number())%>%
#         ungroup()%>%
#         select(cbsa_name,Region_Name, rank, value )%>%
#         left_join(ci_rel, by='cbsa_name')%>%
#         rename(total_ratio=value)%>%
#         mutate(Region_Name=sub(" Region", "", Region_Name))%>%
#         mutate(total_ratio=format(total_ratio, digits=2, nsmall=2))
#       
#       df_le_ratio = summary_large_rel %>% 
#         select(Name = cbsa_name, Region = Region_Name, 
#                abs_ratio_rank = rank,
#                abs_ratio = total_ratio,
#                abs_ratio_uci = uci,
#                abs_ratio_lci = lci)
#     }
#   }
# }
# 
# 2. LE metrics ----
{
  absolute_ineq_long %>% count(type)
  df_table_le = absolute_ineq_long %>% 
    mutate(type_short = type %>% dplyr::recode(
      "Abs. Disparity"="abs_diff",
      "97th% Abs. Disparity"="abs_diff_uci",
      "2.5% Abs. Disparity" = "abs_diff_lci",
      "Rel. Disparity"="abs_ratio",
      "97th% Rel. Disparity"="abs_ratio_uci",
      "2.5% Rel. Disparity"="abs_ratio_lci",
      "Coefficient of Variation"="abs_cv",
      "97th% Coefficient of Variation"="abs_cv_uci",
      "2.5% Coefficient of Variation"="abs_cv_lci",
      "Gini"="abs_gini",
      "97th% Gini"="abs_gini_uci",
      "2.5% Gini"="abs_gini_lci",
      "Mean Log Deviation"="abs_mld",
      "97th% Mean Log Deviation"="abs_mld_uci",
      "2.5% Mean Log Deviation"="abs_mld_lci"
    )) %>% 
    select(Name = cbsa_name, total_pop, Region = Region_Name,type_short, value) %>% 
    pivot_wider(names_from = 'type_short', values_from = 'value')
}

# 3. Income metrics ----
{
  income_ineq_long %>% count(type)
  df_table_income = income_ineq_long %>% 
    mutate(type_short = type %>% dplyr::recode(
      "Top/Bottom Difference"="income_diff",
      "97th% Top/Bottom Difference"="income_diff_uci",
      "2.5% Top/Bottom Difference" = "income_diff_lci",
      "Top/Bottom Ratio"="income_ratio",
      "97th% Top/Bottom Ratio"="income_ratio_uci",
      "2.5% Top/Bottom Ratio"="income_ratio_lci",
      "Between Group Variance"="income_bgv",
      "97th% Between Group Variance"="income_bgv_uci",
      "2.5% Between Group Variance"="income_bgv_lci",
      "Slope Index of Inequality"="income_sii",
      "97th% SII"="income_sii_uci",
      "2.4% SII"="income_sii_lci",
      "Relative Index of Inequality"="income_rii",
      "97th% RII"="income_rii_uci",
      "2.5% RII"="income_rii_lci"
    )) %>% 
    select(Name = cbsa_name,type_short, value)  %>% 
    pivot_wider(names_from = 'type_short', values_from = 'value')
}

# 4. Compile----
{ 
  df_table1_values = df_table_le %>% 
    left_join(df_table_income) %>% 
    mutate_at(vars(contains('abs_mld')), ~round(.x,5)) %>% 
    mutate_at(vars(contains('income_rii')), ~round(.x,3)) %>% 
    mutate_at(vars(-c(Name, total_pop, Region, abs_mld,contains('abs_mld'),contains('income_rii'))),
              ~round(.x,2)) %>% 
    arrange(desc(total_pop))
  
    ## Widen CI info
  df_table1_formatted = df_table1_values %>% 
    pivot_longer(cols = -c(Name, total_pop, Region), names_to = 'type') %>% 
    mutate(value_type = case_when(
      str_detect(type,'lci')~'lci',
      str_detect(type,'uci')~'uci',
      TRUE~'value'),
      type = str_remove(type,"_uci") %>% str_remove('_lci')
    ) %>% 
    pivot_wider(names_from = 'value_type', values_from = 'value') %>% 
    mutate(value_formatted = glue("{value} [{lci}-{uci}]"),
           type = glue("{type}_formatted")) %>% 
    select(Name, type, value_formatted) %>% 
    pivot_wider(names_from = type, values_from = value_formatted)
  
  df_table1 = df_table1_values %>% left_join(df_table1_formatted) %>% 
    select(-contains('uci'),
           -contains('lci'))
}

