
#packages----

library(tidyverse)
library(readr)
library(psych)
library(lavaan)
library(sjPlot)
library(naniar)
library(semPlot)
library(flextable)
library(stringr)
library(officer)
library(apaTables)
library(semTools)

#data import----
data_import <- read_csv("data.csv")
View(data_import) #total participants 

#data preparation ----

#filter people who fulfill prerequisites and create sum scores for scales
data_all <- data_import %>% 
  filter(terapia_skusenost == 1 | terapia_skusenost == 2) %>%  
  # mutate(across(starts_with("c_nip"), ~ . * -1)) %>% (due to mistake in coding during the administration the data had to be reversed so they correspond to the format of C-NIP)
  mutate(td_cd = c_nip_1 + c_nip_2 + c_nip_3 + c_nip_4 + c_nip_5, 
         ei_er = c_nip_6 + c_nip_7 + c_nip_8 + c_nip_9 + c_nip_10,
         pao_pro= c_nip_11 + c_nip_12 + c_nip_13,
         ws_fc = c_nip_14 + c_nip_15 + c_nip_16 + c_nip_17 + c_nip_18) 


#more data preparation - dataset specific 

data_all <- rowid_to_column(data_all, var = "case")
cnip <- data_all %>% select(c_nip_1:c_nip_18)
cnip_missing <- as.data.frame(miss_case_summary(cnip)) 
data_fullinfo <- merge(data_all, cnip_missing, by = "case") 
data <- data_fullinfo %>% filter(pct_miss < 100)
cnip <- data %>% select(c_nip_1:c_nip_18)


clean_to_months <- function(x) {
  num <- as.numeric(str_extract(x, "\\d+"))
  
  case_when(
    str_detect(str_to_lower(x), "year|years|rok|roky|leta|let") ~ num * 12,
    TRUE ~ num
  )
}
clean_sessions <- function(x) {
  
  if (is.na(x)) return(NA_real_)
  
  x_low <- tolower(x)  # ← ADD THIS LINE
  
  if (str_detect(x_low, "neviem|unknown|none|nic|n/a")) {
    return(NA_real_)
  }
  if (str_detect(x_low, "\\d+\\s*[-–—]\\s*\\d+") ||
      str_detect(x_low, "\\d+\\s*až\\s*\\d+")) {
    
    nums <- str_extract_all(x_low, "\\d+")[[1]]
    return(mean(as.numeric(nums)))
  }
  
  num <- as.numeric(str_extract(x_low, "\\d+"))
  return(num)
}

data <- data %>%
  mutate(
    terapia_dlzka = clean_to_months(terapia_dlzka),
    terapia_since = clean_to_months(terapia_since),
    terapia_pocet = sapply(terapia_pocet, clean_sessions)
  )

data$terapia_pocet[data$terapia_pocet > 200] <- NA

#data preparation for testing model 2
data_fivefactor <- cnip %>% select(!c(c_nip_5, c_nip_10, c_nip_15)) 
data_fivefactor <- data_fivefactor %>% mutate(td_cd = c_nip_1 + c_nip_2 + c_nip_3 + c_nip_4,
                                              ei_er = c_nip_6 + c_nip_9,
                                              im_ni = c_nip_7 + c_nip_8,  
                                              pao_pro = c_nip_11 + c_nip_12 + c_nip_13, 
                                              ws_fc = c_nip_14 + c_nip_16 + c_nip_17 + c_nip_18) 
                                              



#descriptive statistics ----

# demographics and sample descriptives------------------------------------------------------------

experience <- data %>% group_by(terapia_skusenost) %>% 
  count()
terapia_now <- data %>% group_by(terapia_now) %>% 
  count()
terapia_dlzka_demo <- data %>% summarise(mean_dlzka = mean(data$terapia_dlzka, na.rm = T), 
                                        sd_dlzka = sd(terapia_dlzka, na.rm = T), 
                                        median_dlzka = median(terapia_dlzka, na.rm = T))
terapia_range <- range(data$terapia_dlzka, na.rm = T)
terapia_since_demo <- data %>% summarise(mean_dlzka = mean(data$terapia_since, na.rm = T), 
                                         sd_dlzka = sd(terapia_since, na.rm = T), 
                                         median_dlzka = median(terapia_since, na.rm = T))
terapia_since_range <- range(data$terapia_since, na.rm = T) 
terapia_pocet_demo <- data %>% summarise(mean_sedeni = mean(terapia_pocet, na.rm = T),
                                         sd_sedeni = sd(terapia_pocet, na.rm = T),
                                         md_sedeni = median(terapia_pocet, na.rm = T))

gender <- data %>% group_by(gender) %>% 
  count()
education <- data %>% group_by(education) %>% 
  count()
age <- suppressWarnings(data %>% summarise(mean_age = mean(as.numeric(age), na.rm = T), 
                                  sd_age = sd(age, na.rm = T)))
  

#cnip descriptives ----

data %>% summarise(
                          
                           td_cd_mean = mean(td_cd, na.rm = T), 
                           td_cd_sd = sd(td_cd, na.rm = T), 
                           td_cd_min = min(td_cd, na.rm = T), 
                           td_cd_max = max(td_cd, na.rm = T), 
                           #emotional intensiy vs emotional reserve
                           ei_er_mean = mean(ei_er, na.rm = T), 
                           ei_er_sd = sd(ei_er, na.rm = T), 
                           ei_er_min = min(ei_er, na.rm = T), 
                           ei_er_max = max(ei_er, na.rm = T), 
                            #past vs present orientation
                          pao_pro_mean = mean(pao_pro, na.rm = T), 
                          pao_pro_sd = sd(pao_pro, na.rm = T), 
                          pao_pro_min = min(pao_pro, na.rm = T), 
                          pao_pro_max = max(pao_pro, na.rm = T), 
                          #warm support vs focused challange 
                          ws_fc_mean = mean(ws_fc, na.rm = T),
                          ws_fc_sd = sd(ws_fc, na.rm = T), 
                          ws_fc_min = min(ws_fc, na.rm = T), 
                          ws_fc_max = max (ws_fc, na.rm = T))

# cut offs ----------------------------------------------------------------

# cut offs based on Cooper 

cut_off_cooper <- function(x) {
  low_emp <- quantile(x, probs = 0.25, na.rm = T)
  standard <- x - mean(x, na.rm = T)/sd(x, na.rm =T)
  low_sd <- quantile(standard, probs = 0.25, na.rm = T)
  low_cut <- ((low_emp + low_sd)/2)
  
  high_emp <- quantile(x, probs = 0.75, na.rm = T)
  high_sd <- quantile(standard, probs = 0.75, na.rm = T)
  high_cut <- ((high_emp + high_sd)/2)
  
  output <- list(low_cut, high_cut)
  return(output)
}


cut_off_cooper(data_fivefactor$td_cd)
cut_off_cooper(data_fivefactor$ei_er)
cut_off_cooper(data_fivefactor$im_ni)
cut_off_cooper(data$pao_pro)
cut_off_cooper(data_fivefactor$ws_fc)

mean(data_fivefactor$td_cd, na.rm = T)
mean(data_fivefactor$ei_er, na.rm = T)
mean(data_fivefactor$im_ni, na.rm = T)
mean(data_fivefactor$pao_pro, na.rm = T)
mean(data_fivefactor$ws_fc, na.rm = T)


# cut offs for better interpretation - empirical 

cut_off <- function(x) {
  low_x <- floor(quantile(x, probs = 0.25, na.rm =T))
  high_x <- ceiling(quantile(x, probs = 0.75, na.rm =T))
  output <- list(low_x, high_x)
  return(output)
}

cut_off(data_fivefactor$td_cd)
cut_off(data_fivefactor$ei_er)
cut_off(data_fivefactor$im_ni)
cut_off(data$pao_pro)
cut_off(data_fivefactor$ws_fc)



# cnip correlations -------------------------------------------------------
cnip_data <- data.frame(
  td_cd   = data$td_cd,
  ei_er   = data$ei_er,
  pao_pro = data$pao_pro,
  ws_fc   = data$ws_fc
)

corellations <- corr.test(cnip_data, use = "pairwise")
apa.cor.table(cnip_data, filename = "correlation_table.doc")

# reliability -------------------------------------------------------------

td_cd <- cnip %>% select(c_nip_1:c_nip_5)
psych::reliability(td_cd)

ei_er <- cnip %>% select(c_nip_6:c_nip_10)
psych::reliability(ei_er)

pao_pro <- cnip %>% select(c_nip_11:c_nip_13)
psych::reliability(pao_pro)

ws_fc <- cnip %>% select(c_nip_14:c_nip_18)
psych::reliability(ws_fc)


# item analysis ---------------


factors_cnip <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,4)
item_analysis <- tab_itemscale(cnip, factor.groups = factors_cnip, file = "itemanalysis.odt", show.kurtosis = T)

#creating item graphs 
a <- alpha(ws_fc)
desc <- describe(ws_fc
                 )

item_table <- data.frame(
  Item     = rownames(desc),
  Median   = desc$median,
  M        = round(desc$mean, 2),
  SD       = round(desc$sd, 2),
  Skew     = round(desc$skew, 2),
  Kurtosis = round(desc$kurtosis, 2),
  r_total  = round(a$item.stats$r.cor, 2),
  alpha_drop = round(a$alpha.drop$raw_alpha, 2)
)

ft <- flextable(item_table) %>%
  set_header_labels(
    Item       = "Položka",
    Median     = "Md",
    M          = "M",
    SD         = "SD",
    Skew       = "Šikmosť",
    Kurtosis   = "Špicatosť",
    r_total    = "r s total",
    alpha_drop = "α bez položky"
  ) %>%
  theme_booktabs() %>%
  autofit()


doc <- read_docx() %>%
  body_add_flextable(ft)
print(doc, target = "item_analyza4.docx")



item_1 <- plot_frq(cnip$c_nip_1, 
                   title = "Položka 1",
                   axis.title = "zameral/a sa na konkrétne ciele vs. nezameral/a sa na konkrétne ciele") +
  scale_x_discrete(limits = rev)

ggsave(item_1, file = "item1.png")

item_2 <- plot_frq(cnip$c_nip_2, 
                   title = "Položka 2",
                   axis.title = "určoval/a priebeh terapie vs. nechal/a terapiu voľne plynúť") +
  scale_x_discrete(limits = rev)

ggsave(item_2, file = "item2.png")

item_3 <- plot_frq(cnip$c_nip_3, 
                   title = "Položka 3",
                   axis.title = "učil/a ma schopnosti k zvládaniu mojich problémov vs. neučil/a ma schopnosti k zvládaniu mojich problémov") +
  scale_x_discrete(limits = rev)

ggsave(item_3, file = "item3.png")

item_4 <- plot_frq(cnip$c_nip_4, 
                   title = "Položka 4",
                   axis.title = "dával/a mi úlohy na doma vs. nedával/a mi úlohy na doma") +
  scale_x_discrete(limits = rev)

ggsave(item_4, file = "item4.png")

item_5 <- plot_frq(cnip$c_nip_5, 
                   title = "Položka 5",
                   axis.title = "on/a viedol/a terapiu vs. umožnil/a mi viesť terapiu") +
  scale_x_discrete(limits = rev)

ggsave(item_5, file = "item5.png")

item_6 <- plot_frq(cnip$c_nip_6,
                   title = "Položka 6", 
                   axis.title = "podporoval/a ma, aby som sa otváral/a náročným emóciám vs. nevyzýval/a ma, aby som sa otváral/a náročným emóciám") +
  scale_x_discrete(limits = rev)

ggsave(item_6, file = "item6.png")

item_7 <- plot_frq(cnip$c_nip_7, 
                   title = "Položka 7",
                   axis.title = "rozprával/a sa so mnou o našom terapeutickom vzťahu vs. nerozprával/a sa so mnou o našom terapeutickom vzťahu") +
  scale_x_discrete(limits = rev)

ggsave(item_7, file = "item7.png")

item_8 <- plot_frq(cnip$c_nip_8, 
                   title = "Položka 8",
                   axis.title = "zameral/a sa na vzťah medzi nami vs. nezameriaval/a sa na vzťah medzi nami") +
  scale_x_discrete(limits = rev)

ggsave(item_8, file = "item8.png")

item_9 <- plot_frq(cnip$c_nip_9,
                   title = "Položka 9", 
                   axis.title = "podporoval/a ma vo vyjadrovaní silných pocitov vs. nevyzýval/a ma k vyjadrovaniu silných pocitov") +
  scale_x_discrete(limits = rev)

ggsave(item_9, file = "item9.png")

item_10 <- plot_frq(cnip$c_nip_10, 
                    title = "Položka 10", 
                    axis.title = "zameral/a sa hlavne na moje pocity vs. zameral/a sa hlavne na moje myšlienky") +
  scale_x_discrete(limits = rev)

ggsave(item_10, file = "item10.png")

item_11 <- plot_frq(cnip$c_nip_11,
                    title = "Položka 11", 
                    axis.title = "zameral/a sa na môj život v minulosti vs. zameral/a sa na môj život v súčasnosti") +
  scale_x_discrete(limits = rev)

ggsave(item_11, file = "item11.png")

item_12 <- plot_frq(cnip$c_nip_12, 
                    title = "Položka 12", 
                    axis.title = "pomohol/a mi zamýšľať sa nad mojím detstvom vs. pomohol/a mi zamýšľať sa nad mojím životom v dospelosti") +
  scale_x_discrete(limits = rev)

ggsave(item_12, file = "item12.png")

item_13 <- plot_frq(cnip$c_nip_13,
                    title = "Položka 13", 
                    axis.title = "zameral/a sa na moju minulosť vs. zameral/a sa na moju budúcnosť") +
  scale_x_discrete(limits = rev)

ggsave(item_13, file = "item13.png")

item_14 <- plot_frq(cnip$c_nip_14,
                    title = "Položka 14",
                    axis.title = "bol/a na mňa mierny/a vs. bol/a na mňa náročný/á") +
  scale_x_discrete(limits = rev)

ggsave(item_14, file = "item14.png")

item_15 <- plot_frq(cnip$c_nip_15,
                    title =  "Položka 15", 
                    axis.title = "bol/a podporujúci/a vs. bol/a konfrontujúci/a") +
  scale_x_discrete(limits = rev)

ggsave(item_15, file = "item15.png")

item_16 <- plot_frq(cnip$c_nip_16, 
                    title = "Položka 16", 
                    axis.title = "neprerušoval/a ma vs. prerušil/a ma, keď odbočím od témy") +
  scale_x_discrete(limits = rev)

ggsave(item_16, file = "item16.png")

item_17 <- plot_frq(cnip$c_nip_17,
                    title = "Položka 17", 
                    axis.title = "nespochybňovala/a moje presvedčenia a názory vs. rozporoval/a moje presvedčenia a názory") +
  scale_x_discrete(limits = rev)

ggsave(item_17, file = "item17.png")

item_18 <- plot_frq(cnip$c_nip_18,
                    title = "Položka 18",
                    axis.title = "bezvýhradne podporoval/a moje správanie vs. spochybnil/a moje správanie, ak by si myslel/a, že je nesprávne") +
  scale_x_discrete(limits = rev)

ggsave(item_18, file = "item18.png")




# factor analysis ---------------------------------------------------------

#four-factor model ----------------

m1 <- "td_cd =~ c_nip_1 + c_nip_2 + c_nip_3 + c_nip_4 + c_nip_5 
         ei_er =~ c_nip_6 + c_nip_7 + c_nip_8 + c_nip_9 + c_nip_10
         pao_pro =~ c_nip_11 + c_nip_12 + c_nip_13
         ws_fc =~ c_nip_14 + c_nip_15 + c_nip_16 + c_nip_17 + c_nip_18"

fit1 <- lavaan::cfa(model = m1, data = data, missing = "ml.x", estimator = "MLR")
summary(fit1, fit.measures = T, standardized = T)

#five-factor model ------------

m2 <- "td_cd =~ c_nip_1 + c_nip_2 + c_nip_3 + c_nip_4  
ei_er =~ c_nip_6 +  c_nip_9
im_nim =~ c_nip_7 + c_nip_8
pao_pro =~ c_nip_11 + c_nip_12 + c_nip_13
ws_fc =~ c_nip_14 + c_nip_16 + c_nip_17 + c_nip_18

c_nip_1 ~~ c_nip_2"

fit2 <- lavaan::cfa(model = m2, data = data, missing = "ml.x", estimator = "MLR", check.lv.names = F)
fit_sum <- summary(fit2, fit.measures = T, standardized = T)

fit2_mod <- modindices(fit2, sort. = T)
fit2_mod[fit2_mod$op == "=~",]
lavInspect(fit2, "cor.lv")
params <- parameterEstimates(fit2, standardized = T)

loadings <- params %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est, se, z, pvalue, std.all) %>%
  rename(
    Factor = lhs,
    Item = rhs,
    Loading = est,
    SE = se,
    Z_value = z,
    P_value = pvalue,
    Std_Loading = std.all
  ) %>%
  mutate(
    Significant = ifelse(P_value < 0.001, "***",
                         ifelse(P_value < 0.01, "**",
                                ifelse(P_value < 0.05, "*", "")))
  )


#reliability of factors 

semTools::reliability(fit2)
print(loadings)

#gender sensitivity ----

female_cnip <- data %>% filter(gender == 2)
male_cnip <- data %>% filter(gender == 1)
data_gender <- data %>% filter(gender == 1 | gender == 2)


fit_female <- cfa(m2,
                  data = female_cnip,
                  std.lv = TRUE,
                  estimator = "MLR",
                  missing = "fiml", 
                  check.lv.names = F)
summary(fit_female, fit.measures = TRUE, standardized = TRUE)

pe <- parameterEstimates(fit_female, standardized = TRUE)
loadings <- pe[pe$op == "=~", c("lhs","rhs","est")]
loadings


m2_fixed <- "
td_cd =~ 0.810*c_nip_1 + 0.914*c_nip_2 + 0.818*c_nip_3 + 1.017*c_nip_4  
im_nim =~ 1.084*c_nip_6 + 1.145*c_nip_9
ei_er  =~ 1.755*c_nip_7 + 1.329*c_nip_8
pao_pro =~ 1.542*c_nip_11 + 1.732*c_nip_12 + 1.560*c_nip_13
ws_fc   =~ 0.935*c_nip_14 + 0.990*c_nip_16 + 1.265*c_nip_17 + 0.572*c_nip_18
"

fit_fixed <- cfa(m2_fixed,
                 data = data_gender,
                 std.lv = TRUE,
                 estimator = "MLR",
                 missing = "fiml", 
                 check.lv.names = F)

summary(fit_fixed, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_female, c("cfi", "rmsea", "srmr"))
fitMeasures(fit_fixed, c("cfi", "rmsea", "srmr"))


delta_cfi_gender <- fitMeasures(fit_fixed, "cfi") - fitMeasures(fit_female, "cfi")
delta_cfi_gender

#therapy experience sensitivity ----

data_terapia <- data %>% filter(terapia_skusenost == 1) 

fit_terapia <- cfa(m2,
                  data = data_terapia,
                  std.lv = TRUE,
                  estimator = "MLR",
                  missing = "fiml", 
                  check.lv.names = F)
summary(fit_terapia, fit.measures = TRUE, standardized = TRUE)

pe <- parameterEstimates(fit_female, standardized = TRUE)
loadings <- pe[pe$op == "=~", c("lhs","rhs","est")]
loadings

m2_terapia <- "
td_cd =~ 0.810*c_nip_1 + 0.914*c_nip_2 + 0.818*c_nip_3 + 1.017*c_nip_4  
im_nim =~ 1.084*c_nip_6 + 1.145*c_nip_9
ei_er  =~ 1.755*c_nip_7 + 1.329*c_nip_8
pao_pro =~ 1.542*c_nip_11 + 1.732*c_nip_12 + 1.560*c_nip_13
ws_fc   =~ 0.935*c_nip_14 + 0.990*c_nip_16 + 1.265*c_nip_17 + 0.572*c_nip_18
"

fit_full_terapia <- cfa(m2_terapia,
                        data = data,
                        std.lv = TRUE,
                        estimator = "MLR",
                        missing = "fiml", 
                        check.lv.names = F)
summary(fit_full_terapia, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_terapia, c("cfi", "rmsea", "srmr"))
fitMeasures(fit_full_terapia, c("cfi", "rmsea", "srmr"))


delta_cfi_terapia <- fitMeasures(fit_full_terapia, "cfi") - fitMeasures(fit_terapia, "cfi")
delta_cfi_terapia



  
