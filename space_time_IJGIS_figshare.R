## Title: Where, When and How? Specifying spatially and temporally varying coefficent models using GAMs with Gaussian Process splines

## Load initial packages 
library(sf)
library(tidyverse)
library(mgcv)
library(broom)
library(ggspatial)
library(glue)

## Load and prepare SY house price data
load("sy_lsoa_permission.RData")
years = as.character(2010:2020)
df_lg_sy = vector()
for (i in years) {
	df.i = df_sf_sy %>% st_drop_geometry() %>%
		select(code, X, Y, ends_with(i))
	df.i$Ti = i
	names(df.i) = gsub(paste0("_",i), "", names(df.i))
	head(df.i)
	df_lg_sy = rbind(df_lg_sy, df.i)	
}
# prepare the data for GAM
df_lg_sy$one = 1
df_lg_sy <- as_tibble(df_lg_sy)
df_lg_sy <- 
  df_lg_sy %>%
  mutate(year = as.numeric(Ti)) %>%
  select(-Ti) 
# tidy
rm(list = c("df.i", "i", "years"))

## Figure 1
# Create a towns layer for illustration
towns = data.frame(towns = c("Sheffield", "Barnsley", "Doncaster"),
           lat = c(53.3811, 53.5526, 53.5228),
           lon = c(-1.4701, -1.4797, -1.1285))
towns = towns %>% st_as_sf(coords = c("lon", "lat"), 
                 crs = 4326, agr = "constant")
towns = st_transform(towns, 27700)
df_sf_sy %>% 
  #st_extent(10000) %>%
	ggplot() + geom_sf() +
  annotation_map_tile(zoom = 12, progress = "none")+
  annotation_scale()+
  geom_sf(data = df_sf_sy, fill = "lightblue", col = "black", alpha = 0.5) + 
  geom_sf(data = towns, col = "red", size = 2) +
  geom_sf_label(data = towns, aes(label = towns), nudge_y = 2500, size = 4) +
	theme_bw() + xlab("") + ylab("") +
	theme(legend.position = "bottom", legend.title = element_blank(),
		legend.key.width = unit(2.1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Figure 2
df_lg_sy %>% 
  select(-X, -Y, -one, -code, -badhealth) %>% 
  rename(`House Price (£1000s)` = hp,
         #`Bad Health (%)` = badhealth,
         `In Migration (%)` = nino,
         `Professional Occupations (%)` = prof,
         `Residential Mobility (%)` = resmob,
         `Unemployment (%)` = unemp) %>% 
  pivot_longer(-year) %>% 
  group_by(name, year) %>% 
  summarise(median = median(value),
            lower =  quantile(value, 0.25),
            upper =  quantile(value, 0.75)) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line(lty = 2) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
                color="blue",width=0.5) +
  facet_wrap(~name, scale = "free") +
  scale_x_continuous(breaks=seq(2010, 2020, 2)) +
  theme_bw() + xlab("Year") + ylab("") +
  theme(strip.background = element_rect(fill="white"))

## Load and prepare MN livestock data
load("mn_soum.RData")
# number of households working with livestock NHOUSE
# N is NDVI, mean, sum and sd
# P is precipitation, mean sum and sd
# AllLoss is the sum of animal loses
df_sf_mn <- 
  df_sf_mn %>% 
  mutate(ID = 1:n()) %>%
  relocate(ID) 
# remove some noise
df_sf_mn <- 
  df_sf_mn %>% 
  select(-contains("sum")) %>% 
  select(-contains("nsd"))
## prepare for GAM
df_sf_mn = st_transform(df_sf_mn, 'ESRI:102226') 
coords = st_coordinates(st_centroid(df_sf_mn))
# head(coords)
df_sf_mn <-  
  df_sf_mn %>% 
  mutate(int = 1, 
         X = coords[,"X"]/1000, 
         Y = coords[,"Y"]/1000) 
# long format  
years = as.character(1991:2006)
df_lg_mn = vector()
for (i in years) {
	df.i = df_sf_mn %>% st_drop_geometry() %>%
		select(ID, X, Y, ends_with(i))
	df.i$year = i
	names(df.i) = gsub(paste0("_",i), "", names(df.i))
	df_lg_mn = rbind(df_lg_mn, df.i)	
}
df_lg_mn$one = 1
df_lg_mn$year = as.numeric(df_lg_mn$year)

## Extract Camel data
df_lg_mn <- 
  df_lg_mn %>% 
  select(ID, starts_with(c("camel", "losscamel", "nhouse", "pmean", "nmean", "year", "one", "X","Y"))) %>% 
  mutate(camel_sq= sqrt(camel)) %>% 
  rename(camel=camel_sq, camel_orig=camel) %>% 
  rename(losses = losscamel, livestockh = nhouse, ndvi = nmean, precip = pmean) %>%
  relocate(ID, camel) %>%
  tibble()
# tidy
rm(list = c("df.i", "i", "years", "coords"))

## Figure 3
# Create a towns layer for illustration
cities = data.frame(cities = c("Ulaanbaatar", "Erdenet", "Darkhan"),
           lat = c(47.8864, 49.0541, 49.4648),
           lon = c(106.9057, 104.0717, 105.9746))
cities = cities %>% st_as_sf(coords = c("lon", "lat"), 
                 crs = 4326, agr = "constant")
st_simplify(df_sf_mn, dTolerance = 100) %>% 
  ggplot() + 
  geom_sf() +
  annotation_map_tile(zoomin = 1, progress = "none") +
  annotation_scale() +
  geom_sf(fill = "pink", col = "black", alpha = 0.5) + 
  geom_sf(data = cities, col = "red", size = 2.5) +
  geom_sf_label(data = cities, aes(label = cities), nudge_x = -200000, nudge_y = c(0, 0, 50000), size = 3) +
	theme_bw() + xlab("") + ylab("") +
	theme(legend.position = "bottom", legend.title = element_blank(),
		legend.key.width = unit(2.1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## Figure 4
df_lg_mn %>% 
  select(-X, -Y, -one, - camel_orig, -ID) %>% 
  rename(`Camel Losses` = losses,
         `Livestock Households` = livestockh,
         `Mean Precipitation (mm)` = precip,
         `Mean NDVI` = ndvi,
         `Camel` = camel, 
         Ti = year) %>% 
  pivot_longer(-Ti) %>% 
  mutate(name = factor(name, levels = c("Camel", "Camel Losses", "Livestock Households", 
                                        "Mean NDVI", "Mean Precipitation (mm)"))) %>% 
  group_by(name, Ti) %>% 
  summarise(median = median(value),
            lower =  quantile(value, 0.25),
            upper =  quantile(value, 0.75)) %>%
  ggplot(aes(x = Ti, y = median)) +
  geom_line(col = "black", lty = 2) +
  geom_errorbar(aes(x = Ti, ymin = lower, ymax = upper),
                color="violetred3",width=0.5) +
  facet_wrap(~name, scale = "free") +
  scale_x_continuous(breaks=seq(1991, 2006, 3)) +
  theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_rect(fill="white"))

## This loop creates the 6480 models for the House Price case study
# it takes time to run
DoSYAnal = FALSE			# change to FALSE to run OR leave and load the data at the end of the loop
if(DoSYAnal) {
  library(doParallel)
  # function to make equations 
  makeform_sy <- function(intcp = 5, unemp = 5, prof =4, resmob = 5, nino = 6, bs='gp') {
    intx <- 	c("", 
    					glue("+s(year,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,bs='{bs}',by=one) + s(year,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,year,bs='{bs}',by=one)"))[intcp]
    unempx <- 	c("", "+ unemp",
    					glue("+s(year,bs='{bs}',by=unemp)"), 	
  	  				glue("+s(X,Y,bs='{bs}',by=unemp)"), 	
  	  				glue("+s(X,Y,bs='{bs}',by=unemp) + s(year,bs='{bs}',by=unemp)"),
  	  				glue("+s(X,Y,year,bs='{bs}',by=unemp)"))[unemp]
    profx <-c("", "+ prof",
    					glue("+s(year,bs='{bs}',by=prof)"), 	
    					glue("+s(X,Y,bs='{bs}',by=prof)"), 	
    					glue("+s(X,Y,bs='{bs}',by=prof) + s(year,bs='{bs}',by=prof)"),
    					glue("+s(X,Y,year,bs='{bs}',by=prof)"))[prof]
    resmobx <- c("", "+ resmob",
    					glue("+s(year,bs='{bs}',by=resmob)"), 	
    					glue("+s(X,Y,bs='{bs}',by=resmob)"), 	
    					glue("+s(X,Y,bs='{bs}',by=resmob) + s(year,bs='{bs}',by=resmob)"),
    					glue("+s(X,Y,year,bs='{bs}',by=resmob)"))[resmob]
    ninox <- c("", "+ nino",
    					glue("+s(year,bs='{bs}',by=nino)"), 	
    					glue("+s(X,Y,bs='{bs}',by=nino)"), 	
    					glue("+s(X,Y,bs='{bs}',by=nino) + s(year,bs='{bs}',by=nino)"),
    					glue("+s(X,Y,year,bs='{bs}',by=nino)"))[nino]
    return(formula(glue("hp~one-1{unempx}{intx}{profx}{resmobx}{ninox}")))
  }
  # grid of combinations
  terms_gr_sy = expand.grid(intcp = 1:5, unemp = 1:6, prof = 1:6, resmob = 1:6, nino = 1:6) 
  do_gam_sy = function(i){
  	f <- makeform_sy(intcp = terms_gr_sy$intcp[i],
  	                 unemp = terms_gr_sy$unemp[i],
  	                 prof = terms_gr_sy$prof[i],
  	                 resmob = terms_gr_sy$resmob[i],
  	                 nino = terms_gr_sy$nino[i],
  	                 bs='gp')
  	m = gam(f,data=df_lg_sy)
    bic = BIC(m)
    index = data.frame(intcp = terms_gr_sy$intcp[i],
                       unemp = terms_gr_sy$unemp[i],
                       prof = terms_gr_sy$prof[i],
                       resmob = terms_gr_sy$resmob[i],
                       nino = terms_gr_sy$nino[i])
    f = paste0('hp~', as.character(f)[3] )			
    return(data.frame(index, bic, f))
  }
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  t1 = Sys.time()
  res_gam_sy <- 
    foreach(i = 1:nrow(terms_gr_sy),
            .combine = 'rbind', 
            .packages = c("glue", "mgcv", "purrr")) %dopar% {
              do_gam_sy(i)
              }
  t_tot2 = Sys.time() - t1
  stopCluster(cl)
  # make tibble
  mod_comp_sy <- tibble(
    res_gam_sy) |>
    rename(BIC = bic) |>
    arrange(BIC) 
  save(mod_comp_sy, file = "mod_comp_sy.RData")
} else {
  # changed from mod_comp in bm_av_lex_v5.R
  load("mod_comp_sy.RData")
}

## This loop creates the 6480 models for the Camel case study
# it takes time to run
DoCamelAnal = FALSE				# change to FALSE to run OR leave and load the data at the end of the loop
if(DoCamelAnal) {
  library(doParallel)
  # function to make equations 
  makeform_mn <- function(intcp = 5, losses = 6, livestockh = 3, ndvi = 6, precip = 2, bs='gp') {
    intx <- 	c("", 
    					glue("+s(year,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,bs='{bs}',by=one) + s(year,bs='{bs}',by=one)"), 
    					glue("+s(X,Y,year,bs='{bs}',by=one)"))[intcp]
    lossesx <- 	c("", "+ losses",
    					glue("+s(year,bs='{bs}',by=losses)"), 	
  	  				glue("+s(X,Y,bs='{bs}',by=losses)"), 	
  	  				glue("+s(X,Y,bs='{bs}',by=losses) + s(year,bs='{bs}',by=losses)"),
  	  				glue("+s(X,Y,year,bs='{bs}',by=losses)"))[losses]
    livestockhx <-c("", "+ livestockh",
    					glue("+s(year,bs='{bs}',by=livestockh)"), 	
    					glue("+s(X,Y,bs='{bs}',by=livestockh)"), 	
    					glue("+s(X,Y,bs='{bs}',by=livestockh) + s(year,bs='{bs}',by=livestockh)"),
    					glue("+s(X,Y,year,bs='{bs}',by=livestockh)"))[livestockh]
    ndvix <- c("", "+ ndvi",
    					glue("+s(year,bs='{bs}',by=ndvi)"), 	
    					glue("+s(X,Y,bs='{bs}',by=ndvi)"), 	
    					glue("+s(X,Y,bs='{bs}',by=ndvi) + s(year,bs='{bs}',by=ndvi)"),
    					glue("+s(X,Y,year,bs='{bs}',by=ndvi)"))[ndvi]
    precipx <- c("", "+ precip",
    					glue("+s(year,bs='{bs}',by=precip)"), 	
    					glue("+s(X,Y,bs='{bs}',by=precip)"), 	
    					glue("+s(X,Y,bs='{bs}',by=precip) + s(year,bs='{bs}',by=precip)"),
    					glue("+s(X,Y,year,bs='{bs}',by=precip)"))[precip]
    return(formula(glue("camel~one-1{lossesx}{intx}{livestockhx}{ndvix}{precipx}")))
  }
  # grid of combinations
  terms_gr_mn = expand.grid(intcp = 1:5, losses = 1:6, livestockh = 1:6, ndvi = 1:6, precip = 1:6) 
  do_gam_mn = function(i){
  	f <- makeform_mn(intcp = terms_gr_mn$intcp[i],
  	                 losses = terms_gr_mn$losses[i],
  	                 livestockh = terms_gr_mn$livestockh[i],
  	                 ndvi = terms_gr_mn$ndvi[i],
  	                 precip = terms_gr_mn$precip[i],
  	                 bs='gp')
  	m = gam(f,data=df_lg_mn)
    bic = BIC(m)
    index = data.frame(intcp = terms_gr_mn$intcp[i],
                       losses = terms_gr_mn$losses[i],
                       livestockh = terms_gr_mn$livestockh[i],
                       ndvi = terms_gr_mn$ndvi[i],
                       precip = terms_gr_mn$precip[i])
    f = paste0('camel~', as.character(f)[3] )			
    return(data.frame(index, bic, f))
  }
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  t1 = Sys.time()
  res_gam_mn <- 
    foreach(i = 1:nrow(terms_gr_mn),
            .combine = 'rbind', 
            .packages = c("glue", "mgcv", "purrr")) %dopar% {
              do_gam_mn(i)
              }
  t_tot1 = Sys.time() - t1 
  stopCluster(cl)
  # make tibble
  mod_comp_mn <- tibble(
    res_gam_mn) |>
    rename(BIC = bic) |>
    arrange(BIC) 
  save(mod_comp_mn, file = "mod_comp_mn.RData")
} else {
  load("mod_comp_mn.RData")
}

## Explore results - Tables 1 and 2
# define the model terms
int_terms <- \(x) c("Fixed","s_T", "s_S", "s_T + S_S", "s_ST")[x]
var_terms <- \(x) c("---", "Fixed","s_T", "s_S", "s_T + s_S", "s_ST")[x]
# create sumamry tables for use in Table 1 and Table 2
# House Price
mod_comp_sy_tab <- 
  mod_comp_sy |> 
  slice_head(n = 10) |> 
  mutate(across(unemp:nino,var_terms)) |>
  mutate(intcp = int_terms(intcp)) |>
  rename(`Intercept` = intcp,
         `In. Mig.` = nino,
         `Prof. Occupn.` = prof,
         `Res. Mobility` = resmob,
         `Unemployment.` = unemp) |>
  mutate(Rank = 1:n()) |>
  relocate(Rank) |>
  select(-f) 
# Camels
mod_comp_mn_tab <- 
  mod_comp_mn |> 
  slice_head(n = 10) |> 
  mutate(across(losses:precip,var_terms)) |>
  mutate(intcp = int_terms(intcp)) |>
  rename(`Intercept` = intcp,
         `Losses` = losses,
         `Liv. Households` = livestockh,
         `Precipitation` = precip,
         `NDVI` = ndvi) |>
  mutate(Rank = 1:n()) |>
  relocate(Rank) |>
  select(-f) 
# Calculate model probabilities for the Tables 1 and 2
# House Price
p1_vec_sy = p2_vec_sy = NULL
for(i in 2:10) {
  p1 = exp(-(mod_comp_sy_tab$BIC[i]-mod_comp_sy_tab$BIC[1])/2)
  p1 = p1/(1+p1)
  p2 = exp(-(mod_comp_sy_tab$BIC[i]-mod_comp_sy_tab$BIC[i-1])/2)
  p2 = p2/(1+p2)
  p1_vec_sy = c(p1_vec_sy, p1)
  p2_vec_sy = c(p2_vec_sy, p2)
}
mod_comp_sy_tab$P = c("--", paste0(format(round(p1_vec_sy*100, digits=1), nsmall = 1), "%"))
# Camels
p1_vec_mn = p2_vec_mn = NULL
for(i in 2:10) {
  p1 = exp(-(mod_comp_mn_tab$BIC[i]-mod_comp_mn_tab$BIC[1])/2)
  p1 = p1/(1+p1)
  p2 = exp(-(mod_comp_mn_tab$BIC[i]-mod_comp_mn_tab$BIC[i-1])/2)
  p2 = p2/(1+p2)
  p1_vec_mn = c(p1_vec_mn, p1)
  p2_vec_mn = c(p2_vec_mn, p2)
}
mod_comp_mn_tab$P = c("--", paste0(format(round(p1_vec_mn*100, digits=1), nsmall = 1), "%"))
# tidy
rm(list = c("p1", "p2", "int_terms", "var_terms", "i"))
# Table 1
colnames(mod_comp_sy_tab)[8] = "Pr(M)"
mod_comp_sy_tab
# Table 2
colnames(mod_comp_mn_tab)[8] = "Pr(M)"
mod_comp_mn_tab

## 4.2 House Price Model Coefficients
# make model with best BIC
f = as.formula(mod_comp_sy$f[1])
sy_mod = gam(f,data=df_lg_sy)
# function for SVC generation
calculate_stvcs = function(model, terms, input_data) {
	n_t = length(terms)
	input_data_copy = input_data
	output_data = input_data
	for (i in 1:n_t) {
		zeros = rep(0, n_t)
		zeros[i] = 1
		df.j = vector()
		terms_df = data.frame(matrix(rep(zeros, nrow(input_data)), ncol = n_t, byrow = T))
		names(terms_df) = terms
		input_data_copy[, terms] = terms_df
		se.j = predict(model, se = TRUE, newdata = input_data_copy)$se.fit
		b.j=predict(model,newdata=input_data_copy)
		expr1 = paste0("b_", terms[i], "= b.j")
		expr2 = paste0("se_",terms[i], "= se.j")
		output_data = output_data %>% 
			mutate(within(., !!parse(text = expr1))) %>% 
			mutate(within(., !!parse(text = expr2))) 
	}
	output_data$yhat = predict(model, newdata = input_data)
	output_data
}
terms = c("one", "unemp", "prof", "resmob", "nino")
svc_sf_sy = calculate_stvcs(model = sy_mod, terms, input_data = df_lg_sy)
svc_sf_sy <- df_sf_sy |> select(code) |> right_join(svc_sf_sy) 
# tidy
rm(list = c("f", "sy_mod", "terms" ))

## Table 3
svc_sf_sy |> st_drop_geometry() |> select(starts_with("b_")) |> 
  rename(`Incercept` = b_one,
         `In Migration (%)` = b_nino,
         `Professional Occupations (%)` = b_prof,
         `Residential Mobility (%)` = b_resmob,
         `Unemployment (%)` = b_unemp) %>% 
  apply(2, summary) |> round(2) |> 
  t() 

## Figure 5
library(cols4all)
library(cowplot)
# inputs to plot
svc_sf_sy |> st_drop_geometry() |> select(starts_with("b_unemp")) |> 
  mutate(year = "All Years") -> tmp
cols = c(c4a("tableau.red_gold", n = 11, reverse = T), "grey")
tit =expression(paste(""*beta[Unemployment]*""))
# plot
p1 = svc_sf_sy |> st_drop_geometry() |> select(year, starts_with("b_unemp")) |> 
  rbind(tmp) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(y = year, x = b_unemp, fill = year)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(values=cols, guide = "none") +
  theme_bw() + xlab(tit) + ylab("Time") 
# inputs to plot
svc_sf_sy |> st_drop_geometry() |> select(starts_with("b_prof")) |> 
  mutate(year = "All Years") -> tmp
cols = c(c4a("tableau.red_gold", n = 11, reverse = T), "grey")
tit =expression(paste(""*beta[`Prof. Occupn.`]*""))
# plot
p2 = svc_sf_sy |> st_drop_geometry() |> select(year, starts_with("b_prof")) |> 
  rbind(tmp) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(y = year, x = b_prof, fill = year)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(values=cols, guide = "none") +
  theme_bw() + xlab(tit) + ylab("Time") 
plot_grid(p1, p2, ncol = 2)
# tidy
rm(list = c("p1", "p2", "tit", "cols", "tmp"))

## Figure 6
tit =expression(paste(""*beta[Unemployment]*""))
p3 = 
  svc_sf_sy %>% 
  ggplot() + geom_sf(aes(fill = b_unemp), col = NA) +
	scale_fill_continuous_c4a_seq(palette="scico.lajolla", name = tit) + 
  facet_wrap(~year) +
	theme_bw() + xlab("") + ylab("") +
	#ggtitle(tit) + 
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 8, margin = margin()),
	  legend.position = "bottom", 
	  legend.key.width = unit(1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  
tit =expression(paste(""*beta[`Prof. Occupn.`]*""))
p4 = 
  svc_sf_sy %>% 
  ggplot() + geom_sf(aes(fill = b_prof), col = NA) +
	scale_fill_continuous_c4a_seq(palette="scico.lajolla", name =  tit) + 
  facet_wrap(~year) +
	theme_bw() + xlab("") + ylab("") +
	#ggtitle(tit) + 
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 8, margin = margin()),
	  legend.position = "bottom", 
	  legend.key.width = unit(1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  
plot_grid(p3, p4, ncol = 2)
rm(list = c("p3", "p4", "tit"))

## 4.3 Averaging of Livestock Model Coefficients 
# make 4 models with best BIC
f = as.formula(mod_comp_mn$f[1])
mn_mod1 = gam(f,data=df_lg_mn)
f = as.formula(mod_comp_mn$f[2])
mn_mod2 = gam(f,data=df_lg_mn)
f = as.formula(mod_comp_mn$f[3])
mn_mod3 = gam(f,data=df_lg_mn)
f = as.formula(mod_comp_mn$f[4])
mn_mod4 = gam(f,data=df_lg_mn)

## make 4 predictions
terms = c("one", "losses", "livestockh", "precip", "ndvi")
svc_sf_mn1 = calculate_stvcs(model = mn_mod1, terms, input_data = df_lg_mn)
svc_sf_mn2 = calculate_stvcs(model = mn_mod2, terms, input_data = df_lg_mn)
svc_sf_mn3 = calculate_stvcs(model = mn_mod3, terms, input_data = df_lg_mn)
svc_sf_mn4 = calculate_stvcs(model = mn_mod4, terms, input_data = df_lg_mn)

# extract and rescale relative probabilities
probs = p1_vec_mn[1:3]
probs = c(1-probs[1], probs)
probs = probs / sum(probs)

res = NULL
for (i in terms) {
  # coefficient averages
  var.i = paste0("b_",i)
  mat = cbind(
      svc_sf_mn1 |> select(var.i),
      svc_sf_mn2 |> select(var.i),
      svc_sf_mn3 |> select(var.i),
      svc_sf_mn4 |> select(var.i)) 
  mat = as.matrix(mat)  %*% probs
  res = cbind(res, mat)
  # posterior sd
  var.i = paste0("se_",i)
  mat = cbind(
      svc_sf_mn1 |> select(var.i),
      svc_sf_mn2 |> select(var.i),
      svc_sf_mn3 |> select(var.i),
      svc_sf_mn4 |> select(var.i)) 
  mat = sqrt(as.matrix(mat)  %*% probs)
  res = cbind(res, mat)
}
colnames(res) = names(svc_sf_mn1)[12:21]

## Table 4
svc_sf_mn = svc_sf_mn1
svc_sf_mn[,colnames(res)] <- res
svc_sf_mn <- df_sf_mn |> st_simplify(dTolerance = 10000) |> select(ID) |> right_join(svc_sf_mn) 
# variations over both
svc_sf_mn |> st_drop_geometry() |> select(starts_with("b_")) |> 
  rename(`Incercept` = b_one,
         `Camel Losses` = b_losses,
         `Livestock Households` = b_livestockh,
         `Mean Precipitation` = b_precip,
         `Mean NDVI` = b_ndvi) %>% 
  apply(2, summary) |> round(3) |> as_tibble() |>
  mutate(`Mean Precipitation` = "-") |> 
  t() 
  
## Figure 7
library(gridExtra)
library(cowplot)
svc_sf_mn |> st_drop_geometry() |> select(starts_with("b_losses")) |> 
  mutate(year = "All Years") |> 
  summarise(year = unique(year), b_losses = median(b_losses))-> tmp
tit =expression(paste(""*beta[Losses]*""))
# plot
rbind(tmp, svc_sf_mn |> st_drop_geometry() |> select(year, starts_with("b_losses")) |> 
  distinct() |> arrange(desc(year)) ) |>
  mutate(b_losses = round(b_losses, 3)) -> df
# inputs to plot
svc_sf_mn |> st_drop_geometry() |> select(starts_with("b_one")) |> 
  mutate(year = "All Years") -> tmp
cols = c(c4a("tableau.red_gold", n = 16, reverse = T), "grey")
tit =expression(paste(""*beta[Intercept]*""))
# plot
p1 = svc_sf_mn |> st_drop_geometry() |> select(year, starts_with("b_one")) |> 
  rbind(tmp) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(y = year, x = b_one, fill = year)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(values=cols, guide = "none") +
  theme_bw() + xlab(tit) + ylab("Time") 
# inputs to plot
svc_sf_mn |> st_drop_geometry() |> select(starts_with("b_ndvi")) |> 
  mutate(year = "All Years") -> tmp
cols = c(c4a("tableau.red_gold", n = 16, reverse = T), "grey")
tit =expression(paste(""*beta[`NDVI`]*""))
# plot
p2 = svc_sf_mn |> st_drop_geometry() |> select(year, starts_with("b_ndvi")) |> 
  rbind(tmp) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(y = year, x = b_ndvi, fill = year)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(values=cols, guide = "none") +
  theme_bw() + xlab(tit) + ylab("") 
plot_grid(p1, p2, ncol = 2)

## Figure 8
mat = NULL
var.i = "b_ndvi"
mat = cbind(
      svc_sf_mn1 |> select(var.i),
      svc_sf_mn2 |> select(var.i),
      svc_sf_mn3 |> select(var.i),
      svc_sf_mn4 |> select(var.i)) 
mat = cbind(svc_sf_mn$ID, svc_sf_mn$year, as.matrix(mat)  %*% probs, mat)
mat = data.frame(mat)
colnames(mat) = c("ID", "Year", "Average", "Model 1", "Model 2", "Model 3", "Model 4")
mat_sf <- df_sf_mn |> st_simplify(dTolerance = 10000) |> select(ID) |> right_join(mat) 
tit =expression(paste(""*beta[NDVI]*""))
p3 = 
  mat_sf |>
  filter(Year %in% years) |> 
  pivot_longer(cols = Average:`Model 4`) |> 
  mutate(x = paste0(Year, " ", name)) |>
  ggplot() + geom_sf(aes(fill = value), col = NA) +
	scale_fill_continuous_c4a_div(palette="brewer.spectral",
	                              reverse = T, name = tit) + 
  facet_wrap(~x, ncol = 5) +
	theme_bw() + xlab("") + ylab("") +
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 10),
	  legend.position = "bottom", 
	  legend.key.width = unit(1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  

mat = NULL
var.i = "b_one"
mat = cbind(
      svc_sf_mn1 |> select(var.i),
      svc_sf_mn2 |> select(var.i),
      svc_sf_mn3 |> select(var.i),
      svc_sf_mn4 |> select(var.i)) 
mat = cbind(svc_sf_mn$ID, svc_sf_mn$year, as.matrix(mat)  %*% probs, mat)
mat = data.frame(mat)
colnames(mat) = c("ID", "Year", "Average", "Model 1", "Model 2", "Model 3", "Model 4")
mat_sf <- df_sf_mn |> st_simplify(dTolerance = 10000) |> select(ID) |> right_join(mat) 
years = c(1991, 2006)
tit =expression(paste(""*beta[Intercept]*""))
p4 = 
  mat_sf |>
  filter(Year %in% years) |> 
  pivot_longer(cols = Average:`Model 4`) |> 
  mutate(x = paste0(Year, " ", name)) |>
  ggplot() + geom_sf(aes(fill = value), col = NA) +
	scale_fill_continuous_c4a_div(palette="brewer.spectral",
	                              reverse = F, name = tit) + 
  facet_wrap(~x, ncol = 5) +
	theme_bw() + xlab("") + ylab("") +
	theme(
	  strip.background =element_rect(fill="white"), 
	 	#strip.text = element_text(size = 8, margin = margin()),
	  strip.text = element_text(size = 10),
	  legend.position = "bottom", 
	  legend.key.width = unit(1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  
plot_grid(p4, p3, ncol = 1)

### END ###