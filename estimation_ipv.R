######## A model-based boosting approach to risk factors for physical intimate partner violence 
######## against women and girls in Mexico 

### Packages ###
if(!require("mboost")) install.packages("mboost")
if(!require("parallel")) install.packages("parallel")
if(!require("dplyr")) install.packages("dplyr")

### Database ###
load("database_ipv.RData") # Variables are already zero-centered

vawgdbOutcC <- vawgdbOutcC %>%
  mutate(vio_fis_año = case_when(vio_fis_año == "yes" ~ 1,
                                 vio_fis_año == "no" ~ 0))

### Model ###
vio_fis_año <- vio_fis_año ~ 
  
  bols(intercept, intercept = FALSE) +  
  
  bols(EDAD, intercept = FALSE) +                                                   
  bbs(EDAD, knots = 20, df = 1, center = TRUE) +                                    
  
  bols(EDAD, by = indigena, intercept = FALSE) +                                    
  bbs(EDAD, by = indigena, knots = 20, df = 1, center = TRUE) +                     
  
  bols(EDAD, by = niv_edmedium, intercept = FALSE) +                                
  bbs(EDAD, by = niv_edmedium, knots = 20, df = 1, center = TRUE) +                 
  bols(EDAD, by = niv_edhigh, intercept = FALSE) +                                  
  bbs(EDAD, by = niv_edhigh, knots = 20, df = 1, center = TRUE) +                   
  
  bols(eda_hij, intercept = FALSE) +                                                
  bbs(eda_hij, knots = 20, df = 1, center = TRUE) +                                 
  
  bspatial(eda_hij, EDAD, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_sex, intercept = FALSE) +                                                
  bbs(eda_sex, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(eda_sex, by = con_sex, intercept = FALSE) +                                  
  bbs(eda_sex, by = con_sex, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_sex, EDAD, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_mat, intercept = FALSE) +                                                
  bbs(eda_mat, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(eda_mat, by = mot_mat, intercept = FALSE) +                                  
  bbs(eda_mat, by = mot_mat, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_mat, EDAD, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_par2, intercept = FALSE) +                                               
  bbs(eda_par2, knots = 20, df = 1, center = TRUE) +                                
  
  bspatial(eda_par2, EDAD, center = TRUE, differences = 1, knots = 20, df = 1) +    
  
  bols(hacin, intercept = FALSE) +                                                  
  bbs(hacin, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(act_distboth, intercept = FALSE, df = 1) +                                   
  bols(act_distmales, intercept = FALSE, df = 1) +                                  
  
  bols(feminist_gradmedium, intercept = FALSE, df = 1) +                            
  bols(feminist_gradhigh, intercept = FALSE, df = 1) +                              
  
  bols(lib_sex_gradmedium, intercept = FALSE, df = 1) +                             
  bols(lib_sex_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_eco_gradmedium, intercept = FALSE, df = 1) +                             
  bols(lib_eco_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_soc_gradmedium, intercept = FALSE, df = 1) +                             
  bols(lib_soc_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(redsoc_gradmedium, intercept = FALSE, df = 1) +                              
  bols(redsoc_gradhigh, intercept = FALSE, df = 1) +                                
  
  bols(rout_gradmedium, intercept = FALSE, df = 1) +                                
  bols(rout_gradhigh, intercept = FALSE, df = 1) +                                  
  
  bspatial(ingm_muj, ingm_par, center = TRUE, differences = 1, knots = 20, df = 1) +
  
  brandom(cvegeo, df = 1) +                                                         
  
  bols(mhr15, intercept = FALSE) +                                                  
  bbs(mhr15, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(fhr15, intercept = FALSE) +                                                  
  bbs(fhr15, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(ghr15, intercept = FALSE) +                                                  
  bbs(ghr15, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(phogjef_f, intercept = FALSE) +                                              
  bbs(phogjef_f, knots = 20, df = 1, center = TRUE) +                               
  
  bols(pres2010_f, intercept = FALSE) +                                             
  bbs(pres2010_f, knots = 20, df = 1, center = TRUE) +                              
  
  bols(pres2010_m, intercept = FALSE) +                                             
  bbs(pres2010_m, knots = 20, df = 1, center = TRUE) +                              
  
  bols(gini15, intercept = FALSE) +                                                 
  bbs(gini15, knots = 20, df = 1, center = TRUE) +                                  
  
  bols(idh2015, intercept = FALSE) +                                                
  bbs(idh2015, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(icfm, intercept = FALSE) +                                                   
  bbs(icfm, knots = 20, df = 1, center = TRUE) +                                    
  
  bols(pea_f, intercept = FALSE) +                                                  
  bbs(pea_f, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(pea_m, intercept = FALSE) +                                                  
  bbs(pea_m, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(Marg15low, intercept = FALSE, df = 1) +                                      
  bols(Marg15medium, intercept = FALSE, df = 1) +                                   
  bols(Marg15high, intercept = FALSE, df = 1) +                                     
  bols(Marg15very_high, intercept = FALSE, df = 1) +                                
  
  bols(Type_comlow_urban, intercept = FALSE, df = 1) +                              
  bols(Type_commedium_urban, intercept = FALSE, df = 1) +                           
  bols(Type_comhigh_urban, intercept = FALSE, df = 1) +                             
  
  bspatial(x, y, center = TRUE, differences = 1, knots = 20, df = 1) +              
  
  brandom(cveent, df = 1) +                                                         
  
  bols(MasPrev, intercept = FALSE) +                                                
  bbs(MasPrev, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(FemPrev, intercept = FALSE) +                                                
  bbs(FemPrev, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(MasNoDen, intercept = FALSE) +                                               
  bbs(MasNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(FemNoDen, intercept = FALSE) +                                               
  bbs(FemNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(cor15, intercept = FALSE) +                                                  
  bbs(cor15, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(satis15, intercept = FALSE) +                                                
  bbs(satis15, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(ParPolF, intercept = FALSE) +                                                
  bbs(ParPolF, knots = 20, df = 1, center = TRUE)  
  
### Three-step strategy ###
## Functional gradient descent boosting
modelfisipv <- gamboost(vio_fis_año,
                        data = vawgdbOutcC,
                        control = boost_control(mstop = 2000, nu = 0.5, 
                                                trace = TRUE, 
                                                stopintern = TRUE),
                        weights = vawgdbOutcC$FAC_MUJ,
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(vawgdbOutcC[, "vio_fis_año"])),
                                                     w = vawgdbOutcC$FAC_MUJ))-0.5,
                        family = Binomial(link = "probit"))

# Cross-validation
set.seed(1806)
cvfisipv <- cvrisk(modelfisipv, folds = cv(model.weights(modelfisipv), 
                                           type = "subsampling"), 
                   grid = 1:10000, 
                   papply = mclapply,
                   mc.cores = parallel::detectCores())
stopfisipv <- mstop(cvfisipv)
modelfisipv[stopfisipv]

## Stability selection
p <- length(names(coef(modelfisipv, which = "")))
stabsel_conf <- stabsel_parameters(p = p, 
                                   q = 20, 
                                   cutoff = 0.8)
# Stability selection with unimodality assumption

# Cutoff: 0.8; q: 20; PFER (*):  3.74 
# (*) or expected number of low selection probability variables
# PFER (specified upper bound):  3.743316 
# PFER corresponds to signif. level 0.0425 (without multiplicity adjustment)
stabselfisipv <- stabsel(modelfisipv,
                         q = 20, 
                         cutoff = 0.8,
                         sampling.type = "SS",
                         mc.cores = parallel::detectCores())

## Pointwise bootstrap confidence intervals
confintfisipv <- confint(modelfisipv, B = 1000, 
                         level = 0.95, B.mstop = 0,
                         papply = mclapply, 
                         cvrisk_options = list(mc.cores = 25))

save(confintfisipv, stabselfisipv, modelfisipv, stopfisipv, cvfisipv, file = "estimation_ipv.RData")

