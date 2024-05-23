# Where, When and How? Specifying spatially and temporally varying coefficent models using GAMs with Gaussian Process splines

Alexis Comber<sup>1*</sup>, Paul Harris<sup>2</sup>, Naru Tsutsumida<sup>3</sup> , Jennie Gray<sup>4</sup>  and Chris Brunsdon<sup>5</sup> 

<sup>1</sup> School of Geography, University of Leeds, Leeds, UK.\
<sup>2</sup> Sustainable Agriculture Sciences North Wyke, Rothamsted Research, Okehampton, UK.\
<sup>3</sup> Department of Information and Computer Sciences, Saitama University, Japan.\
<sup>4</sup> Geographic Data Science Lab, University of Liverpool, UK.\
<sup>5</sup> National Centre for Geocomputation, Maynooth University, Ireland.\
<sup>*</sup> contact author: a.comber@leeds.ac.uk

## Abstract
This paper describes the extension of recently proposed GAM-based approaches to spatially varying coefficient (SVC) models into the temporal domain. The Geographical and Temporal Gaussian Process GAM (GTGP-GAM) creates spatially and temporally varying coefficient (STVC) regression models that allow relationships to vary over time as well as space. A key issue in such models how to determine and specify the space and time interactions. The paper uses 2 case studies: a regional house price case study from the UK over 10 years and a national livestock case study from Mongolia over 16 years. Multiple GAM models were created for each case study with Gaussian Process splines (smooths) for each covariate parameterised with location and time in different ways within the splines, allowing space and time to interact in different ways in each model for each covariate. The models were evaluated using BIC and the relative plausibility of each model was determined. For one of the case studies there was considerable doubt over the best form of space time model so it was subject to Bayesian Model Averaging. This allows the results of competing models to be combined. The discussion reflects on the GTGP-GAM approach and compares it with GTWR. 

This paper has been submitted to IJGIS.

## Code
To run the analysis in this paper you should download the the R script `space_time_IJGIS_figshare` and the 4 data files, including the input datasets (`mn_soum.RData` and `sy_lsoa_permission.RData`) and the pre-compiled results for which the code is given (`mod_comp_sy.RData` and `mod_comp_mn.RData`). Package and other info is below. The data files and supporting scripts will need will need to be locally available. The code recreates the resultsin the same sequence as the paper. 

If you have any problems with data / code / versions etc please contact Lex Comber at the email above.

```{r}
sessionInfo()
R version 4.3.2 (2023-10-31)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS Sonoma 14.1.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] gtable_0.3.4          xfun_0.43             ggplot2_3.5.1         lattice_0.22-5       
 [5] LearnBayes_2.15.1     vctrs_0.6.5           tools_4.3.2           generics_0.1.3       
 [9] sandwich_3.1-0        spdep_1.3-1           parallel_4.3.2        tibble_3.2.1         
[13] proxy_0.4-27          spacetime_1.3-1       fansi_1.0.6           DEoptimR_1.1-3       
[17] xts_0.13.1            pkgconfig_2.0.3       Matrix_1.6-4          KernSmooth_2.23-22   
[21] lifecycle_1.0.4       compiler_4.3.2        stringr_1.5.1         deldir_2.0-2         
[25] FNN_1.1.3.2           microbenchmark_1.4.10 munsell_0.5.1         codetools_0.2-19     
[29] htmltools_0.5.8.1     class_7.3-22          yaml_2.3.8            pillar_1.9.0         
[33] MASS_7.3-60.0.1       classInt_0.4-10       lwgeom_0.2-14         wk_0.9.1             
[37] spatialreg_1.3-1      multcomp_1.4-25       boot_1.3-28.1         abind_1.4-5          
[41] nlme_3.1-164          robustbase_0.99-1     tidyselect_1.2.1      digest_0.6.35        
[45] mvtnorm_1.2-4         stringi_1.8.3         sf_1.0-16             dplyr_1.1.4          
[49] purrr_1.0.2           splines_4.3.2         ggthemes_5.0.0        GWmodel_2.3-1        
[53] cowplot_1.1.2         fastmap_1.1.1         grid_4.3.2            colorspace_2.1-0     
[57] cli_3.6.2             magrittr_2.0.3        survival_3.5-7        cols4all_0.7-1       
[61] utf8_1.2.4            TH.data_1.1-2         e1071_1.7-14          scales_1.3.0         
[65] sp_2.1-4              spData_2.3.0          rmarkdown_2.26        gridExtra_2.3        
[69] zoo_1.8-12            png_0.1-8             coda_0.19-4           evaluate_0.23        
[73] knitr_1.45            mgcv_1.9-1            s2_1.1.6              rlang_1.1.3          
[77] Rcpp_1.0.12           glue_1.7.0            DBI_1.2.2             rstudioapi_0.16.0    
[81] R6_2.5.1              spacesXYZ_1.3-0       intervals_0.15.4      units_0.8-5
```
