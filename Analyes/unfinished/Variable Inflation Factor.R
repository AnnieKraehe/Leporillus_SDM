# # 
# # 
# # # #####VIF
# # #
# # #
# # # apicalis2<-data.frame(as.numeric(apicalis$class),apicalis$bio01,apicalis$bio05,apicalis$bio06,apicalis$bio12,apicalis$bio13,apicalis$bio14)
# # # colnames(apicalis2)<-c('presence','bio01','bio05','bio06','bio12','bio13','bio14')
# # #
# # #
# # # vif(lm(presence~bio01+bio05+bio06+bio12+bio13+bio14,data = apicalis2))
# # # vif(lm(presence~bio01+bio05+bio06+bio13+bio14,data = apicalis2))
# # # vif(lm(presence~bio05+bio06+bio13+bio14,data = apicalis2))
# 
# #We now need a time series of palaeoclimate reconstructions. In this vignette, we will use the example dataset from pastclim. This dataset only has reconstructions every 5k years for the past 20k years at 1 degree resolution, with 3 bioclimatic variables. It will suffice for illustrative purposes, but we recommend that you download higher quality datasets with pastclim for real analysis. As for the land mask, we will cut the reconstructions to cover Europe only:
# # library(pastclim)
# # climate_vars <- c('bio01','bio05','bio06','bio12','bio13','bio14')
# # climate_full <- pastclim::region_series(
# #   bio_variables = climate_vars,
# #   data = "Krapp2021",
# #   crop = vect(Aust_extent)
# # )
# 
# 
# # #####VIF
# #
# #
# # apicalis2<-data.frame(as.numeric(apicalis$class),apicalis$bio01,apicalis$bio05,apicalis$bio06,apicalis$bio12,apicalis$bio13,apicalis$bio14)
# # colnames(apicalis2)<-c('presence','bio01','bio05','bio06','bio12','bio13','bio14')
# #
# #
# # vif(lm(presence~bio01+bio05+bio06+bio12+bio13+bio14,data = apicalis2))
# # vif(lm(presence~bio01+bio05+bio06+bio13+bio14,data = apicalis2))
# # vif(lm(presence~bio05+bio06+bio13+bio14,data = apicalis2))

