# =================================================================
# This R script is meant to conduct statistical analysis 
# for the cross-sectional analysis of the baseline data.
# =================================================================

# create data frame
DATA <- read.delim("D:\01_MRI_data\01_lumbar_cord\longitudinal\03_analysis\data_test.txt")
#DATA$X <- NULL
#DATA$X.1 <- NULL
DATA <- DATA[(-27:-33),]

n0 <- sum(DATA$group==0)
n1 <- sum(DATA$group==1)

# create box plot of the cervical cross-sectional areas
DF1 <- data.frame(
        y = c(c(DATA$sca_c[1:n0],DATA$sca_c[(n0+1):(n0+n1)]),c(DATA$gma_c[1:n0],DATA$gma_c[(n0+1):(n0+n1)]), 
              c(DATA$wma_c[1:n0],DATA$wma_c[(n0+1):(n0+n1)]),c(DATA$dca_c[1:n0],DATA$dca_c[(n0+1):(n0+n1)])),
        x1 = rep(rep(1:2, c(n0,n1)), 4),
        x2 = rep(c("1","2","3","4"), each = (n0+n1)),
        stringsAsFactors = FALSE
)
boxplot(y ~ x1 + x2, data = DF1,
        names = c("SCA","","GMA","","WMA","","DCA",""), xaxs = FALSE, notch = FALSE)
legend("bottomleft", legend = c("controls","patients"), horiz = F)

# create box plot of the lumbar cross-sectional areas
DF2 <- data.frame(
  y = c(c(DATA$sca_l[1:n0],DATA$sca_l[(n0+1):(n0+n1)]),c(DATA$gma_l[1:n0],DATA$gma_l[(n0+1):(n0+n1)]), 
        c(DATA$wma_l[1:n0],DATA$wma_l[(n0+1):(n0+n1)]),c(DATA$dca_l[1:n0],DATA$dca_l[(n0+1):(n0+n1)])),
  x1 = rep(rep(1:2, c(n0,n1)), 4),
  x2 = rep(c("1","2","3","4"), each = (n0+n1)),
  stringsAsFactors = FALSE
)
boxplot(y ~ x1 + x2, data = DF2,
        names = c("SCA","","GMA","","WMA","","DCA",""), xaxs = FALSE, notch = FALSE)
legend("bottomleft", legend = c("controls","patients"), horiz = F)

# =============================== correlation analysis ===========================
# mstot - MRI_cervical
par(mfrow=c(3,3))
lm1 <- lm(mstot_m12 ~ sca_c, data=DATA); summary(lm1); plot(mstot_m12 ~ sca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(mstot_m12 ~ gma_c, data=DATA); summary(lm2); plot(mstot_m12 ~ gma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(mstot_m12 ~ wma_c, data=DATA); summary(lm3); plot(mstot_m12 ~ wma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(mstot_m12 ~ dca_c, data=DATA); summary(lm4); plot(mstot_m12 ~ dca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(mstot_m12 ~ fa_sc_c, data=DATA); summary(lm5); plot(mstot_m12 ~ fa_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(mstot_m12 ~ md_sc_c, data=DATA); summary(lm6); plot(mstot_m12 ~ md_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(mstot_m12 ~ ad_sc_c, data=DATA); summary(lm7); plot(mstot_m12 ~ ad_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(mstot_m12 ~ rd_sc_c, data=DATA); summary(lm8); plot(mstot_m12 ~ rd_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# lttot - MRI_cervical
par(mfrow=c(3,3))
lm1 <- lm(lttot_m12 ~ sca_c, data=DATA); summary(lm1); plot(lttot_m12 ~ sca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(lttot_m12 ~ gma_c, data=DATA); summary(lm2); plot(lttot_m12 ~ gma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(lttot_m12 ~ wma_c, data=DATA); summary(lm3); plot(lttot_m12 ~ wma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(lttot_m12 ~ dca_c, data=DATA); summary(lm4); plot(lttot_m12 ~ dca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(lttot_m12 ~ fa_sc_c, data=DATA); summary(lm5); plot(lttot_m12 ~ fa_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(lttot_m12 ~ md_sc_c, data=DATA); summary(lm6); plot(lttot_m12 ~ md_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(lttot_m12 ~ ad_sc_c, data=DATA); summary(lm7); plot(lttot_m12 ~ ad_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(lttot_m12 ~ rd_sc_c, data=DATA); summary(lm8); plot(lttot_m12 ~ rd_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# pptot - MRI_cervical
par(mfrow=c(3,3))
lm1 <- lm(pptot_m12 ~ sca_c, data=DATA); summary(lm1); plot(pptot ~ sca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(pptot_m12 ~ gma_c, data=DATA); summary(lm2); plot(pptot ~ gma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(pptot_m12 ~ wma_c, data=DATA); summary(lm3); plot(pptot ~ wma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(pptot_m12 ~ dca_c, data=DATA); summary(lm4); plot(pptot ~ dca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(pptot_m12 ~ fa_sc_c, data=DATA); summary(lm5); plot(pptot ~ fa_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(pptot_m12 ~ md_sc_c, data=DATA); summary(lm6); plot(pptot ~ md_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(pptot_m12_m12 ~ ad_sc_c, data=DATA); summary(lm7); plot(pptot ~ ad_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(pptot ~ rd_sc_c, data=DATA); summary(lm8); plot(pptot ~ rd_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# scim - MRI_cervical
par(mfrow=c(3,3))
lm1 <- lm(scim_m12 ~ sca_c, data=DATA); summary(lm1); plot(scim_m12 ~ sca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(scim_m12 ~ gma_c, data=DATA); summary(lm2); plot(scim_m12 ~ gma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(scim_m12 ~ wma_c, data=DATA); summary(lm3); plot(scim_m12 ~ wma_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(scim_m12 ~ dca_c, data=DATA); summary(lm4); plot(scim_m12 ~ dca_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(scim_m12 ~ fa_sc_c, data=DATA); summary(lm5); plot(scim_m12 ~ fa_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(scim_m12 ~ md_sc_c, data=DATA); summary(lm6); plot(scim_m12 ~ md_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(scim_m12 ~ ad_sc_c, data=DATA); summary(lm7); plot(scim_m12 ~ ad_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(scim_m12 ~ rd_sc_c, data=DATA); summary(lm8); plot(scim_m12 ~ rd_sc_c, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")




# mstot - MRI_lumbar
par(mfrow=c(3,3))
lm1 <- lm(mstot_m12 ~ sca_l, data=DATA); summary(lm1); plot(mstot_m12 ~ sca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(mstot_m12 ~ gma_l, data=DATA); summary(lm2); plot(mstot_m12 ~ gma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(mstot_m12 ~ wma_l, data=DATA); summary(lm3); plot(mstot_m12 ~ wma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(mstot_m12 ~ dca_l, data=DATA); summary(lm4); plot(mstot_m12 ~ dca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(mstot_m12 ~ fa_sc_l, data=DATA); summary(lm5); plot(mstot_m12 ~ fa_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(mstot_m12 ~ md_sc_l, data=DATA); summary(lm6); plot(mstot_m12 ~ md_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(mstot_m12 ~ ad_sc_l, data=DATA); summary(lm7); plot(mstot_m12 ~ ad_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(mstot_m12 ~ rd_sc_l, data=DATA); summary(lm8); plot(mstot_m12 ~ rd_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# lttot - MRI_lumbar
par(mfrow=c(3,3))
lm1 <- lm(lttot_m12 ~ sca_l, data=DATA); summary(lm1); plot(lttot_m12 ~ sca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(lttot_m12 ~ gma_l, data=DATA); summary(lm2); plot(lttot_m12 ~ gma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(lttot_m12 ~ wma_l, data=DATA); summary(lm3); plot(lttot_m12 ~ wma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(lttot_m12 ~ dca_l, data=DATA); summary(lm4); plot(lttot_m12 ~ dca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(lttot_m12 ~ fa_sc_l, data=DATA); summary(lm5); plot(lttot_m12 ~ fa_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(lttot_m12 ~ md_sc_l, data=DATA); summary(lm6); plot(lttot_m12 ~ md_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(lttot_m12 ~ ad_sc_l, data=DATA); summary(lm7); plot(lttot_m12 ~ ad_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(lttot_m12 ~ rd_sc_l, data=DATA); summary(lm8); plot(lttot_m12 ~ rd_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# pptot - MRI_lumbar
par(mfrow=c(3,3))
lm1 <- lm(pptot_m12 ~ sca_l, data=DATA); summary(lm1); plot(pptot_m12 ~ sca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(pptot_m12 ~ gma_l, data=DATA); summary(lm2); plot(pptot_m12 ~ gma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(pptot_m12 ~ wma_l, data=DATA); summary(lm3); plot(pptot_m12 ~ wma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(pptot_m12 ~ dca_l, data=DATA); summary(lm4); plot(pptot_m12 ~ dca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(pptot_m12 ~ fa_sc_l, data=DATA); summary(lm5); plot(pptot_m12 ~ fa_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(pptot_m12 ~ md_sc_l, data=DATA); summary(lm6); plot(pptot_m12 ~ md_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(pptot_m12 ~ ad_sc_l, data=DATA); summary(lm7); plot(pptot_m12 ~ ad_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(pptot_m12 ~ rd_sc_l, data=DATA); summary(lm8); plot(pptot_m12 ~ rd_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")

# scim - MRI_lumbar
par(mfrow=c(3,3))
lm1 <- lm(scim_m12 ~ sca_l, data=DATA); summary(lm1); plot(scim_m12 ~ sca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm1, col="red")
lm2 <- lm(scim_m12 ~ gma_l, data=DATA); summary(lm2); plot(scim_m12 ~ gma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm2, col="red")
lm3 <- lm(scim_m12 ~ wma_l, data=DATA); summary(lm3); plot(scim_m12 ~ wma_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm3, col="red")
lm4 <- lm(scim_m12 ~ dca_l, data=DATA); summary(lm4); plot(scim_m12 ~ dca_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm4, col="red")
lm5 <- lm(scim_m12 ~ fa_sc_l, data=DATA); summary(lm5); plot(scim_m12 ~ fa_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm5, col="red")
lm6 <- lm(scim_m12 ~ md_sc_l, data=DATA); summary(lm6); plot(scim_m12 ~ md_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm6, col="red")
lm7 <- lm(scim_m12 ~ ad_sc_l, data=DATA); summary(lm7); plot(scim_m12 ~ ad_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm7, col="red")
lm8 <- lm(scim_m12 ~ rd_sc_l, data=DATA); summary(lm8); plot(scim_m12 ~ rd_sc_l, data=DATA, pch=16, ylim=c(0,110)); abline(lm8, col="red")