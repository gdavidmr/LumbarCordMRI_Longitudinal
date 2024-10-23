
# load in libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(nlme)
library(reshape2)
library(merTools)
library(ggthemes)

# ########################### SET GLOBAL VARIABLES ########################

PATH <<- "D:/projects/lumbar_cord_study"
PATH_FIG <<- paste(PATH, "figures", sep = "/")
RES <<- 0.001

# set parameters for linear mixed effect model
ctrl <<- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 10000, niterEM = 10000)

width  <- 500
height <- 500

# ########################### DATA PREPARATION ###########################

# reading excel file into dataframe
df <- read_excel(path = paste(PATH, "results.xlsx", sep = "/"), sheet = "auxforR")

# dealing with empty entries and empty columns
df <- df[colSums(!is.na(df)) > 1]
df[df=="-"]<-NA

# typecasting
df$id <- round(df$id)
df$group <- as.factor(df$group)
df$id_group <- as.integer(df$id_group)
df$id_lesionanalysis <- as.integer(df$id_lesionanalysis)
df$id_lesion <- as.integer(df$id_lesion)
df$id_tb <- as.integer(df$id_tb)
df$age <- as.integer(df$age)
df$months_ <- as.numeric(df$months_)
df$months <- as.numeric(df$months)
df$sca_c <- as.numeric(df$sca_c); df$sca_l <- as.numeric(df$sca_l); df$gma_c <- as.numeric(df$gma_c); df$gma_l <- as.numeric(df$gma_l);
df$wma_c <- as.numeric(df$wma_c); df$wma_l <- as.numeric(df$wma_l); df$dca_c <- as.numeric(df$dca_c); df$dca_l <- as.numeric(df$dca_l);
df$mstot <- as.numeric(df$mstot); df$uems <- as.numeric(df$uems); df$lems <- as.numeric(df$lems);
df$lttot <- as.numeric(df$lttot); df$uelt <- as.numeric(df$uelt); df$lelt <- as.numeric(df$lelt);
df$pptot <- as.numeric(df$pptot); df$uepp <- as.numeric(df$uepp); df$lepp <- as.numeric(df$lepp);
df$scim <- as.numeric(df$scim);
df$la <- as.numeric(df$la); df$ll <- as.numeric(df$ll); df$lw <- as.numeric(df$lw);
df$tb_total <- as.numeric(df$tb_total); df$tb_ventral <- as.numeric(df$tb_ventral); df$tb_dorsal <- as.numeric(df$tb_dorsal);

df$ssep_lat_p40_tib <- as.numeric(df$ssep_lat_p40_tib)
df$mep_lat_tib <- as.numeric(df$mep_lat_tib)
df$mep_lat_abd <- as.numeric(df$mep_lat_abd)
df$mep_lat_hal <- as.numeric(df$mep_lat_hal)
df$dml_tib <- as.numeric(df$dml_tib)
df$fwave_lat_tib <- as.numeric(df$fwave_lat_tib)
df$fwave_pers_tib <- as.numeric(df$fwave_pers_tib)
df$dml_uln <- as.numeric(df$dml_uln)
df$fwave_lat_uln <- as.numeric(df$fwave_lat_uln)
df$fwave_pers_uln <- as.numeric(df$fwave_pers_uln)

# add new columns to df
df$months2 <- df$months^2
df$monthslog <- log(df$months); df$monthslog[df$monthslog==-Inf] = 0;

# create arrays for baseline cross-sectional areas (CSA) and clinical scores
age_m00 <- df$age[df$months==2 & df$group==1]
sca_c_m00 <- df$sca_c[df$months==2 & df$group==1]
gma_c_m00 <- df$gma_c[df$months==2 & df$group==1]
wma_c_m00 <- df$wma_c[df$months==2 & df$group==1]
dca_c_m00 <- df$dca_c[df$months==2 & df$group==1]
sca_l_m00 <- df$sca_l[df$months==2 & df$group==1]
gma_l_m00 <- df$gma_l[df$months==2 & df$group==1]
wma_l_m00 <- df$wma_l[df$months==2 & df$group==1]
dca_l_m00 <- df$dca_l[df$months==2 & df$group==1]
uems_m00 <- df$uems[df$months==2 & df$group==1]
lems_m00 <- df$lems[df$months==2 & df$group==1]
mstot_m00 <- df$mstot[df$months==2 & df$group==1]
uelt_m00 <- df$uelt[df$months==2 & df$group==1]
lelt_m00 <- df$lelt[df$months==2 & df$group==1]
lttot_m00 <- df$lttot[df$months==2 & df$group==1]
uepp_m00 <- df$uepp[df$months==2 & df$group==1]
lepp_m00 <- df$lepp[df$months==2 & df$group==1]
pptot_m00 <- df$pptot[df$months==2 & df$group==1]
scim_m00 <- df$scim[df$months==2 & df$group==1]
la_m00 <- df$la[df$months==2 & df$group==1]
ll_m00 <- df$ll[df$months==2 & df$group==1]
lw_m00 <- df$lw[df$months==2 & df$group==1]
tb_total_m00 <- df$tb_total[df$months==2 & df$group==1]
tb_ventral_m00 <- df$tb_ventral[df$months==2 & df$group==1]
tb_dorsal_m00 <- df$tb_dorsal[df$months==2 & df$group==1]

ssep_lat_p40_tib_m00 <- df$ssep_lat_p40_tib[df$months==2 & df$group==1]
mep_lat_tib_m00 <- df$mep_lat_tib[df$months==2 & df$group==1]
mep_lat_abd_m00 <- df$mep_lat_abd[df$months==2 & df$group==1]
mep_lat_hal_m00 <- df$mep_lat_hal[df$months==2 & df$group==1]
dml_tib_m00 <- df$dml_tib[df$months==2 & df$group==1]
fwave_lat_tib_m00 <- df$fwave_lat_tib[df$months==2 & df$group==1]
fwave_pers_tib_m00 <- df$fwave_pers_tib[df$months==2 & df$group==1]
dml_uln_m00 <- df$dml_uln[df$months==2 & df$group==1]
fwave_lat_uln_m00 <- df$fwave_lat_uln[df$months==2 & df$group==1]
fwave_pers_uln_m00 <- df$fwave_pers_uln[df$months==2 & df$group==1]


# create arrays for 1y cross-sectional areas (CSA) and 1y clinical scores
sca_c_m12 <- df$sca_c[df$months==18 & df$group==1]
gma_c_m12 <- df$gma_c[df$months==18 & df$group==1]
wma_c_m12 <- df$wma_c[df$months==18 & df$group==1]
dca_c_m12 <- df$dca_c[df$months==18 & df$group==1]
sca_l_m12 <- df$sca_l[df$months==18 & df$group==1]
gma_l_m12 <- df$gma_l[df$months==18 & df$group==1]
wma_l_m12 <- df$wma_l[df$months==18 & df$group==1]
dca_l_m12 <- df$dca_l[df$months==18 & df$group==1]
uems_m12 <- df$uems[df$months==18 & df$group==1]
lems_m12 <- df$lems[df$months==18 & df$group==1]
mstot_m12 <- df$mstot[df$months==18 & df$group==1]
uelt_m12 <- df$uelt[df$months==18 & df$group==1]
lelt_m12 <- df$lelt[df$months==18 & df$group==1]
lttot_m12 <- df$lttot[df$months==18 & df$group==1]
uepp_m12 <- df$uepp[df$months==18 & df$group==1]
lepp_m12 <- df$lepp[df$months==18 & df$group==1]
pptot_m12 <- df$pptot[df$months==18 & df$group==1]
scim_m12 <- df$scim[df$months==18 & df$group==1]
la_m12 <- df$la[df$months==18 & df$group==1]
ll_m12 <- df$ll[df$months==18 & df$group==1]
lw_m12 <- df$lw[df$months==18 & df$group==1]
tb_total_m12 <- df$tb_total[df$months==18 & df$group==1]
tb_ventral_m12 <- df$tb_ventral[df$months==18 & df$group==1]
tb_dorsal_m12 <- df$tb_dorsal[df$months==18 & df$group==1]

# create delta(measures)= value_m08 - value_m02
delta06_sca_c <- df$sca_c[df$months==8 & df$group==1]-df$sca_c[df$months==2 & df$group==1]
delta06_gma_c <- df$gma_c[df$months==8 & df$group==1]-df$gma_c[df$months==2 & df$group==1]
delta06_wma_c <- df$wma_c[df$months==8 & df$group==1]-df$wma_c[df$months==2 & df$group==1]
delta06_dca_c <- df$dca_c[df$months==8 & df$group==1]-df$dca_c[df$months==2 & df$group==1]
delta06_sca_l <- df$sca_l[df$months==8 & df$group==1]-df$sca_l[df$months==2 & df$group==1]
delta06_gma_l <- df$gma_l[df$months==8 & df$group==1]-df$gma_l[df$months==2 & df$group==1]
delta06_wma_l <- df$wma_l[df$months==8 & df$group==1]-df$wma_l[df$months==2 & df$group==1]
delta06_dca_l <- df$dca_l[df$months==8 & df$group==1]-df$dca_l[df$months==2 & df$group==1]
delta06_uems <- df$uems[df$months==8 & df$group==1]-df$uems[df$months==2 & df$group==1]
delta06_lems <- df$lems[df$months==8 & df$group==1]-df$lems[df$months==2 & df$group==1]
delta06_mstot <- df$mstot[df$months==8 & df$group==1]-df$mstot[df$months==2 & df$group==1]
delta06_uelt <- df$uelt[df$months==8 & df$group==1]-df$uelt[df$months==2 & df$group==1]
delta06_lelt <- df$lelt[df$months==8 & df$group==1]-df$lelt[df$months==2 & df$group==1]
delta06_lttot <- df$lttot[df$months==8 & df$group==1]-df$lttot[df$months==2 & df$group==1]
delta06_uepp <- df$uepp[df$months==8 & df$group==1]-df$uepp[df$months==2 & df$group==1]
delta06_lepp <- df$lepp[df$months==8 & df$group==1]-df$lepp[df$months==2 & df$group==1]
delta06_pptot <- df$pptot[df$months==8 & df$group==1]-df$pptot[df$months==2 & df$group==1]
delta06_scim <- df$scim[df$months==8 & df$group==1]-df$scim[df$months==2 & df$group==1]
delta06_la <- df$la[df$months==8 & df$group==1]-df$la[df$months==2 & df$group==1]
delta06_ll <- df$ll[df$months==8 & df$group==1]-df$ll[df$months==2 & df$group==1]
delta06_lw <- df$lw[df$months==8 & df$group==1]-df$lw[df$months==2 & df$group==1]
delta06_tb_total <- df$tb_total[df$months==8 & df$group==1]-df$tb_total[df$months==2 & df$group==1]
delta06_tb_ventral <- df$tb_ventral[df$months==8 & df$group==1]-df$tb_ventral[df$months==2 & df$group==1]
delta06_tb_dorsal <- df$tb_dorsal[df$months==8 & df$group==1]-df$tb_dorsal[df$months==2 & df$group==1]

# create delta(measures)= value_m18 - value_m02
delta12_sca_c <- df$sca_c[df$months==18 & df$group==1]-df$sca_c[df$months==2 & df$group==1]
delta12_gma_c <- df$gma_c[df$months==18 & df$group==1]-df$gma_c[df$months==2 & df$group==1]
delta12_wma_c <- df$wma_c[df$months==18 & df$group==1]-df$wma_c[df$months==2 & df$group==1]
delta12_dca_c <- df$dca_c[df$months==18 & df$group==1]-df$dca_c[df$months==2 & df$group==1]
delta12_sca_l <- df$sca_l[df$months==18 & df$group==1]-df$sca_l[df$months==2 & df$group==1]
delta12_gma_l <- df$gma_l[df$months==18 & df$group==1]-df$gma_l[df$months==2 & df$group==1]
delta12_wma_l <- df$wma_l[df$months==18 & df$group==1]-df$wma_l[df$months==2 & df$group==1]
delta12_dca_l <- df$dca_l[df$months==18 & df$group==1]-df$dca_l[df$months==2 & df$group==1]
delta12_uems <- df$uems[df$months==18 & df$group==1]-df$uems[df$months==2 & df$group==1]
delta12_lems <- df$lems[df$months==18 & df$group==1]-df$lems[df$months==2 & df$group==1]
delta12_mstot <- df$mstot[df$months==18 & df$group==1]-df$mstot[df$months==2 & df$group==1]
delta12_uelt <- df$uelt[df$months==18 & df$group==1]-df$uelt[df$months==2 & df$group==1]
delta12_lelt <- df$lelt[df$months==18 & df$group==1]-df$lelt[df$months==2 & df$group==1]
delta12_lttot <- df$lttot[df$months==18 & df$group==1]-df$lttot[df$months==2 & df$group==1]
delta12_uepp <- df$uepp[df$months==18 & df$group==1]-df$uepp[df$months==2 & df$group==1]
delta12_lepp <- df$lepp[df$months==18 & df$group==1]-df$lepp[df$months==2 & df$group==1]
delta12_pptot <- df$pptot[df$months==18 & df$group==1]-df$pptot[df$months==2 & df$group==1]
delta12_scim <- df$scim[df$months==18 & df$group==1]-df$scim[df$months==2 & df$group==1]
delta12_la <- df$la[df$months==18 & df$group==1]-df$la[df$months==2 & df$group==1]
delta12_ll <- df$ll[df$months==18 & df$group==1]-df$ll[df$months==2 & df$group==1]
delta12_lw <- df$lw[df$months==18 & df$group==1]-df$lw[df$months==2 & df$group==1]
delta12_tb_total <- df$tb_total[df$months==18 & df$group==1]-df$tb_total[df$months==2 & df$group==1]
delta12_tb_ventral <- df$tb_ventral[df$months==18 & df$group==1]-df$tb_ventral[df$months==2 & df$group==1]
delta12_tb_dorsal <- df$tb_dorsal[df$months==18 & df$group==1]-df$tb_dorsal[df$months==2 & df$group==1]

# useful variables
maxTime <<- max(df$months)
numLongCtrl <<- length(df[(df$group==0 & df$id_long==1),]$group)/3
numLongPat  <<- length(df[(df$group==1 & df$id_long==1),]$group)/3

# ########################### FUNCTION DEFINITIONS ###########################

mytheme = list(
    theme_classic()+
        theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
              legend.title = element_blank(),legend.position="bottom", strip.text = element_text(face="bold", size=9),
              axis.text=element_text(face="bold"),axis.title = element_text(face="bold"),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)

lme_1group_linear <- function(df_,y_,x_,logy) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  
  # fit linear mixed effect model including a single term only
  if (logy == FALSE) {
    out <- lme(y ~ x, random = ~ 1 + x | id, na.action = na.omit, control = ctrl) 
  } else {
    out <- lme(log(y) ~ x, random = ~ 1 + x | id, na.action = na.omit, control = ctrl)
  }
  
  summary(out)
  return(out)
  
}

lme_1group_quadratic <- function(df_,y_,x1_,x2_) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y  <- df[[y_]]
  x1 <- df[[x1_]]
  x2 <- df[[x2_]]
  id <- df$id
  
  # fit linear mixed effect model including two terms (linear + quadratic)
  out <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl)
  summary(out)
  
  return(out)
  
}

lme_2group_linear <- function(df_,y_,x_) {
  
  # extract variables from the dataframe
  # include only the longitudinal subjects (controls + patients)
  df <- df_[df_$id_long==1,]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # fit linear mixed effect model including only linear term with group-interaction
  out <- lme(y ~ x*group, random = ~ 1 + x | id, na.action = na.omit, control = ctrl)
  summary(out)
  
  return(out)
  
}

lme_2group_quadratic <- function(df_,y_,x1_,x2_) {
  
  # extract variables from the dataframe
  # include only the longitudinal subjects (controls + patients)
  df <- df_[df_$id_long==1,]
  y <- df[[y_]]
  x1 <- df[[x1_]]
  x2 <- df[[x2_]]
  id <- df$id
  group <- df$group
  
  # fit linear mixed effect model including two terms (linear + quadratic) with group-interaction
  out <- lme(y ~ x1*group + x2*group, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl)
  summary(out)
  
  return(out)
  
}

plot_lme_1group_lin <- function(df_,y_,x_,lme,figname,xlab,ylab,ylim1,ylim2,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit <- rep(seq(2,maxTime,RES), numLongPat)
  if (type=="smooth") {
      yfit <- lme$coefficients$fixed[2]*xfit + 
                  lme$coefficients$fixed[1]
  } else if (type=="line") {
      yfit <- rep(lme$fitted[1:3,1],length(x)/3)
  }
  
  # create and save figure
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  if (type=="smooth") {
    plot_1group_smooth(x,y,id,yfit,xfit,xlab,ylab,ylim1,ylim2)
  } else if (type=="line") {
    plot_1group_line(x,y,id,yfit,xlab,ylab,ylim1,ylim2)
  }
  
}

plot_lme_1group_quad <- function(df_,y_,x_,lme,figname,xlab,ylab,ylim1,ylim2,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit <- rep(seq(2,maxTime,RES), numLongPat)
  if (type=="smooth") {
      yfit <- lme$coefficients$fixed[3]*xfit^2 + 
                  lme$coefficients$fixed[2]*xfit + 
                  lme$coefficients$fixed[1]
  } else if (type=="line") {
      yfit <- rep(lme$fitted[1:3,1],length(x)/3)
  }
  
  # create and save figure
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  if (type=="smooth") {
    plot_1group_smooth(x,y,id,yfit,xfit,xlab,ylab,ylim1,ylim2)
  } else if (type=="line") {
    plot_1group_line(x,y,id,yfit,xlab,ylab,ylim1,ylim2)
  }
  
}

plot_lme_1group_quad2 <- function(df_,y_,x_,lme,figname,xlab,ylab,ylim1,ylim2,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit <- rep(seq(2,maxTime,RES), numLongPat)
  yfit <- lme$coefficients$fixed[3]*xfit^2 + 
            lme$coefficients$fixed[2]*xfit + 
            lme$coefficients$fixed[1]
    
  # for plotting the confidence interval
  y_lower <- intervals(tmp)$fixed[3,1]*xfit^2 + 
                intervals(tmp)$fixed[2,1]*xfit + 
                intervals(tmp)$fixed[1,1]
    
  y_upper <- intervals(tmp)$fixed[3,3]*xfit^2 + 
                intervals(tmp)$fixed[2,3]*xfit + 
                intervals(tmp)$fixed[1,3]
  
  # create and save figure
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  plot_1group_smooth2(x,y,id,yfit,xfit,y_lower,y_upper,xlab,ylab,ylim1,ylim2)
  
}

plot_lme_1group_exp <- function(df_,y_,x_,lme,figname,xlab,ylab,ylim1,ylim2,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[(df_$group==1 & df_$id_long==1),]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit <- rep(seq(2,maxTime,RES), numLongPat)
  if (type=="smooth") {
      yfit <- exp(lme$coefficients$fixed[2]*xfit + lme$coefficients$fixed[1])
    } else if (type=="line") {
      yfit <- exp(rep(lme$fitted[1:3,1],length(x)/3))
  }
  
  # create and save figure
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  if (type=="smooth") {
    plot_1group_smooth(x,y,id,yfit,xfit,xlab,ylab,ylim1,ylim2)
  } else if (type=="line") {
    plot_1group_line(x,y,id,yfit,xlab,ylab,ylim1,ylim2)
  }
  
}

plot_lme_2group_lin <- function(df_,y_,x_,lme,figname,xlab,ylab,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[df_$id_long==1,]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit1  <- rep(seq(2,maxTime,RES),numLongCtrl)
  xfit2  <- rep(seq(2,maxTime,RES),numLongPat)
  if (type=="smooth") {
    yfit1 <- lme$coefficients$fixed[2]*xfit1 + 
      lme$coefficients$fixed[1]
    yfit2 <- (lme$coefficients$fixed[2] + lme$coefficients$fixed[4])*xfit2 +
      lme$coefficients$fixed[1] + lme$coefficients$fixed[3]
  } else if (type=="line") {
    yfit1 <- rep(lme$fitted[1:3,1],length(x)/3)
    yfit2 <- rep(lme$fitted[c("43","44","45"),1],length(x)/3)    
  } 

  # create and save figure
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  if (type=="smooth") {
    plot_2group_smooth(x,y,id,group,yfit1,xfit1,yfit2,xfit2,xlab,ylab)
  } else if (type=="line") {
    plot_2group_line(x,y,id,group,yfit1,yfit2,xlab,ylab)
  }
  
}

plot_lme_2group_quad <- function(df_,y_,x_,lme,figname,xlab,ylab,width,height,type) {
  
  # extract variables from the dataframe
  # include only the longitudinal patients
  df <- df_[df_$id_long==1,]
  y <- df[[y_]]
  x <- df[[x_]]
  id <- df$id
  group <- df$group
  
  # for plotting the fitted curve
  xfit1  <- rep(seq(2,maxTime,RES),numLongCtrl)
  xfit2  <- rep(seq(2,maxTime,RES),numLongPat)
  if (type=="smooth") {
    yfit1 <- lme$coefficients$fixed[4]*xfit1^2 + 
      lme$coefficients$fixed[2]*xfit1 + 
      lme$coefficients$fixed[1]
    yfit2 <- (lme$coefficients$fixed[4] + lme$coefficients$fixed[6])*xfit2^2 + 
      (lme$coefficients$fixed[2] + lme$coefficients$fixed[5])*xfit2 +
      lme$coefficients$fixed[1] + lme$coefficients$fixed[3]
  } else if (type=="line") {
    yfit1 <- rep(lme$fitted[1:3,1],length(x)/3)
    yfit2 <- rep(lme$fitted[c("43","44","45"),1],length(x)/3)    
  } 

  # create and save figure
  #setEPS()
  #postscript(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  png(file = paste(PATH_FIG,figname,sep="/"), width = width, height = height)
  if (type=="smooth") {
    plot_2group_smooth(x,y,id,group,yfit1,xfit1,yfit2,xfit2,xlab,ylab)
  } else if (type=="line") {
    plot_2group_line(x,y,id,group,yfit1,yfit2,xlab,ylab)
  }
  #ggsave(filename=paste(PATH_FIG,figname,sep="/"), plot = last_plot(), scale = 1, width=width, height=height, limitsize = FALSE, device = "eps")
  
}

plot_1group_line <- function(x,y,id,yfit,xlab,ylab,ylim1,ylim2) {
  theme(text = element_text(size=22),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(color = 'red'), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(y = yfit, color = 'red'), size = 3 ) +
    geom_hline(yintercept=0, color='black', linetype=1, alpha=0.2) +
    xlab(xlab) + 
    ylab(ylab) +
    ylim(ylim1,ylim2) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}

plot_1group_smooth <- function(x,y,id,yfit,xfit,xlab,ylab,ylim1,ylim2) {
  theme(text = element_text(size=22),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(color = 'red'), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(x = xfit, y = yfit, group = rep(1:numLongPat,each=length(seq(2,maxTime,RES))), color = 'red'), size = 3 ) +
    geom_hline(yintercept=0, color='black', linetype=1, alpha=0.4) +
    xlab(xlab) +
    ylab(ylab) +
    ylim(ylim1,ylim2) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}

plot_1group_smooth2 <- function(x,y,id,yfit,xfit,y_lower,y_upper,xlab,ylab,ylim1,ylim2) {
  theme(text = element_text(size=22),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(x = xfit, y = yfit, group = rep(1:numLongPat,each=length(seq(2,maxTime,RES))), color = 'red'), size = 3 ) +
    geom_ribbon(aes(x = xfit, ymin = y_lower, ymax = y_upper), alpha = .2) +
    geom_hline(yintercept=0, color='black', linetype=1, alpha=0.4) +
    xlab(xlab) +
    ylab(ylab) +
    ylim(ylim1,ylim2) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}

plot_1group_smooth_log <- function(x,y,id,yfit,xfit,xlab,ylab,ylim1,ylim2) {
  theme(text = element_text(size=22),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(color = 'red'), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(x = xfit, y = yfit, group = rep(1:numLongPat,each=length(seq(log(2),log(maxTime),RES)))), color = "red", size = 3) +
    scale_x_log10(limits = c(0,1)) +
    geom_hline(yintercept=0, color='black', linetype=1, alpha=0.2) +
    xlab(xlab) + 
    ylab(ylab) +
    ylim(ylim1,ylim2) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}

plot_2group_line <- function(x,y,id,group,yfit1,yfit2,xlab,ylab) {
  theme(text = element_text(size=32),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(color = group, group = id), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(x, yfit1, color = 'blue'), size = 3 ) +
    geom_line(aes(x, yfit2, color = 'red'), size = 3 ) +
    scale_colour_manual(values = c("blue","red","blue","red")) +
    xlab(xlab) + 
    ylab(ylab) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}

plot_2group_smooth <- function(x,y,id,group,yfit1,xfit1,yfit2,xfit2,xlab,ylab) {
  theme(text = element_text(size=32),
               panel.border = element_blank(),
               panel.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.line = element_line(colour = "black"),
               axis.text = element_text(size=40),
               axis.title = element_text(size=40,face="plain"))
  ggplot(mapping = aes(x, y, group = id)) +
    geom_line(aes(color = group), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(x = xfit1, y = yfit1, group = rep(1:numLongCtrl,each=length(seq(2,maxTime,RES))), color = "blue"), size = 3) +
    geom_line(aes(x = xfit2, y = yfit2, group = rep(1:numLongPat,each=length(seq(2,maxTime,RES))), color = "red"), size = 3) +
    scale_colour_manual(values = c("blue","red","blue","red")) +
    xlab(xlab) + 
    ylab(ylab) +
    scale_x_continuous(breaks = c(2,8,18)) +
    mytheme
}


maxTime

# ########################### STAT - AGE DIFFERENCES ###########################

wilcox.test(df$age[df$months==2 & df$group==0],
            df$age[df$months==2 & df$group==1],
            aternative = "two.sided", mu = 0, paired = FALSE,
            conf.int = FALSE, conf.level = 0.95)

# Comment: The p-value above is 0.476, therefore, patients and controls do not differ in age

# ########################### STAT - CLINICAL SCORES ###########################

# fit model: y ~ log(months)
print("mstot - logmonth")
summary(lme_mstot_monthslog <- lme_1group_linear(df,'mstot','monthslog','FALSE'))
intervals(lme_mstot_monthslog)
print("uems - logmonth")
summary(lme_uems_monthslog <- lme_1group_linear(df,'uems','monthslog','FALSE'))
intervals(lme_uems_monthslog)
print("lems - logmonth")
summary(lme_lems_monthslog <- lme_1group_linear(df,'lems','monthslog','FALSE'))
intervals(lme_lems_monthslog)
print("lttot - logmonth")
summary(lme_lttot_monthslog <- lme_1group_linear(df,'lttot','monthslog','FALSE'))
intervals(lme_lttot_monthslog)
print("pptot - logmonth")
summary(lme_pptot_monthslog <- lme_1group_linear(df,'pptot','monthslog','FALSE'))
intervals(lme_pptot_monthslog)
print("scim - logmonth")
summary(lme_scim_monthslog <- lme_1group_linear(df,'scim','monthslog','FALSE'))
intervals(lme_scim_monthslog)

# compare significance (p-value) of rate of change between linear and logarithmic model
print("uems - logmonth: logarithmic")
summary(lme_uems_monthslog <- lme_1group_linear(df,'uems','monthslog','FALSE'))
print("uems - month: linear")
summary(lme_uems_months <- lme_1group_linear(df,'uems','months','FALSE'))

print("lems - logmonth: logarithmic")
summary(lme_lems_monthslog <- lme_1group_linear(df,'lems','monthslog','FALSE'))
print("lems - month: linear")
summary(lme_lems_months <- lme_1group_linear(df,'lems','months','FALSE'))

print("lttot - logmonth: logarithmic")
summary(lme_lttot_monthslog <- lme_1group_linear(df,'lttot','monthslog','FALSE'))
print("lttot - month: linear")
summary(lme_lttot_months <- lme_1group_linear(df,'lttot','months','FALSE'))

print("pptot - logmonth: logarithmic")
summary(lme_pptot_monthslog <- lme_1group_linear(df,'pptot','monthslog','FALSE'))
print("pptot - month: linear")
summary(lme_pptot_months <- lme_1group_linear(df,'pptot','months','FALSE'))

print("scim - logmonth: logarithmic")
summary(lme_scim_monthslog <- lme_1group_linear(df,'scim','monthslog','FALSE'))
print("scim - month: linear")
summary(lme_scim_months <- lme_1group_linear(df,'scim','months','FALSE'))

# fit and plot exponential model: y ~ exp(months) -> log(y) ~ months
print("exp(mstot) - month")
summary(lme_exp_mstot_months <- lme_1group_linear(df,'mstot','months','TRUE'))
print("exp(uems) - month")
summary(lme_exp_uems_months <- lme_1group_linear(df,'uems','months','TRUE'))
print("exp(lems) - month")
#summary(lme_exp_lems_months <- lme_1group_linear(df,'lems','months','TRUE'))
print("exp(lttot) - month")
summary(lme_exp_lttot_months <- lme_1group_linear(df,'lttot','months','TRUE'))
print("exp(pptot) - month")
summary(lme_exp_pptot_months <- lme_1group_linear(df,'pptot','months','TRUE'))
print("exp(scim) - month")
summary(lme_exp_scim_months <- lme_1group_linear(df,'scim','months','TRUE'))

plot_lme_1group_exp(df,"mstot","months",lme_exp_mstot_months,"fig_mstot.png","Time after baseline [months]","Motor score",0,100,800,600,"smooth"); dev.off();
plot_lme_1group_exp(df,"uems","months",lme_exp_uems_months,"fig_uems.png","Time after baseline [months]","Upper extremity motor score",0,50,800,600,"smooth"); dev.off();
#plot_lme_1group_exp(df,"lems","months",lme_exp_lems_months,"fig_lems.png","Time after baseline [months]","Lower extremity motor score",0,50,800,600,"smooth"); dev.off();
plot_lme_1group_exp(df,"lttot","months",lme_exp_lttot_months,"fig_lttot.png","Time after baseline [months]","Light touch score",0,112,800,600,"smooth"); dev.off();
plot_lme_1group_exp(df,"pptot","months",lme_exp_pptot_months,"fig_pptot.png","Time after baseline [months]","Pin prick score",0,112,800,600,"smooth"); dev.off();
plot_lme_1group_exp(df,"scim","months",lme_exp_scim_months,"fig_scim.png","Time after baseline [months]","SCIM score",0,100,800,600,"smooth"); dev.off();







# ########################### STAT - LESION PARAMETERS ###########################

df1 <- df[df$group==1 & df$id_lesionanalysis==1 & df$id_lesion==1,]
print("lesion area")
summary(lme_la2 <- lme_1group_quadratic(df1,'la','months','months2'))
summary(lme_la <- lme_1group_linear(df1,'la','months','FALSE'))
print("lesion length")
summary(lme_ll2 <- lme_1group_quadratic(df1,'ll','months','months2'))
summary(lme_ll <- lme_1group_linear(df1,'ll','months','FALSE'))
print("lesion width")
summary(lme_lw2 <- lme_1group_quadratic(df1,'lw','months','months2'))
summary(lme_lw <- lme_1group_linear(df1,'lw','months','FALSE'))
intervals(lme_lw)


df2 <- df[df$group==1 & df$id_lesionanalysis==1 & df$id_tb==1,]
print("lesion tissue bridges total")
summary(lme_tb_total2 <- lme_1group_quadratic(df2,'tb_total','months','months2'))
summary(lme_tb_total <- lme_1group_linear(df2,'tb_total','months','FALSE'))
print("lesion tissue bridges ventral")
#summary(lme_tb_ventral2 <- lme_1group_quadratic(df,'tb_ventral','months','months2'))
summary(lme_tb_ventral <- lme_1group_linear(df2,'tb_ventral','months','FALSE'))
print("lesion tissue bridges dorsal")
#summary(lme_tb_dorsal2 <- lme_1group_quadratic(df,'tb_dorsal','months','months2'))
summary(lme_tb_dorsal <- lme_1group_linear(df2,'tb_dorsal','months','FALSE'))

plot_lme_1group_quad(df,"la","months",lme_la2,"fig_lesion_area.png","Time after baseline [months]","Lesion area [mm^2]",0,150,800,600,"smooth"); dev.off();
plot_lme_1group_quad(df,"ll","months",lme_ll2,"fig_lesion_length.png","Time after baseline [months]","Lesion length [mm]",0,25,800,600,"smooth"); dev.off();
plot_lme_1group_quad(df,"lw","months",lme_lw2,"fig_lesion_width.png","Time after baseline [months]","Lesion width [mm]",0,15,800,600,"smooth"); dev.off();
plot_lme_1group_quad(df,"tb_total","months",lme_tb_total2,"fig_tissue_bridges.png","Time after baseline [months]","Total tissue bridges [mm^2]",0,10,800,600,"smooth"); dev.off();




# ########################### STAT - CROSS-SECTIONAL AREAS ###########################

# baseline comparisons
t.test(df$sca_c[df$months==2 & df$group==0], df$sca_c[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_c[df$months==2 & df$group==0], df$gma_c[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_c[df$months==2 & df$group==0], df$wma_c[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_c[df$months==2 & df$group==0], df$dca_c[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

t.test(df$sca_l[df$months==2 & df$group==0], df$sca_l[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_l[df$months==2 & df$group==0], df$gma_l[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_l[df$months==2 & df$group==0], df$wma_l[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_l[df$months==2 & df$group==0], df$dca_l[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# 6month comparisons
t.test(df$sca_c[df$months==8 & df$group==0], df$sca_c[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_c[df$months==8 & df$group==0], df$gma_c[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_c[df$months==8 & df$group==0], df$wma_c[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_c[df$months==8 & df$group==0], df$dca_c[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

t.test(df$sca_l[df$months==8 & df$group==0], df$sca_l[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_l[df$months==8 & df$group==0], df$gma_l[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_l[df$months==8 & df$group==0], df$wma_l[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_l[df$months==8 & df$group==0], df$dca_l[df$months==8 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# 12month comparisons
t.test(df$sca_c[df$months==18 & df$group==0], df$sca_c[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_c[df$months==18 & df$group==0], df$gma_c[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_c[df$months==18 & df$group==0], df$wma_c[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_c[df$months==18 & df$group==0], df$dca_c[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

t.test(df$sca_l[df$months==18 & df$group==0], df$sca_l[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$gma_l[df$months==18 & df$group==0], df$gma_l[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$wma_l[df$months==18 & df$group==0], df$wma_l[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
t.test(df$dca_l[df$months==18 & df$group==0], df$dca_l[df$months==18 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# longitudinal statistics
print("sca_c")
summary(lme2_sca_c <- lme_2group_quadratic(df,"sca_c","months","months2"))
intervals(lme_1group_quadratic(df,"sca_c","months","months2"), which = "fixed")
summary(lme_sca_c  <- lme_2group_linear(df,"sca_c","months"))
intervals(lme_1group_linear(df,"sca_c","months","FALSE"), which = "fixed")

print("gma_c")
summary(lme2_gma_c <- lme_2group_quadratic(df,"gma_c","months","months2"))
intervals(lme_1group_quadratic(df,"gma_c","months","months2"), which = "fixed")
summary(lme_gma_c  <- lme_2group_linear(df,"gma_c","months"))
intervals(lme_1group_linear(df,"gma_c","months","FALSE"), which = "fixed")

print("wma_c")
summary(lme2_wma_c <- lme_2group_quadratic(df,"wma_c","months","months2"))
intervals(lme_1group_quadratic(df,"wma_c","months","months2"), which = "fixed")
summary(lme_wma_c  <- lme_2group_linear(df,"wma_c","months"))
intervals(lme_1group_linear(df,"wma_c","months","FALSE"), which = "fixed")

print("dca_c")
summary(lme2_dca_c <- lme_2group_quadratic(df,"dca_c","months","months2"))
intervals(lme_1group_quadratic(df,"dca_c","months","months2"), which = "fixed")
summary(lme_dca_c  <- lme_2group_linear(df,"dca_c","months"))
intervals(lme_1group_linear(df,"dca_c","months","FALSE"), which = "fixed")

print("sca_l")
summary(lme2_sca_l <- lme_2group_quadratic(df,"sca_l","months","months2"))
intervals(lme_1group_quadratic(df,"sca_l","months","months2"), which = "fixed")
summary(lme_sca_l  <- lme_2group_linear(df,"sca_l","months"))
intervals(lme_1group_linear(df,"sca_l","months","FALSE"), which = "fixed")

print("gma_l")
summary(lme2_gma_l <- lme_2group_quadratic(df,"gma_l","months","months2"))
intervals(lme_1group_quadratic(df,"gma_l","months","months2"), which = "fixed")
summary(lme_gma_l  <- lme_2group_linear(df,"gma_l","months"))
intervals(lme_1group_linear(df,"gma_l","months","FALSE"), which = "fixed")

print("wma_l")
summary(lme2_wma_l <- lme_2group_quadratic(df,"wma_l","months","months2"))
intervals(lme_1group_quadratic(df,"wma_l","months","months2"), which = "fixed")
summary(lme_wma_l  <- lme_2group_linear(df,"wma_l","months"))
intervals(lme_1group_linear(df,"wma_l","months","FALSE"), which = "fixed")

print("dca_l")
summary(lme2_dca_l <- lme_2group_quadratic(df,"dca_l","months","months2"))
intervals(lme_1group_quadratic(df,"dca_l","months","months2"), which = "fixed")
summary(lme_dca_l  <- lme_2group_linear(df,"dca_l","months"))
intervals(lme_1group_linear(df,"dca_l","months","FALSE"), which = "fixed")



# longitudinal plots
# -> smooth plots (quadratic curves)
plot_lme_2group_quad(df,"sca_c","months",lme2_sca_c,"long_cervical_sca.png","Time after injury [months]","Spinal cord area at C2/C3 [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"gma_c","months",lme2_gma_c,"long_cervical_gma.png","Time after injury [months]","Gray matter area at C2/C3 [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"wma_c","months",lme2_wma_c,"long_cervical_wma.png","Time after injury [months]","White matter area at C2/C3 [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"dca_c","months",lme2_dca_c,"long_cervical_dca.png","Time after injury [months]","Dorsal column area at C2/C3 [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"sca_l","months",lme2_sca_l,"long_lumbar_sca.png","Time after injury [months]","Spinal cord area in the lumbar enlargement [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"gma_l","months",lme2_gma_l,"long_lumbar_gma.png","Time after injury [months]","Gray matter area in the lumbar enlargement [mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"wma_l","months",lme2_wma_l,"long_lumbar_wma.png","Time after injury [months]","White matter area in the lumbar enlargement[mm^2]",500,500,"smooth"); dev.off();
plot_lme_2group_quad(df,"dca_l","months",lme2_dca_l,"long_lumbar_dca.png","Time after injury [months]","Dorsal column area in the lumbar enlargement [mm^2]",500,500,"smooth"); dev.off();

# boxplots
png(file=paste(PATH_FIG,"boxplot_cervical_sca.png",sep = "/"), width=width, height=height)
boxplot(sca_c ~ group*months, data = df, border=(c("blue","red")), ylab = "Spinal cord area at C2/C3 [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(50,115)); dev.off()

png(file=paste(PATH_FIG,"boxplot_cervical_gma.png",sep = "/"), width=width, height=height)
boxplot(gma_c ~ group*months, data = df, border=(c("blue","red")), ylab = "Gray matter area at C2/C3 [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(9,17)); dev.off()

png(file=paste(PATH_FIG,"boxplot_cervical_wma.png",sep = "/"), width=width, height=height)
boxplot(wma_c ~ group*months, data = df, border=(c("blue","red")), ylab = "White matter area at C2/C3 [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(40,100)); dev.off()

png(file=paste(PATH_FIG,"boxplot_cervical_dca.png",sep = "/"), width=width, height=height)
boxplot(dca_c ~ group*months, data = df, border=(c("blue","red")), ylab = "Dorsal column area at C2/C3 [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(13,33)); dev.off()

png(file=paste(PATH_FIG,"boxplot_lumbar_sca.png",sep = "/"), width=width, height=height)
boxplot(sca_l ~ group*months, data = df, border=(c("blue","red")), ylab = "Spinal cord area at the lumbar enlargement [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(30,95)); dev.off()

png(file=paste(PATH_FIG,"boxplot_lumbar_gma.png",sep = "/"), width=width, height=height)
boxplot(gma_l ~ group*months, data = df, border=(c("blue","red")), ylab = "Gray matter area at the lumbar enlargement [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(11,29)); dev.off()

png(file=paste(PATH_FIG,"boxplot_lumbar_wma.png",sep = "/"), width=width, height=height)
boxplot(wma_l ~ group*months, data = df, border=(c("blue","red")), ylab = "White matter area at the lumbar enlargement [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(25,70)); dev.off()

png(file=paste(PATH_FIG,"boxplot_lumbar_dca.png",sep = "/"), width=width, height=height)
boxplot(dca_l ~ group*months, data = df, border=(c("blue","red")), ylab = "Dorsal column area at the lumbar enlargement [mm^2]",
        names=c("controls_m02","patients_m02","controls_m08","patients_m08","controls_m18","patients_m18"), ylim = c(7,27)); dev.off()























# ########################### STAT - DTI ATLAS-BASED ANALYSIS ###########################

df_dti_cervical <- read.csv(paste(PATH,"DTI_atlas_combined_results_cervical.txt",sep="/"))
df_dti_cervical$id <- df$id
df_dti_cervical$group <- df$group
df_dti_cervical$id_long <- df$id_long
df_dti_cervical$months <- df$months
head(df_dti_cervical,5)
str(df_dti_cervical)

df_dti_lumbar <- read.csv(paste(PATH,"DTI_atlas_combined_results_lumbar.txt",sep="/"))
df_dti_lumbar$id <- df$id
df_dti_lumbar$group <- df$group
df_dti_lumbar$id_long <- df$id_long
df_dti_lumbar$months <- df$months
head(df_dti_lumbar,5)
str(df_dti_lumbar)











################################### Cross-sectional analysis ##########################

colnames(df_dti_cervical)[129]

# CS cervical - baseline comparisons
sign_dti_m00 <- vector()

k <- 0
for (i in 1:86) {
    y <- as.numeric(unlist(df_dti_cervical[,i]))
    tmp <- t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
    if (tmp$p.value < 0.05) {
        k <- k+1
        sign_dti_m00[k] = i;
      }
    }
for (i in 87:129) {
    y <- as.numeric(unlist(df_dti_cervical[,i]))
    tmp <- t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)
    if (tmp$p.value < 0.05) {
        k <- k+1
        sign_dti_m00[k] = i;
      }
    }

colnames(df_dti_cervical)[sign_dti_m00]

# CS cervical - White matter
print("Cross-sectional, cervical, White matter  FA")
i <- 39
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, White matter  AD")
i <- 82
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, White matter  RD")
i <- 125
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# linear

# CS cervical - Gray matter
print("Cross-sectional, cervical, Gray matter  FA")
i <- 40
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Gray matter  AD")
i <- 83
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Gray matter  RD")
i <- 126
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS cervical - Dorsal columns
print("Cross-sectional, cervical, Dorsal columns  FA")
i <- 41
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Dorsal columns  AD")
i <- 84
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Dorsal columns  RD")
i <- 127
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS cervical - Lateral funiculi
print("Cross-sectional, cervical, Lateral funiculi  FA")
i <- 42
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Lateral funiculi  AD")
i <- 85
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Lateral funiculi  RD")
i <- 128
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS cervical - Ventral funiculi
print("Cross-sectional, cervical, Ventral funiculi  FA")
i <- 43
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Ventral funiculi  AD")
i <- 86
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, cervical, Ventral funiculi  RD")
i <- 129
y <- as.numeric(unlist(df_dti_cervical[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS lumbar - baseline comparisons
sign_dti_m00 <- vector()
k <- 0
for (i in 1:86) {
    y <- as.numeric(unlist(df_dti_lumbar[,i]))
    tmp <- t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)
    if (tmp$p.value < 0.05) {
        k <- k+1
        sign_dti_m00[k] = i;
      }
    }
for (i in 87:129) {
    y <- as.numeric(unlist(df_dti_lumbar[,i]))
    tmp <- t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)
    if (tmp$p.value < 0.05) {
        k <- k+1
        sign_dti_m00[k] = i;
      }
    }

colnames(df_dti_lumbar)[sign_dti_m00]

# CS lumbar - White matter

print("Cross-sectional, lumbar, White matter  FA")
i <- 39
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, White matter  AD")
i <- 82
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, White matter  RD")
i <- 125
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS lumbar - Gray matter
print("Cross-sectional, lumbar, Gray matter  FA")
i <- 40
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Gray matter  AD")
i <- 83
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Gray matter  RD")
i <- 126
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS lumbar - Dorsal columns
print("Cross-sectional, lumbar, Dorsal columns  FA")
i <- 41
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Dorsal columns  AD")
i <- 84
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Dorsal columns RD")
i <- 127
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS lumbar - Lateral funiculi
print("Cross-sectional, lumbar, Lateral funiculi  FA")
i <- 42
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Lateral funiculi  AD")
i <- 85
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Lateral funiculi  RD")
i <- 128
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# CS lumbar - Ventral funiculi
print("Cross-sectional, lumbar, Ventral funiculi  FA")
i <- 43
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Ventral funiculi  AD")
i <- 86
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE)

print("Cross-sectional, lumbar, Ventral funiculi  RD")
i <- 129
y <- as.numeric(unlist(df_dti_lumbar[,i]))
t.test(y[df$months==2 & df$group==0], y[df$months==2 & df$group==1],
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = FALSE)

# plots

# cervical FA
dfm <- melt(df_dti_cervical[df_dti_cervical$months==2,c(38,39,40,41,42,43,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_cervical_m02_fa.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()

# cervical AD
dfm <- melt(df_dti_cervical[df_dti_cervical$months==2,c(81,82,83,84,85,86,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_cervical_m02_ad.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()

# cervical RD
dfm <- melt(df_dti_cervical[df_dti_cervical$months==2,c(124,125,126,127,128,129,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_cervical_m02_rd.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()

# lumbar FA
dfm <- melt(df_dti_lumbar[df_dti_lumbar$months==2,c(38,39,40,41,42,43,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_lumbar_m02_fa.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()

# lumbar AD
dfm <- melt(df_dti_lumbar[df_dti_lumbar$months==2,c(81,82,83,84,85,86,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_lumbar_m02_ad.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()

# lumbar RD
dfm <- melt(df_dti_lumbar[df_dti_lumbar$months==2,c(124,125,126,127,128,129,130,131,132)], id.vars = c("id","id_long","group"))
png(file=paste(PATH_FIG,"boxplot_lumbar_m02_rd.png",sep = "/"), width=width, height=height)
ggplot(data=dfm) + 
    geom_boxplot(aes(x=group,y=value)) + facet_wrap(~variable)
dev.off()











################################### longitudinal analysis ##########################

# longitudinal cervical - all tracts
sign_dti_long <- vector()

months <- df$months[df$id_long==1]
months2 <- df$months2[df$id_long==1]
group <- df$group[df$id_long==1]
id <- df$id[df$id_long==1]
k <- 0
for (i in 1:129) {
    y <- as.numeric(unlist(df_dti_cervical[,i]))
    y <- y[df$id_long==1]
    lme <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl)
    if (summary(lme)$tTable["months:group1","p-value"] < 0.05) {
        k <- k+1
        sign_dti_long[k] = i;
      }
    }

colnames(df_dti_cervical)[sign_dti_long]

# longitudinal cervical - White matter

print("longitudinal, cervical, White matter,   FA")
i <- 39
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(tmp11 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp12 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, White matter,   AD")
i <- 82
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(tmp31 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp32 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, White matter,   RD")
i <- 125
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(tmp41 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp42 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

plot_lme_2group_lin(df_dti_cervical,"FA_c_white_matter","months",tmp12,"long_cervical_fa_wm.png","Time after injury [months]","Fractional anisotropy",500,500,"smooth"); dev.off();
plot_lme_2group_lin(df_dti_cervical,"AD_c_white_matter","months",tmp32,"long_cervical_ad_wm.png","Time after injury [months]","Axial diffusivity",500,500,"smooth"); dev.off();
plot_lme_2group_lin(df_dti_cervical,"RD_c_white_matter","months",tmp42,"long_cervical_rd_wm.png","Time after injury [months]","Radial diffusivity",500,500,"smooth"); dev.off();

# longitudinal cervical - Gray matter
print("longitudinal, cervical, Gray matter,   FA")
i <- 40
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Gray matter,   AD")
i <- 83
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Gray matter,   RD")
i <- 126
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# longitudinal cervical - Dorsal columns
print("longitudinal, cervical, Dorsal columns,   FA")
i <- 41
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Dorsal columns,   AD")
i <- 84
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Dorsal columns,   RD")
i <- 127
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# longitudinal cervical - Lateral funiculi
print("longitudinal, cervical, Lateral funiculi,   FA")
i <- 42
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Lateral funiculi,   AD")
i <- 85
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Lateral funiculi,   RD")
i <- 128
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# longitudinal cervical - Ventral funiculi
print("longitudinal, cervical, Ventral funiculi,   FA")
i <- 43
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Ventral funiculi,   AD")
i <- 86
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, cervical, Ventral funiculi,   RD")
i <- 129
y <- as.numeric(unlist(df_dti_cervical[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# longitudinal lumbar - all tracts
sign_dti_long <- vector()

months <- df$months[df$id_long==1]
months2 <- df$months2[df$id_long==1]
group <- df$group[df$id_long==1]
id <- df$id[df$id_long==1]


k <- 0
for (i in 1:129) {
    y <- as.numeric(unlist(df_dti_lumbar[,i]))
    y <- y[df$id_long==1]
    lme <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl)
    if (summary(lme)$tTable["months:group1","p-value"] < 0.05) {
        k <- k+1
        sign_dti_long[k] = i;
      }
    }

colnames(df_dti_lumbar)[sign_dti_long]

# longitudinal lumbar - White matter

print("longitudinal, lumbar, White matter,   FA")
i <- 39
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp11 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp12 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, White matter,   AD")
i <- 82
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp31 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp32 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, White matter,   RD")
i <- 125
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp41 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp42 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

plot_lme_2group_lin(df_dti_lumbar,"FA_l_white_matter","months",tmp12,"long_lumbar_fa_wm.png","Time after injury [months]","Fractional anisotropy",500,500,"smooth"); dev.off();
plot_lme_2group_lin(df_dti_lumbar,"AD_l_white_matter","months",tmp32,"long_lumbar_ad_wm.png","Time after injury [months]","Axial diffusivity",500,500,"smooth"); dev.off();
plot_lme_2group_lin(df_dti_lumbar,"RD_l_white_matter","months",tmp42,"long_lumbar_rd_wm.png","Time after injury [months]","Radial diffusivity",500,500,"smooth"); dev.off();

# longitudinal lumbar - Gray matter

print("longitudinal, lumbar, Gray matter,   FA")
i <- 40
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, Gray matter,   AD")
i <- 83
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, Gray matter,   RD")
i <- 126
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# lumbar - Dorsal columns

print("longitudinal, lumbar, Dorsal columns,   FA")
i <- 41
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp11 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp12 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

df1 <- df[df$group==1 & df$id_long==1,]
y1 <- as.numeric(unlist(df_dti_lumbar[,i])) 
df1$y1 <- y1[df$id_long==1 & df$group==1]
intervals(lme(y1 ~ months, random = ~ 1+months | id, data = df1, na.action = na.omit, control = ctrl), which = "fixed")

print("longitudinal, lumbar, Dorsal columns,   AD")
i <- 84
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp21 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp22 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

df1 <- df[df$group==1 & df$id_long==1,]
y1 <- as.numeric(unlist(df_dti_lumbar[,i])) 
df1$y1 <- y1[df$id_long==1 & df$group==1]
intervals(lme(y1 ~ months, random = ~ 1+months | id, data = df1, na.action = na.omit, control = ctrl), which = "fixed")

print("longitudinal, lumbar, Dorsal columns,   RD")
i <- 127
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp31 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp32 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

df1 <- df[df$group==1 & df$id_long==1,]
y1 <- as.numeric(unlist(df_dti_lumbar[,i])) 
df1$y1 <- y1[df$id_long==1 & df$group==1]
intervals(lme(y1 ~ months, random = ~ 1+months | id, data = df1, na.action = na.omit, control = ctrl), which = "fixed")

plot_lme_2group_lin(df_dti_lumbar,"FA_l_dorsal_columns","months",tmp12,"long_lumbar_fa_wm_dorsal.png","Time after injury [months]","Fractional anisotropy",500,500,"smooth"); dev.off();
plot_lme_2group_lin(df_dti_lumbar,"RD_l_dorsal_columns","months",tmp32,"long_lumbar_rd_wm_dorsal.png","Time after injury [months]","Radial diffusivity",500,500,"smooth"); dev.off();

# longitudinal lumbar - Lateral funiculi

print("longitudinal, lumbar, Lateral funiculi,   FA")
i <- 42
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, Lateral funiculi,   AD")
i <- 85
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

print("longitudinal, lumbar, Lateral funiculi,   RD")
i <- 128
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))

# longitudinal lumbar - Ventral funiculi

print("longitudinal, lumbar, Ventral funiculi,   FA")
i <- 43
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp11 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp12 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))


print("longitudinal, lumbar, Ventral funiculi,   AD")
i <- 86
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp31 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp32 <- tmp3 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))


print("longitudinal, lumbar, Ventral funiculi,   RD")
i <- 129
y <- as.numeric(unlist(df_dti_lumbar[,i]))
y <- y[df$id_long==1]
summary(tmp41 <- lme(y ~ months*group + months2*group, random = ~ 1+months+months2 | id, na.action = na.omit, control = ctrl))
summary(tmp42 <- tmp4 <- lme(y ~ months*group, random = ~ 1+months | id, na.action = na.omit, control = ctrl))





























# standardize scores as suggested
df$sca_c_norm <- df$sca_c
df[df$group==0,]$sca_c_norm <- (df[df$group==0,]$sca_c_norm - mean(df[(df$months==2 & df$group==0),]$sca_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$sca_c, na.rm=TRUE)
df[df$group==1,]$sca_c_norm <- (df[df$group==1,]$sca_c_norm - mean(df[(df$months==2 & df$group==0),]$sca_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$sca_c, na.rm=TRUE)

df$gma_c_norm <- df$gma_c
df[df$group==0,]$gma_c_norm <- (df[df$group==0,]$gma_c_norm - mean(df[(df$months==2 & df$group==0),]$gma_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$gma_c, na.rm=TRUE)
df[df$group==1,]$gma_c_norm <- (df[df$group==1,]$gma_c_norm - mean(df[(df$months==2 & df$group==0),]$gma_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$gma_c, na.rm=TRUE)

df$wma_c_norm <- df$wma_c
df[df$group==0,]$wma_c_norm <- (df[df$group==0,]$wma_c_norm - mean(df[(df$months==2 & df$group==0),]$wma_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$wma_c, na.rm=TRUE)
df[df$group==1,]$wma_c_norm <- (df[df$group==1,]$wma_c_norm - mean(df[(df$months==2 & df$group==0),]$wma_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$wma_c, na.rm=TRUE)

df$dca_c_norm <- df$dca_c
df[df$group==0,]$dca_c_norm <- (df[df$group==0,]$dca_c_norm - mean(df[(df$months==2 & df$group==0),]$dca_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$dca_c, na.rm=TRUE)
df[df$group==1,]$dca_c_norm <- (df[df$group==1,]$dca_c_norm - mean(df[(df$months==2 & df$group==0),]$dca_c, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$dca_c, na.rm=TRUE)

df$sca_l_norm <- df$sca_l
df[df$group==0,]$sca_l_norm <- (df[df$group==0,]$sca_l_norm - mean(df[(df$months==2 & df$group==0),]$sca_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$sca_l, na.rm=TRUE)
df[df$group==1,]$sca_l_norm <- (df[df$group==1,]$sca_l_norm - mean(df[(df$months==2 & df$group==0),]$sca_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$sca_l, na.rm=TRUE)

df$gma_l_norm <- df$gma_l
df[df$group==0,]$gma_l_norm <- (df[df$group==0,]$gma_l_norm - mean(df[(df$months==2 & df$group==0),]$gma_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$gma_l, na.rm=TRUE)
df[df$group==1,]$gma_l_norm <- (df[df$group==1,]$gma_l_norm - mean(df[(df$months==2 & df$group==0),]$gma_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$gma_l, na.rm=TRUE)

df$wma_l_norm <- df$wma_l
df[df$group==0,]$wma_l_norm <- (df[df$group==0,]$wma_l_norm - mean(df[(df$months==2 & df$group==0),]$wma_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$wma_l, na.rm=TRUE)
df[df$group==1,]$wma_l_norm <- (df[df$group==1,]$wma_l_norm - mean(df[(df$months==2 & df$group==0),]$wma_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$wma_l, na.rm=TRUE)

df$dca_l_norm <- df$dca_l
df[df$group==0,]$dca_l_norm <- (df[df$group==0,]$dca_l_norm - mean(df[(df$months==2 & df$group==0),]$dca_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$dca_l, na.rm=TRUE)
df[df$group==1,]$dca_l_norm <- (df[df$group==1,]$dca_l_norm - mean(df[(df$months==2 & df$group==0),]$dca_l, na.rm=TRUE)) / sd(df[(df$months==2 & df$group==0),]$dca_l, na.rm=TRUE)

# difference at baseline, GM vs. WM
t.test(df[df$months==2,]$gma_c_norm, df[df$months==2,]$wma_c_norm,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(df[df$months==2,]$gma_l_norm, df[df$months==2,]$wma_l_norm,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

# differences between gma and wma standardized scores
df$diff_gmawma_c_norm <- df$gma_c_norm - df$wma_c_norm
df$diff_gmawma_l_norm <- df$gma_l_norm - df$wma_l_norm

df_ <- df[df$group==1 & df$id_long==1,]
y <- df_$diff_gmawma_c_norm
x1 <- df_$months
x2 <- df_$months2
id <- df_$id
  
summary(tmp11 <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl))
summary(tmp12 <- lme(y ~ x1, random = ~ 1 + x1 | id, na.action = na.omit, control = ctrl))
plot_lme_1group_quad(df_,"diff_gmawma_c_norm","months",tmp11,"GM(norm)vsWM(norm)_cervical.png","Time after baseline [months]","cervical GM - WM (standardized)",-2.5,2.5,width,height,"smooth"); dev.off();

df_ <- df[df$group==1 & df$id_long==1,]
y <- df_$diff_gmawma_l_norm
x1 <- df_$months
x2 <- df_$months2
id <- df_$id

summary(tmp21 <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl))
summary(tmp22 <- lme(y ~ x1, random = ~ 1 + x1 | id, na.action = na.omit, control = ctrl))
plot_lme_1group_quad(df_,"diff_gmawma_l_norm","months",tmp21,"GM(norm)vsWM(norm)_lumbar.png","Time after baseline [months]","lumbar GM - WM (standardized)",-2.5,2.5,width,height,"smooth"); dev.off();

xfit <- rep(seq(0,maxTime,RES), numLongPat)
yfit <- tmp1$coefficients$fixed[3]*xfit^2 + 
            tmp1$coefficients$fixed[2]*xfit + 
            tmp1$coefficients$fixed[1]

print(length(xfit))
print(length(yfit))
print(length(rep(1:numLongPat,each=length(seq(0,maxTime,RES)))))

# create and save figure
png(file = paste(PATH_FIG,"test.png",sep="/"), width = width, height = height)
theme_update(text = element_text(size=22),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
ggplot(mapping = aes(x = df[(df$group==1 & df$id_long==1),]$months, y = df[(df$group==1 & df$id_long==1),]$diff_gmawma_c_norm, group = df[(df$group==1 & df$id_long==1),]$id)) +
    geom_line(aes(color = 'red'), size = 1, alpha = 0.5, linetype = "dashed") +
    geom_ribbon(aes(color = 'red', ymin = a$lwr, ymax = a$upr)) +
    geom_line(aes(x = xfit, y = yfit, group = rep(1:numLongPat,each=length(seq(0,maxTime,RES))), color = 'red'), size = 3 ) +
    geom_hline(yintercept=0, color='black', linetype=1, alpha=0.2) +
    xlab("a") +
    ylab("b") +
    ylim(-2,2) +
    scale_x_continuous(breaks = round(seq(min(df[(df$group==1 & df$id_long==1),]$months), max(df[(df$group==1 & df$id_long==1),]$months), by = 3),1))
dev.off();













# difference at baseline, cervical vs. lumbar
t.test(df[df$months==0,]$sca_c_norm, df[df$months==0,]$sca_l_norm,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(df[df$months==0,]$gma_c_norm, df[df$months==0,]$gma_l_norm,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

t.test(df[df$months==0,]$wma_c_norm, df[df$months==0,]$wma_l_norm,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)

# differences between cervical and lumbar standardized scores
df$diff_cervicallumbar_sca_norm <- df$sca_c_norm - df$sca_l_norm
df$diff_cervicallumbar_gma_norm <- df$gma_c_norm - df$gma_l_norm
df$diff_cervicallumbar_wma_norm <- df$wma_c_norm - df$wma_l_norm

df_ <- df[df$group==1,]
y <- df_$diff_cervicallumbar_sca_norm
x1 <- df_$months
x2 <- df_$months2
id <- df_$id_long
summary(tmp <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl))
plot_lme_1group_quad(df_,"diff_cervicallumbar_sca_norm","months",tmp,"cervicalvslumbar_sca.png","Time after baseline [months]","cervical - lumbar SCA (standardized)",-4,4,width,height,"smooth"); dev.off();

df_ <- df[df$group==1,]
y <- df_$diff_cervicallumbar_gma_norm
x1 <- df_$months
x2 <- df_$months2
id <- df_$id_long
summary(tmp <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl))
plot_lme_1group_quad(df_,"diff_cervicallumbar_gma_norm","months",tmp,"cervicalvslumbar_gma.png","Time after baseline [months]","cervical - lumbar GMA (standardized)",-4,4,width,height,"smooth"); dev.off();

df_ <- df[df$group==1,]
y <- df_$diff_cervicallumbar_wma_norm
x1 <- df_$months
x2 <- df_$months2
id <- df_$id_long
summary(tmp <- lme(y ~ x1 + x2, random = ~ 1 + x1 + x2 | id, na.action = na.omit, control = ctrl))
plot_lme_1group_quad(df_,"diff_cervicallumbar_wma_norm","months",tmp,"cervicalvslumbar_wma.png","Time after baseline [months]","cervical - lumbar WMA (standardized)",-4,4,width,height,"smooth"); dev.off();

# correlation between individual cervical and lumbar standardized values - MONTH00 (baseline)
MONTH <- 0
print(r_sca_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$sca_c_norm, df[(df$months==MONTH & df$group==0),]$sca_l_norm, use="complete.obs"))
print(r_sca_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$sca_c_norm, df[(df$months==MONTH & df$group==1),]$sca_l_norm, use="complete.obs"))
print(r_gma_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$gma_c_norm, df[(df$months==MONTH & df$group==0),]$gma_l_norm, use="complete.obs"))
print(r_gma_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$gma_c_norm, df[(df$months==MONTH & df$group==1),]$gma_l_norm, use="complete.obs"))
print(r_wma_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$wma_c_norm, df[(df$months==MONTH & df$group==0),]$wma_l_norm, use="complete.obs"))
print(r_wma_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$wma_c_norm, df[(df$months==MONTH & df$group==1),]$wma_l_norm, use="complete.obs"))

summary(lm_sca_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$sca_l_norm ~ df[(df$months==MONTH & df$group==0),]$sca_c_norm))
summary(lm_sca_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$sca_l_norm ~ df[(df$months==MONTH & df$group==1),]$sca_c_norm))
summary(lm_gma_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$gma_l_norm ~ df[(df$months==MONTH & df$group==0),]$gma_c_norm))
summary(lm_gma_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$gma_l_norm ~ df[(df$months==MONTH & df$group==1),]$gma_c_norm))
summary(lm_wma_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$wma_l_norm ~ df[(df$months==MONTH & df$group==0),]$wma_c_norm))
summary(lm_wma_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$wma_l_norm ~ df[(df$months==MONTH & df$group==1),]$wma_c_norm))

png(file = paste(PATH_FIG,"cervical_vs_lumbar_sca_individual_m00.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_sca_norm_ctrl$coefficients[1] + lm_sca_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_sca_norm_pat$coefficients[1] + lm_sca_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$sca_c_norm, y=df[(df$months==MONTH & df$group==0),]$sca_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$sca_c_norm, y=df[(df$months==MONTH & df$group==1),]$sca_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical SCA at baseline (standardized)") +
    ylab("Lumbar SCA at baseline (standardized)")
dev.off();

png(file = paste(PATH_FIG,"cervical_vs_lumbar_gma_individual_m00.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$gma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$gma_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_gma_norm_ctrl$coefficients[1] + lm_gma_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$gma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$gma_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_gma_norm_pat$coefficients[1] + lm_gma_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$gma_c_norm, y=df[(df$months==MONTH & df$group==0),]$gma_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$gma_c_norm, y=df[(df$months==MONTH & df$group==1),]$gma_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical GMA at baseline (standardized)") +
    ylab("Lumbar GMA at baseline (standardized)")
dev.off();

png(file = paste(PATH_FIG,"cervical_vs_lumbar_wma_individual_m00.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$wma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$wma_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_wma_norm_ctrl$coefficients[1] + lm_wma_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$wma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$wma_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_wma_norm_pat$coefficients[1] + lm_wma_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$wma_c_norm, y=df[(df$months==MONTH & df$group==0),]$wma_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$wma_c_norm, y=df[(df$months==MONTH & df$group==1),]$wma_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical WMA at baseline (standardized)") +
    ylab("Lumbar WMA at baseline (standardized)")
dev.off();

# correlation between individual cervical and lumbar standardized values - MONTH12
MONTH <- 12
print(r_sca_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$sca_c_norm, df[(df$months==MONTH & df$group==0),]$sca_l_norm, use="complete.obs"))
print(r_sca_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$sca_c_norm, df[(df$months==MONTH & df$group==1),]$sca_l_norm, use="complete.obs"))
print(r_gma_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$gma_c_norm, df[(df$months==MONTH & df$group==0),]$gma_l_norm, use="complete.obs"))
print(r_gma_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$gma_c_norm, df[(df$months==MONTH & df$group==1),]$gma_l_norm, use="complete.obs"))
print(r_wma_norm_ctrl <- cor.test(df[(df$months==MONTH & df$group==0),]$wma_c_norm, df[(df$months==MONTH & df$group==0),]$wma_l_norm, use="complete.obs"))
print(r_wma_norm_pat <- cor.test(df[(df$months==MONTH & df$group==1),]$wma_c_norm, df[(df$months==MONTH & df$group==1),]$wma_l_norm, use="complete.obs"))

summary(lm_sca_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$sca_l_norm ~ df[(df$months==MONTH & df$group==0),]$sca_c_norm))
summary(lm_sca_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$sca_l_norm ~ df[(df$months==MONTH & df$group==1),]$sca_c_norm))
summary(lm_gma_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$gma_l_norm ~ df[(df$months==MONTH & df$group==0),]$gma_c_norm))
summary(lm_gma_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$gma_l_norm ~ df[(df$months==MONTH & df$group==1),]$gma_c_norm))
summary(lm_wma_norm_ctrl <- lm(df[(df$months==MONTH & df$group==0),]$wma_l_norm ~ df[(df$months==MONTH & df$group==0),]$wma_c_norm))
summary(lm_wma_norm_pat <- lm(df[(df$months==MONTH & df$group==1),]$wma_l_norm ~ df[(df$months==MONTH & df$group==1),]$wma_c_norm))

png(file = paste(PATH_FIG,"cervical_vs_lumbar_sca_individual_m12.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_sca_norm_ctrl$coefficients[1] + lm_sca_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_sca_norm_pat$coefficients[1] + lm_sca_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$sca_c_norm, y=df[(df$months==MONTH & df$group==0),]$sca_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$sca_c_norm, y=df[(df$months==MONTH & df$group==1),]$sca_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical SCA at 12 months (standardized)") +
    ylab("Lumbar SCA at 12 months (standardized)")
dev.off();

png(file = paste(PATH_FIG,"cervical_vs_lumbar_gma_individual_m12.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$gma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$gma_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_gma_norm_ctrl$coefficients[1] + lm_gma_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$gma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$gma_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_gma_norm_pat$coefficients[1] + lm_gma_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$gma_c_norm, y=df[(df$months==MONTH & df$group==0),]$gma_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$gma_c_norm, y=df[(df$months==MONTH & df$group==1),]$gma_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical GMA at 12 months (standardized)") +
    ylab("Lumbar GMA at 12 months (standardized)")
dev.off();

png(file = paste(PATH_FIG,"cervical_vs_lumbar_wma_individual_m12.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$wma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$wma_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_wma_norm_ctrl$coefficients[1] + lm_wma_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$wma_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$wma_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_wma_norm_pat$coefficients[1] + lm_wma_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$wma_c_norm, y=df[(df$months==MONTH & df$group==0),]$wma_l_norm), size = 4, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$wma_c_norm, y=df[(df$months==MONTH & df$group==1),]$wma_l_norm), size = 4, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit1), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.3, linetype = 2) +
    xlab("Cervical WMA at 12 months (standardized)") +
    ylab("Lumbar WMA at 12 months (standardized)")
dev.off();

# plot correlations




png(file = paste(PATH_FIG,"cervical_vs_lumbar_sca_individual_m12.png",sep="/"), width = width, height = height)
xfit1 <- seq(from=min(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==0),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit1 <- yfit <- lm_sca_norm_ctrl$coefficients[1] + lm_sca_norm_ctrl$coefficients[2]*xfit1
xfit2 <- seq(from=min(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), to=max(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE), length.out=1000)
yfit2 <- yfit <- lm_sca_norm_pat$coefficients[1] + lm_sca_norm_pat$coefficients[2]*xfit2
ggplot() +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==0),]$sca_c_norm, y=df[(df$months==MONTH & df$group==0),]$sca_l_norm), size = 3, color="blue") +
    geom_point(mapping = aes(x=df[(df$months==MONTH & df$group==1),]$sca_c_norm, y=df[(df$months==MONTH & df$group==1),]$sca_l_norm), size = 3, color="red") +
    geom_line(mapping = aes(x=xfit1, y=yfit2), size = 0.8, color="blue") +
    geom_line(mapping = aes(x=xfit2, y=yfit2), size = 0.8, color="red") +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    xlab("Cervical SCA at 12 months (standardized)") +
    ylab("Lumbar SCA at 12 months (standardized)")
dev.off();

xfit <- seq(from=min(df[(df$months==MONTH & df$group==0),]$sca_c_norm), to=max(df[(df$months==MONTH & df$group==0),]$sca_c_norm), length.out=1000)

min(df[(df$months==MONTH & df$group==1),]$sca_c_norm, na.rm=TRUE)



length(yfit)

lm_sca_norm_ctrl$coefficients[1]





png(file = paste(PATH_FIG,"cervical_vs_lumbar_sca_individual_m12.png",sep="/"), width = width, height = height)
ggplot(mapping = aes(x = df[df$months==12,]$sca_c_norm, y = df[df$months==12,]$sca_l_norm)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, alpha = 0.3) +
    geom_vline(xintercept = 0, alpha = 0.3) +
    xlab("Cervical SCA at 12 months (standardized)") +
    ylab("Lumbar SCA at 12 months (standardized)")
dev.off();































# ########################### CORRELATION ANALYSIS ###########################

# correlations between baseline MRI and 1y clinical scores:

summary(lm(mstot_m12 ~ sca_c_m00,data=df)); summary(lm(uems_m12 ~ sca_c_m00,data=df)); summary(lm(lems_m12 ~ sca_c_m00,data=df));
summary(lm(lttot_m12 ~ sca_c_m00,data=df)); summary(lm(uelt_m12 ~ sca_c_m00,data=df)); summary(lm(lelt_m12 ~ sca_c_m00,data=df));
summary(lm(pptot_m12 ~ sca_c_m00,data=df)); summary(lm(uepp_m12 ~ sca_c_m00,data=df)); summary(lm(lepp_m12 ~ sca_c_m00,data=df));
summary(lm(scim_m12 ~ sca_c_m00,data=df));

summary(lm(mstot_m12 ~ gma_c_m00,data=df)); summary(lm(uems_m12 ~ gma_c_m00,data=df)); summary(lm(lems_m12 ~ gma_c_m00,data=df));
summary(lm(lttot_m12 ~ gma_c_m00,data=df)); summary(lm(uelt_m12 ~ gma_c_m00,data=df)); summary(lm(lelt_m12 ~ gma_c_m00,data=df));
summary(lm(pptot_m12 ~ gma_c_m00,data=df)); summary(lm(uepp_m12 ~ gma_c_m00,data=df)); summary(lm(lepp_m12 ~ gma_c_m00,data=df));
summary(lm(scim_m12 ~ gma_c_m00,data=df));

summary(lm(mstot_m12 ~ wma_c_m00,data=df)); summary(lm(uems_m12 ~ wma_c_m00,data=df)); summary(lm(lems_m12 ~ wma_c_m00,data=df));
summary(lm(lttot_m12 ~ wma_c_m00,data=df)); summary(lm(uelt_m12 ~ wma_c_m00,data=df)); summary(lm(lelt_m12 ~ wma_c_m00,data=df));
summary(lm(pptot_m12 ~ wma_c_m00,data=df)); summary(lm(uepp_m12 ~ wma_c_m00,data=df)); summary(lm(lepp_m12 ~ wma_c_m00,data=df));
summary(lm(scim_m12 ~ wma_c_m00,data=df));

summary(lm(mstot_m12 ~ sca_l_m00,data=df)); summary(lm(uems_m12 ~ sca_l_m00,data=df)); summary(lm(lems_m12 ~ sca_l_m00,data=df));
summary(lm(lttot_m12 ~ sca_l_m00,data=df)); summary(lm(uelt_m12 ~ sca_l_m00,data=df)); summary(lm(lelt_m12 ~ sca_l_m00,data=df));
summary(lm(pptot_m12 ~ sca_l_m00,data=df)); summary(lm(uepp_m12 ~ sca_l_m00,data=df)); summary(lm(lepp_m12 ~ sca_l_m00,data=df));
summary(lm(scim_m12 ~ sca_l_m00,data=df));

summary(lm(mstot_m12 ~ gma_l_m00,data=df)); summary(lm(uems_m12 ~ gma_l_m00,data=df)); summary(lm(lems_m12 ~ gma_l_m00,data=df));
summary(lm(lttot_m12 ~ gma_l_m00,data=df)); summary(lm(uelt_m12 ~ gma_l_m00,data=df)); summary(lm(lelt_m12 ~ gma_l_m00,data=df));
summary(lm(pptot_m12 ~ gma_l_m00,data=df)); summary(lm(uepp_m12 ~ gma_l_m00,data=df)); summary(lm(lepp_m12 ~ gma_l_m00,data=df));
summary(lm(scim_m12 ~ gma_l_m00,data=df));

summary(lm(mstot_m12 ~ wma_l_m00,data=df)); summary(lm(uems_m12 ~ wma_l_m00,data=df)); summary(lm(lems_m12 ~ wma_l_m00,data=df));
summary(lm(lttot_m12 ~ wma_l_m00,data=df)); summary(lm(uelt_m12 ~ wma_l_m00,data=df)); summary(lm(lelt_m12 ~ wma_l_m00,data=df));
summary(lm(pptot_m12 ~ wma_l_m00,data=df)); summary(lm(uepp_m12 ~ wma_l_m00,data=df)); summary(lm(lepp_m12 ~ wma_l_m00,data=df));
summary(lm(scim_m12 ~ wma_l_m00,data=df));


summary(lm(mstot_m12 ~ la_m00,data=df)); summary(lm(uems_m12 ~ la_m00,data=df)); summary(lm(lems_m12 ~ la_m00,data=df));
summary(lm(lttot_m12 ~ la_m00,data=df)); summary(lm(uelt_m12 ~ la_m00,data=df)); summary(lm(lelt_m12 ~ la_m00,data=df));
summary(lm(pptot_m12 ~ la_m00,data=df)); summary(lm(uepp_m12 ~ la_m00,data=df)); summary(lm(lepp_m12 ~ la_m00,data=df));
summary(lm(scim_m12 ~ la_m00,data=df));

summary(lm(mstot_m12 ~ ll_m00,data=df)); summary(lm(uems_m12 ~ ll_m00,data=df)); summary(lm(lems_m12 ~ ll_m00,data=df));
summary(lm(lttot_m12 ~ ll_m00,data=df)); summary(lm(uelt_m12 ~ ll_m00,data=df)); summary(lm(lelt_m12 ~ ll_m00,data=df));
summary(lm(pptot_m12 ~ ll_m00,data=df)); summary(lm(uepp_m12 ~ ll_m00,data=df)); summary(lm(lepp_m12 ~ ll_m00,data=df));
summary(lm(scim_m12 ~ ll_m00,data=df));

summary(lm(mstot_m12 ~ lw_m00,data=df)); summary(lm(uems_m12 ~ lw_m00,data=df)); summary(lm(lems_m12 ~ lw_m00,data=df));
summary(lm(lttot_m12 ~ lw_m00,data=df)); summary(lm(uelt_m12 ~ lw_m00,data=df)); summary(lm(lelt_m12 ~ lw_m00,data=df));
summary(lm(pptot_m12 ~ lw_m00,data=df)); summary(lm(uepp_m12 ~ lw_m00,data=df)); summary(lm(lepp_m12 ~ lw_m00,data=df));
summary(lm(scim_m12 ~ lw_m00,data=df));

summary(lm(mstot_m12 ~ tb_total_m00,data=df)); summary(lm(uems_m12 ~ tb_total_m00,data=df)); summary(lm(lems_m12 ~ tb_total_m00,data=df));
summary(lm(lttot_m12 ~ tb_total_m00,data=df)); summary(lm(uelt_m12 ~ tb_total_m00,data=df)); summary(lm(lelt_m12 ~ tb_total_m00,data=df));
summary(lm(pptot_m12 ~ tb_total_m00,data=df)); summary(lm(uepp_m12 ~ tb_total_m00,data=df)); summary(lm(lepp_m12 ~ tb_total_m00,data=df));
summary(lm(scim_m12 ~ tb_total_m00,data=df));

summary(lm(mstot_m12 ~ tb_ventral_m00,data=df)); summary(lm(uems_m12 ~ tb_ventral_m00,data=df)); summary(lm(lems_m12 ~ tb_ventral_m00,data=df));
summary(lm(lttot_m12 ~ tb_ventral_m00,data=df)); summary(lm(uelt_m12 ~ tb_ventral_m00,data=df)); summary(lm(lelt_m12 ~ tb_ventral_m00,data=df));
summary(lm(pptot_m12 ~ tb_ventral_m00,data=df)); summary(lm(uepp_m12 ~ tb_ventral_m00,data=df)); summary(lm(lepp_m12 ~ tb_ventral_m00,data=df));
summary(lm(scim_m12 ~ tb_ventral_m00,data=df));

summary(lm(mstot_m12 ~ tb_dorsal_m00,data=df)); summary(lm(uems_m12 ~ tb_dorsal_m00,data=df)); summary(lm(lems_m12 ~ tb_dorsal_m00,data=df));
summary(lm(lttot_m12 ~ tb_dorsal_m00,data=df)); summary(lm(uelt_m12 ~ tb_dorsal_m00,data=df)); summary(lm(lelt_m12 ~ tb_dorsal_m00,data=df));
summary(lm(pptot_m12 ~ tb_dorsal_m00,data=df)); summary(lm(uepp_m12 ~ tb_dorsal_m00,data=df)); summary(lm(lepp_m12 ~ tb_dorsal_m00,data=df));
summary(lm(scim_m12 ~ tb_dorsal_m00,data=df));

# correlations between baseline CSA and delta clinical scores (0-12 months)
summary(lm(delta12_mstot ~ sca_c_m00, data=df)); summary(lm(delta12_uems ~ sca_c_m00, data=df)); summary(lm(delta12_lems ~ sca_c_m00, data=df))
summary(lm(delta12_lttot ~ sca_c_m00, data=df)); summary(lm(delta12_uelt ~ sca_c_m00, data=df)); summary(lm(delta12_lelt ~ sca_c_m00, data=df))
summary(lm(delta12_pptot ~ sca_c_m00, data=df)); summary(lm(delta12_uepp ~ sca_c_m00, data=df)); summary(lm(delta12_lepp ~ sca_c_m00, data=df))
summary(lm(delta12_scim ~ sca_c_m00, data=df));

summary(lm(delta12_mstot ~ gma_c_m00, data=df)); summary(lm(delta12_uems ~ gma_c_m00, data=df)); summary(lm(delta12_lems ~ gma_c_m00, data=df))
summary(lm(delta12_lttot ~ gma_c_m00, data=df)); summary(lm(delta12_uelt ~ gma_c_m00, data=df)); summary(lm(delta12_lelt ~ gma_c_m00, data=df))
summary(lm(delta12_pptot ~ gma_c_m00, data=df)); summary(lm(delta12_uepp ~ gma_c_m00, data=df)); summary(lm(delta12_lepp ~ gma_c_m00, data=df))
summary(lm(delta12_scim ~ gma_c_m00, data=df));

summary(lm(delta12_mstot ~ wma_c_m00, data=df)); summary(lm(delta12_uems ~ wma_c_m00, data=df)); summary(lm(delta12_lems ~ wma_c_m00, data=df))
summary(lm(delta12_lttot ~ wma_c_m00, data=df)); summary(lm(delta12_uelt ~ wma_c_m00, data=df)); summary(lm(delta12_lelt ~ wma_c_m00, data=df))
summary(lm(delta12_pptot ~ wma_c_m00, data=df)); summary(lm(delta12_uepp ~ wma_c_m00, data=df)); summary(lm(delta12_lepp ~ wma_c_m00, data=df))
summary(lm(delta12_scim ~ wma_c_m00, data=df));

summary(lm(delta12_mstot ~ sca_l_m00, data=df)); summary(lm(delta12_uems ~ sca_l_m00, data=df)); summary(lm(delta12_lems ~ sca_l_m00, data=df))
summary(lm(delta12_lttot ~ sca_l_m00, data=df)); summary(lm(delta12_uelt ~ sca_l_m00, data=df)); summary(lm(delta12_lelt ~ sca_l_m00, data=df))
summary(lm(delta12_pptot ~ sca_l_m00, data=df)); summary(lm(delta12_uepp ~ sca_l_m00, data=df)); summary(lm(delta12_lepp ~ sca_l_m00, data=df))
summary(lm(delta12_scim ~ sca_l_m00, data=df));

summary(lm(delta12_mstot ~ gma_l_m00, data=df)); summary(lm(delta12_uems ~ gma_l_m00, data=df)); summary(lm(delta12_lems ~ gma_l_m00, data=df))
summary(lm(delta12_lttot ~ gma_l_m00, data=df)); summary(lm(delta12_uelt ~ gma_l_m00, data=df)); summary(lm(delta12_lelt ~ gma_l_m00, data=df))
summary(lm(delta12_pptot ~ gma_l_m00, data=df)); summary(lm(delta12_uepp ~ gma_l_m00, data=df)); summary(lm(delta12_lepp ~ gma_l_m00, data=df))
summary(lm(delta12_scim ~ gma_l_m00, data=df));

summary(lm(delta12_mstot ~ wma_l_m00, data=df)); summary(lm(delta12_uems ~ wma_l_m00, data=df)); summary(lm(delta12_lems ~ wma_l_m00, data=df))
summary(lm(delta12_lttot ~ wma_l_m00, data=df)); summary(lm(delta12_uelt ~ wma_l_m00, data=df)); summary(lm(delta12_lelt ~ wma_l_m00, data=df))
summary(lm(delta12_pptot ~ wma_l_m00, data=df)); summary(lm(delta12_uepp ~ wma_l_m00, data=df)); summary(lm(delta12_lepp ~ wma_l_m00, data=df))
summary(lm(delta12_scim ~ wma_l_m00, data=df));


summary(lm(delta12_mstot ~ la_m00, data=df)); summary(lm(delta12_uems ~ la_m00, data=df)); summary(lm(delta12_lems ~ la_m00, data=df))
summary(lm(delta12_lttot ~ la_m00, data=df)); summary(lm(delta12_uelt ~ la_m00, data=df)); summary(lm(delta12_lelt ~ la_m00, data=df))
summary(lm(delta12_pptot ~ la_m00, data=df)); summary(lm(delta12_uepp ~ la_m00, data=df)); summary(lm(delta12_lepp ~ la_m00, data=df))
summary(lm(delta12_scim ~ la_m00, data=df));

summary(lm(delta12_mstot ~ ll_m00, data=df)); summary(lm(delta12_uems ~ ll_m00, data=df)); summary(lm(delta12_lems ~ ll_m00, data=df))
summary(lm(delta12_lttot ~ ll_m00, data=df)); summary(lm(delta12_uelt ~ ll_m00, data=df)); summary(lm(delta12_lelt ~ ll_m00, data=df))
summary(lm(delta12_pptot ~ ll_m00, data=df)); summary(lm(delta12_uepp ~ ll_m00, data=df)); summary(lm(delta12_lepp ~ ll_m00, data=df))
summary(lm(delta12_scim ~ ll_m00, data=df));

summary(lm(delta12_mstot ~ lw_m00, data=df)); summary(lm(delta12_uems ~ lw_m00, data=df)); summary(lm(delta12_lems ~ lw_m00, data=df))
summary(lm(delta12_lttot ~ lw_m00, data=df)); summary(lm(delta12_uelt ~ lw_m00, data=df)); summary(lm(delta12_lelt ~ lw_m00, data=df))
summary(lm(delta12_pptot ~ lw_m00, data=df)); summary(lm(delta12_uepp ~ lw_m00, data=df)); summary(lm(delta12_lepp ~ lw_m00, data=df))
summary(lm(delta12_scim ~ lw_m00, data=df));

summary(lm(delta12_mstot ~ tb_total_m00, data=df)); summary(lm(delta12_uems ~ tb_total_m00, data=df)); summary(lm(delta12_lems ~ tb_total_m00, data=df))
summary(lm(delta12_lttot ~ tb_total_m00, data=df)); summary(lm(delta12_uelt ~ tb_total_m00, data=df)); summary(lm(delta12_lelt ~ tb_total_m00, data=df))
summary(lm(delta12_pptot ~ tb_total_m00, data=df)); summary(lm(delta12_uepp ~ tb_total_m00, data=df)); summary(lm(delta12_lepp ~ tb_total_m00, data=df))
summary(lm(delta12_scim ~ tb_total_m00, data=df));

summary(lm(delta12_mstot ~ tb_ventral_m00, data=df)); summary(lm(delta12_uems ~ tb_ventral_m00, data=df)); summary(lm(delta12_lems ~ tb_ventral_m00, data=df))
summary(lm(delta12_lttot ~ tb_ventral_m00, data=df)); summary(lm(delta12_uelt ~ tb_ventral_m00, data=df)); summary(lm(delta12_lelt ~ tb_ventral_m00, data=df))
summary(lm(delta12_pptot ~ tb_ventral_m00, data=df)); summary(lm(delta12_uepp ~ tb_ventral_m00, data=df)); summary(lm(delta12_lepp ~ tb_ventral_m00, data=df))
summary(lm(delta12_scim ~ tb_ventral_m00, data=df));

summary(lm(delta12_mstot ~ tb_dorsal_m00, data=df)); summary(lm(delta12_uems ~ tb_dorsal_m00, data=df)); summary(lm(delta12_lems ~ tb_dorsal_m00, data=df))
summary(lm(delta12_lttot ~ tb_dorsal_m00, data=df)); summary(lm(delta12_uelt ~ tb_dorsal_m00, data=df)); summary(lm(delta12_lelt ~ tb_dorsal_m00, data=df))
summary(lm(delta12_pptot ~ tb_dorsal_m00, data=df)); summary(lm(delta12_uepp ~ tb_dorsal_m00, data=df)); summary(lm(delta12_lepp ~ tb_dorsal_m00, data=df))
summary(lm(delta12_scim ~ tb_dorsal_m00, data=df));

# correlations between delta CSA (0-12months) and delta clinical scores (0-12 months):
summary(lm(delta12_mstot ~ delta12_sca_c,data=df)); summary(lm(delta12_uems ~ delta12_sca_c,data=df));  summary(lm(delta12_lems ~ delta12_sca_c,data=df)); 
summary(lm(delta12_lttot ~ delta12_sca_c,data=df)); summary(lm(delta12_uelt ~ delta12_sca_c,data=df));  summary(lm(delta12_lelt ~ delta12_sca_c,data=df));
summary(lm(delta12_pptot ~ delta12_sca_c,data=df)); summary(lm(delta12_uepp ~ delta12_sca_c,data=df));  summary(lm(delta12_lepp ~ delta12_sca_c,data=df));

summary(lm(delta12_mstot ~ delta12_gma_c,data=df)); summary(lm(delta12_uems ~ delta12_gma_c,data=df));  summary(lm(delta12_lems ~ delta12_gma_c,data=df)); 
summary(lm(delta12_lttot ~ delta12_gma_c,data=df)); summary(lm(delta12_uelt ~ delta12_gma_c,data=df));  summary(lm(delta12_lelt ~ delta12_gma_c,data=df));
summary(lm(delta12_pptot ~ delta12_gma_c,data=df)); summary(lm(delta12_uepp ~ delta12_gma_c,data=df));  summary(lm(delta12_lepp ~ delta12_gma_c,data=df));

summary(lm(delta12_mstot ~ delta12_wma_c,data=df)); summary(lm(delta12_uems ~ delta12_wma_c,data=df));  summary(lm(delta12_lems ~ delta12_wma_c,data=df)); 
summary(lm(delta12_lttot ~ delta12_wma_c,data=df)); summary(lm(delta12_uelt ~ delta12_wma_c,data=df));  summary(lm(delta12_lelt ~ delta12_wma_c,data=df));
summary(lm(delta12_pptot ~ delta12_wma_c,data=df)); summary(lm(delta12_uepp ~ delta12_wma_c,data=df));  summary(lm(delta12_lepp ~ delta12_wma_c,data=df));

summary(lm(delta12_mstot ~ delta12_sca_l,data=df)); summary(lm(delta12_uems ~ delta12_sca_l,data=df));  summary(lm(delta12_lems ~ delta12_sca_l,data=df)); 
summary(lm(delta12_lttot ~ delta12_sca_l,data=df)); summary(lm(delta12_uelt ~ delta12_sca_l,data=df));  summary(lm(delta12_lelt ~ delta12_sca_l,data=df));
summary(lm(delta12_pptot ~ delta12_sca_l,data=df)); summary(lm(delta12_uepp ~ delta12_sca_l,data=df));  summary(lm(delta12_lepp ~ delta12_sca_l,data=df));

summary(lm(delta12_mstot ~ delta12_gma_l,data=df)); summary(lm(delta12_uems ~ delta12_gma_l,data=df));  summary(lm(delta12_lems ~ delta12_gma_l,data=df)); 
summary(lm(delta12_lttot ~ delta12_gma_l,data=df)); summary(lm(delta12_uelt ~ delta12_gma_l,data=df));  summary(lm(delta12_lelt ~ delta12_gma_l,data=df));
summary(lm(delta12_pptot ~ delta12_gma_l,data=df)); summary(lm(delta12_uepp ~ delta12_gma_l,data=df));  summary(lm(delta12_lepp ~ delta12_gma_l,data=df));

summary(lm(delta12_mstot ~ delta12_wma_l,data=df)); summary(lm(delta12_uems ~ delta12_wma_l,data=df));  summary(lm(delta12_lems ~ delta12_wma_l,data=df)); 
summary(lm(delta12_lttot ~ delta12_wma_l,data=df)); summary(lm(delta12_uelt ~ delta12_wma_l,data=df));  summary(lm(delta12_lelt ~ delta12_wma_l,data=df));
summary(lm(delta12_pptot ~ delta12_wma_l,data=df)); summary(lm(delta12_uepp ~ delta12_wma_l,data=df));  summary(lm(delta12_lepp ~ delta12_wma_l,data=df));

# correlations between delta MRI (12 months) and 1y clinical scores
# corrected for baseline and age

summary(lm(mstot_m12 ~ delta12_sca_c + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_sca_c + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_sca_c + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_sca_c + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_sca_c + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_sca_c + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_sca_c + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_sca_c + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_sca_c+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_sca_c + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_gma_c + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_gma_c + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_gma_c + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_gma_c + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_gma_c + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_gma_c + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_gma_c + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_gma_c + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_gma_c+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_gma_c + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_wma_c + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_wma_c + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_wma_c + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_wma_c + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_wma_c + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_wma_c + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_wma_c + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_wma_c + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_wma_c+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_wma_c + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_sca_l + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_sca_l + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_sca_l + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_sca_l + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_sca_l + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_sca_l + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_sca_l + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_sca_l + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_sca_l+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_sca_l + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_gma_l + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_gma_l + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_gma_l + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_gma_l + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_gma_l + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_gma_l + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_gma_l + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_gma_l + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_gma_l+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_gma_l + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_wma_l + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_wma_l + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_wma_l + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_wma_l + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_wma_l + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_wma_l + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_wma_l + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_wma_l + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_wma_l+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_wma_l + scim_m00 + age_m00,data=df));



summary(lm(mstot_m12 ~ delta12_la + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_la + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_la + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_la + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_la + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_la + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_la + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_la + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_la + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_la + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_ll + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_ll + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_ll + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_ll + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_ll + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_ll + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_ll + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_ll + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_ll + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_ll + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_lw + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_lw + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_lw + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_lw + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_lw + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_lw + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_lw + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_lw + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_lw + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_lw + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ delta12_tb_total + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ delta12_tb_total + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ delta12_tb_total + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ delta12_tb_total + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ delta12_tb_total + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ delta12_tb_total + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ delta12_tb_total + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ delta12_tb_total + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ delta12_tb_total + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ delta12_tb_total + scim_m00 + age_m00,data=df));


# correlations between delta MRI (12 months) and 1y clinical scores
# corrected for baseline

summary(lm(mstot_m12 ~ delta12_sca_c + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_sca_c + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_sca_c + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_sca_c + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_sca_c + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_sca_c + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_sca_c + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_sca_c + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_sca_c+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_sca_c + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_gma_c + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_gma_c + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_gma_c + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_gma_c + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_gma_c + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_gma_c + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_gma_c + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_gma_c + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_gma_c+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_gma_c + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_wma_c + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_wma_c + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_wma_c + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_wma_c + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_wma_c + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_wma_c + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_wma_c + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_wma_c + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_wma_c+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_wma_c + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_sca_l + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_sca_l + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_sca_l + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_sca_l + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_sca_l + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_sca_l + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_sca_l + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_sca_l + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_sca_l+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_sca_l + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_gma_l + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_gma_l + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_gma_l + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_gma_l + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_gma_l + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_gma_l + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_gma_l + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_gma_l + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_gma_l+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_gma_l + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_wma_l + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_wma_l + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_wma_l + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_wma_l + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_wma_l + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_wma_l + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_wma_l + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_wma_l + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_wma_l+ lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_wma_l + scim_m00,data=df));



summary(lm(mstot_m12 ~ delta12_la + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_la + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_la + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_la + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_la + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_la + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_la + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_la + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_la + lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_la + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_ll + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_ll + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_ll + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_ll + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_ll + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_ll + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_ll + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_ll + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_ll + lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_ll + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_lw + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_lw + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_lw + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_lw + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_lw + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_lw + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_lw + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_lw + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_lw + lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_lw + scim_m00,data=df));

summary(lm(mstot_m12 ~ delta12_tb_total + mstot_m00,data=df)); summary(lm(uems_m12 ~ delta12_tb_total + uems_m00,data=df)); summary(lm(lems_m12 ~ delta12_tb_total + lems_m00,data=df));
summary(lm(lttot_m12 ~ delta12_tb_total + lttot_m00,data=df)); summary(lm(uelt_m12 ~ delta12_tb_total + uelt_m00,data=df)); summary(lm(lelt_m12 ~ delta12_tb_total + lelt_m00,data=df));
summary(lm(pptot_m12 ~ delta12_tb_total + pptot_m00,data=df)); summary(lm(uepp_m12 ~ delta12_tb_total + uepp_m00,data=df)); summary(lm(lepp_m12 ~ delta12_tb_total + lepp_m00,data=df));
summary(lm(scim_m12 ~ delta12_tb_total + scim_m00,data=df));


# correlations between baseline MRI and 1y clinical scores
# corrected for baseline

summary(lm(mstot_m12 ~ sca_c_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ sca_c_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ sca_c_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ sca_c_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ sca_c_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ sca_c_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ sca_c_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ sca_c_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ sca_c_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ sca_c_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ gma_c_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ gma_c_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ gma_c_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ gma_c_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ gma_c_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ gma_c_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ gma_c_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ gma_c_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ gma_c_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ gma_c_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ wma_c_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ wma_c_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ wma_c_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ wma_c_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ wma_c_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ wma_c_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ wma_c_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ wma_c_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ wma_c_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ wma_c_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ sca_l_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ sca_l_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ sca_l_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ sca_l_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ sca_l_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ sca_l_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ sca_l_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ sca_l_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ sca_l_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ sca_l_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ gma_l_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ gma_l_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ gma_l_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ gma_l_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ gma_l_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ gma_l_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ gma_l_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ gma_l_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ gma_l_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ gma_l_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ wma_l_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ wma_l_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ wma_l_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ wma_l_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ wma_l_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ wma_l_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ wma_l_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ wma_l_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ wma_l_m00+ lepp_m00,data=df));
summary(lm(scim_m12 ~ wma_l_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ la_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ la_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ la_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ la_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ la_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ la_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ la_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ la_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ la_m00 + lepp_m00,data=df));
summary(lm(scim_m12 ~ la_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ ll_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ ll_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ ll_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ ll_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ ll_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ ll_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ ll_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ ll_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ ll_m00 + lepp_m00,data=df));
summary(lm(scim_m12 ~ ll_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ lw_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ lw_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ lw_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ lw_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ lw_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ lw_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ lw_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ lw_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ lw_m00 + lepp_m00,data=df));
summary(lm(scim_m12 ~ lw_m00 + scim_m00,data=df));

summary(lm(mstot_m12 ~ tb_total_m00 + mstot_m00,data=df)); summary(lm(uems_m12 ~ tb_total_m00 + uems_m00,data=df)); summary(lm(lems_m12 ~ tb_total_m00 + lems_m00,data=df));
summary(lm(lttot_m12 ~ tb_total_m00 + lttot_m00,data=df)); summary(lm(uelt_m12 ~ tb_total_m00 + uelt_m00,data=df)); summary(lm(lelt_m12 ~ tb_total_m00 + lelt_m00,data=df));
summary(lm(pptot_m12 ~ tb_total_m00 + pptot_m00,data=df)); summary(lm(uepp_m12 ~ tb_total_m00 + uepp_m00,data=df)); summary(lm(lepp_m12 ~ tb_total_m00 + lepp_m00,data=df));
summary(lm(scim_m12 ~ tb_total_m00 + scim_m00,data=df));


# correlations between baseline MRI and 1y clinical scores
# corrected for baseline

# cervical, dorsal column, FA
print("Cervical, dorsal column, FA")
y <- as.numeric(unlist(df_dti_cervical[,41]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, dorsal column, AD
print("Cervical, dorsal column, AD")
y <- as.numeric(unlist(df_dti_cervical[,84]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, dorsal column, RD
print("Cervical, dorsal column, RD")
y <- as.numeric(unlist(df_dti_cervical[,127]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));



# cervical, lateral column, FA
print("Cervical, lateral column, FA")
y <- as.numeric(unlist(df_dti_cervical[,42]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, lateral column, AD
print("Cervical, lateral column, AD")
y <- as.numeric(unlist(df_dti_cervical[,85]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, lateral column, RD
print("Cervical, lateral column, RD")
y <- as.numeric(unlist(df_dti_cervical[,128]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));



# cervical, ventral column, FA
print("Cervical, ventral column, FA")
y <- as.numeric(unlist(df_dti_cervical[,43]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, ventral column, AD
print("Cervical, ventral column, AD")
y <- as.numeric(unlist(df_dti_cervical[,86]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# cervical, ventral column, RD
print("Cervical, ventral column, RD")
y <- as.numeric(unlist(df_dti_cervical[,129]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

plot()

# correlations between baseline MRI and 1y clinical scores
# corrected for baseline

# lumbar, dorsal column, FA
print("Lumbar, dorsal column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,41]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, dorsal column, AD
print("Lumbar, dorsal column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,84]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, dorsal column, RD
print("Lumbar, dorsal column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,127]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));



# lumbar, lateral column, FA
print("Lumbar, lateral column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,42]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, lateral column, AD
print("Lumbar, lateral column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,85]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, lateral column, RD
print("Lumbar, lateral column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,128]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));



# lumbar, ventral column, FA
print("Lumbar, ventral column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,43]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, ventral column, AD
print("Lumbar, ventral column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,86]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));

# lumbar, ventral column, RD
print("Lumbar, ventral column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,129]))
y <- y[df$months==2 & df$group==1]
summary(lm(mstot_m12 ~ y + mstot_m00)); summary(lm(uems_m12 ~ y + uems_m00)); summary(lm(lems_m12 ~ y + lems_m00));
summary(lm(lttot_m12 ~ y + lttot_m00)); summary(lm(uelt_m12 ~ y + uelt_m00)); summary(lm(lelt_m12 ~ y + lelt_m00));
summary(lm(pptot_m12 ~ y + pptot_m00)); summary(lm(uepp_m12 ~ y + uepp_m00)); summary(lm(lepp_m12 ~ y + lepp_m00));
summary(lm(scim_m12 ~ y + scim_m00));



# correlations between baseline MRI and 1y clinical scores
# corrected for baseline and age

summary(lm(mstot_m12 ~ sca_c_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ sca_c_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ sca_c_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ sca_c_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ sca_c_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ sca_c_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ sca_c_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ sca_c_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ sca_c_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ sca_c_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ gma_c_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ gma_c_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ gma_c_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ gma_c_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ gma_c_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ gma_c_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ gma_c_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ gma_c_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ gma_c_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ gma_c_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ wma_c_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ wma_c_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ wma_c_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ wma_c_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ wma_c_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ wma_c_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ wma_c_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ wma_c_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ wma_c_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ wma_c_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ sca_l_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ sca_l_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ sca_l_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ sca_l_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ sca_l_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ sca_l_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ sca_l_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ sca_l_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ sca_l_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ sca_l_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ gma_l_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ gma_l_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ gma_l_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ gma_l_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ gma_l_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ gma_l_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ gma_l_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ gma_l_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ gma_l_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ gma_l_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ wma_l_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ wma_l_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ wma_l_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ wma_l_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ wma_l_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ wma_l_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ wma_l_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ wma_l_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ wma_l_m00+ lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ wma_l_m00 + scim_m00 + age_m00,data=df));



summary(lm(mstot_m12 ~ la_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ la_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ la_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ la_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ la_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ la_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ la_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ la_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ la_m00 + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ la_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ ll_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ ll_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ ll_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ ll_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ ll_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ ll_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ ll_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ ll_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ ll_m00 + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ ll_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ lw_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ lw_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ lw_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ lw_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ lw_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ lw_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ lw_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ lw_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ lw_m00 + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ lw_m00 + scim_m00 + age_m00,data=df));

summary(lm(mstot_m12 ~ tb_total_m00 + mstot_m00 + age_m00,data=df)); summary(lm(uems_m12 ~ tb_total_m00 + uems_m00 + age_m00,data=df)); summary(lm(lems_m12 ~ tb_total_m00 + lems_m00 + age_m00,data=df));
summary(lm(lttot_m12 ~ tb_total_m00 + lttot_m00 + age_m00,data=df)); summary(lm(uelt_m12 ~ tb_total_m00 + uelt_m00 + age_m00,data=df)); summary(lm(lelt_m12 ~ tb_total_m00 + lelt_m00 + age_m00,data=df));
summary(lm(pptot_m12 ~ tb_total_m00 + pptot_m00 + age_m00,data=df)); summary(lm(uepp_m12 ~ tb_total_m00 + uepp_m00 + age_m00,data=df)); summary(lm(lepp_m12 ~ tb_total_m00 + lepp_m00 + age_m00,data=df));
summary(lm(scim_m12 ~ tb_total_m00 + scim_m00 + age_m00,data=df));


# correlations between lesion parameters and baseline MRI
summary(lm(sca_c_m00 ~ la_m00, data=df)); summary(lm(gma_c_m00 ~ la_m00, data=df)); summary(lm(wma_c_m00 ~ la_m00, data=df));
summary(lm(sca_l_m00 ~ la_m00, data=df)); summary(lm(gma_l_m00 ~ la_m00, data=df)); summary(lm(wma_l_m00 ~ la_m00, data=df));

summary(lm(sca_c_m00 ~ ll_m00, data=df)); summary(lm(gma_c_m00 ~ ll_m00, data=df)); summary(lm(wma_c_m00 ~ ll_m00, data=df));
summary(lm(sca_l_m00 ~ ll_m00, data=df)); summary(lm(gma_l_m00 ~ ll_m00, data=df)); summary(lm(wma_l_m00 ~ ll_m00, data=df));

summary(lm(sca_c_m00 ~ lw_m00, data=df)); summary(lm(gma_c_m00 ~ lw_m00, data=df)); summary(lm(wma_c_m00 ~ lw_m00, data=df));
summary(lm(sca_l_m00 ~ lw_m00, data=df)); summary(lm(gma_l_m00 ~ lw_m00, data=df)); summary(lm(wma_l_m00 ~ lw_m00, data=df));

summary(lm(sca_c_m00 ~ tb_total_m00, data=df)); summary(lm(gma_c_m00 ~ tb_total_m00, data=df)); summary(lm(wma_c_m00 ~ tb_total_m00, data=df));
summary(lm(sca_l_m00 ~ tb_total_m00, data=df)); summary(lm(gma_l_m00 ~ tb_total_m00, data=df)); summary(lm(wma_l_m00 ~ tb_total_m00, data=df));





# cervical, dorsal column, FA
print("Cervical, dorsal column, FA")
y <- as.numeric(unlist(df_dti_cervical[,41]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, dorsal column, AD
print("Cervical, dorsal column, AD")
y <- as.numeric(unlist(df_dti_cervical[,84]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, dorsal column, RD
print("Cervical, dorsal column, RD")
y <- as.numeric(unlist(df_dti_cervical[,127]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, lateral column, FA
print("Cervical, lateral column, FA")
y <- as.numeric(unlist(df_dti_cervical[,42]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, lateral column, AD
print("Cervical, lateral column, AD")
y <- as.numeric(unlist(df_dti_cervical[,85]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, lateral column, RD
print("Cervical, lateral column, RD")
y <- as.numeric(unlist(df_dti_cervical[,128]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, ventral column, FA
print("Cervical, ventral column, FA")
y <- as.numeric(unlist(df_dti_cervical[,43]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, ventral column, AD
print("Cervical, ventral column, AD")
y <- as.numeric(unlist(df_dti_cervical[,86]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# cervical, ventral column, RD
print("Cervical, ventral column, RD")
y <- as.numeric(unlist(df_dti_cervical[,129]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));



# lumbar, dorsal column, FA
print("Lumbar, dorsal column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,41]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, dorsal column, AD
print("Lumbar, dorsal column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,84]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, dorsal column, RD
print("Lumbar, dorsal column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,127]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, lateral column, FA
print("Lumbar, lateral column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,42]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, lateral column, AD
print("Lumbar, lateral column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,85]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, lateral column, RD
print("Lumbar, lateral column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,128]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, ventral column, FA
print("Lumbar, ventral column, FA")
y <- as.numeric(unlist(df_dti_lumbar[,43]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, ventral column, AD
print("Lumbar, ventral column, AD")
y <- as.numeric(unlist(df_dti_lumbar[,86]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# lumbar, ventral column, RD
print("Lumbar, ventral column, RD")
y <- as.numeric(unlist(df_dti_lumbar[,129]))
y <- y[df$months==2 & df$group==1]
summary(lm(y ~ la_m00)); summary(lm(y ~ ll_m00)); summary(lm(y ~ lw_m00)); summary(lm(y ~ tb_total_m00));

# correlations between lesion parameters and dMRI 1-year
summary(lm(delta12_sca_c ~ la_m00, data=df)); summary(lm(delta12_gma_c ~ la_m00, data=df)); summary(lm(delta12_wma_c ~ la_m00, data=df));
summary(lm(delta12_sca_l ~ la_m00, data=df)); summary(lm(delta12_gma_l ~ la_m00, data=df)); summary(lm(delta12_wma_l ~ la_m00, data=df));

summary(lm(delta12_sca_c ~ ll_m00, data=df)); summary(lm(delta12_gma_c ~ ll_m00, data=df)); summary(lm(delta12_wma_c ~ ll_m00, data=df));
summary(lm(delta12_sca_l ~ ll_m00, data=df)); summary(lm(delta12_gma_l ~ ll_m00, data=df)); summary(lm(delta12_wma_l ~ ll_m00, data=df));

summary(lm(delta12_sca_c ~ lw_m00, data=df)); summary(lm(delta12_gma_c ~ lw_m00, data=df)); summary(lm(delta12_wma_c ~ lw_m00, data=df));
summary(lm(delta12_sca_l ~ lw_m00, data=df)); summary(lm(delta12_gma_l ~ lw_m00, data=df)); summary(lm(delta12_wma_l ~ lw_m00, data=df));

summary(lm(delta12_sca_c ~ tb_total_m00, data=df)); summary(lm(delta12_gma_c ~ tb_total_m00, data=df)); summary(lm(delta12_wma_c ~ tb_total_m00, data=df));
summary(lm(delta12_sca_l ~ tb_total_m00, data=df)); summary(lm(delta12_gma_l ~ tb_total_m00, data=df)); summary(lm(delta12_wma_l ~ tb_total_m00, data=df));


# correlations between baseline electrophysiology and MRI
summary(lm(sca_c_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(sca_c_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(sca_c_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(gma_c_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(gma_c_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(gma_c_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(wma_c_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(wma_c_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(wma_c_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(dca_c_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(dca_c_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(dca_c_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(sca_l_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(sca_l_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(sca_l_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(gma_l_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(gma_l_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(gma_l_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(wma_l_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(wma_l_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(wma_l_m00 ~ mep_lat_hal_m00,data=df));
summary(lm(dca_l_m00 ~ ssep_lat_p40_tib_m00,data=df)); summary(lm(dca_l_m00 ~ mep_lat_tib_m00,data=df)); summary(lm(dca_l_m00 ~ mep_lat_hal_m00,data=df));
