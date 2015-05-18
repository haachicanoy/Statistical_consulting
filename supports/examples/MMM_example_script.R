# ---------------------------------------------------------------------------- #
# Multivariate mixed models (only in R 3.1.3)
# Example
# ---------------------------------------------------------------------------- #

library(spida)
library(p3d)
library(nlme)  
data(hs)
head(hs)
hs.w <- hs

names(hs.w) <- sub('^mathach$', 'y.mathach', names(hs.w))
names(hs.w) <- sub('^ses$',     'y.ses',     names(hs.w))
head(hs.w)
dd <- reshape( hs.w, direction = 'long', varying = 2:3, timevar = 'var')
dd$var <- as.factor(dd$var)
dd$v <- as.numeric(dd$var)
dim(hs); dim(dd)
head(dd)

# A one-level multivariate model

fit.onelevel <- gls( y ~ var/(Sex + Sector) -1,  dd, 
                     correlation =  corSymm( form = ~ v |id), # v must be integer
                     weights = varIdent(form = ~ 1 | v)) 
summary(fit.onelevel)
getVarCov(fit.onelevel)

fit.math <- lm( mathach ~ Sex + Sector, hs)
summary(fit.math)
summary(fit.math)$sigma^2

fit.ses <- lm( ses ~ Sex + Sector, hs)
summary(fit.ses)
summary(fit.ses)$sigma^2

# Multilevel Multivariate Model

fit.multilevel <- lme( y ~ var/(Sex * Sector) -1,  dd,
                       random = ~ var -1| school,   
                       correlation =  corSymm( form = ~ v |school/id), 
                       # v must be integer and school 
                       # must be included
                       # even though it's not necessary 
                       # since 'id' is unique
                       # across schools
                       weights = varIdent(form = ~ 1 | v))

summary(fit.multilevel)
getVarCov(fit.multilevel)
VarCorr(fit.multilevel)
plot(fit.multilevel)

intervals(fit.multilevel)

wald(fit.multilevel, ":.*:")  # testing for Sex by Sector interaction

# MCMCglmm

library(MCMCglmm)

# random intercept model
fit.w.null <- MCMCglmm( cbind(mathach, ses) ~ trait -1 ,
                        random = ~us(trait):school,   
                        # 'trait' is reserved keyword to index dependent variables
                        # unstructured covariance matrix
                        # equivalent to random = ~ trait -1 | school
                        # 
                        rcov = ~us(trait):units,
                        # Unstructured covariance matrix. 
                        #  'units' is reserved
                        # to refer to individual observations
                        data = hs,
                        family = c("gaussian","gaussian"))
# uses default prior  
windows()
plot(fit.w.null)  
summary(fit.w.null)


# full model
fit.w.full <- MCMCglmm( cbind(mathach, ses) ~ trait/(Sex * Sector) -1 ,
                        random = ~us(trait):school,   
                        # 'trait' is reserved keyword to index dependent variables
                        # unstructured covariance matrix
                        # equivalent to random = ~ trait -1 | school
                        # 
                        rcov = ~us(trait):units,
                        # Unstructured covariance matrix. 'units' is reserved
                        # to refer to individual observations
                        data = hs,
                        family = c("gaussian","gaussian"))
# uses default prior  
plot(fit.w.full)  
summary(fit.w.full)

str(fit.w.full)
fit.w.full$VCV
fit.w.full$Z
fit.w.full$X
fit.w.full$DIC
fit.w.null$DIC - fit.w.full$DIC


plot(fit.w.full$Sol)  # fixed effects
plot(fit.w.full$VCV)  # G and R

effectiveSize(fit.w.full$VCV)
HPDinterval(fit.w.full$VCV)

# Estimating the regression coefficient

effectiveSize(fit.w.full$VCV[,6]/fit.w.full$VCV[,8])
plot(fit.w.full$VCV[,6]/fit.w.full$VCV[,8])
HPDinterval(fit.w.full$VCV[,6]/fit.w.full$VCV[,8])

# Interpreting changes in DIC

fit.w.full$DIC
fit.w.null$DIC

fit.w.null$DIC - fit.w.full$DIC
cbind( 0:10,1/(1+exp(0:10)))
cbind( "DIC drop"=0:10,
       "Appx posterior prob or null model" = round(1/(1+exp(0:10)),5))

# ---------------------------------------------------------------------------- #
# Another example with lme4
# ---------------------------------------------------------------------------- #

library(lme4)

Data <- read.csv('example_data.csv')

# Univariate mixed model

lmer.m1 <- lmer(Y1~A*B+(1|Block)+(1|Block:A), data=Data)
summary(lmer.m1)
anova(lmer.m1)

lmer.m2 <- lmer(Y2~A*B+(1|Block)+(1|Block:A), data=Data)
summary(lmer.m2)
anova(lmer.m2)

# Multivarite mixed model

library(reshape)

Data = melt(Data, id.vars=1:3, variable_name='Y')
Data$Y = factor(gsub('Y(.+)', '\\1', Data$Y))

lmer.m3 <- lmer(value~Y+A*B+(1|Block)+(1|Block:A), data=Data)

# Deviance function
# This deviance can be regarded as a measure of the lack of fit between the model and the data.
par(mar=c(4,4,2,2)+.1)
curve(-2*log(x), 0, 1, xlab='x: Likelihood')




