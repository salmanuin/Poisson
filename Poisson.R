> library(MASS)
> library(car)
> library(pscl)
> library(ggplot2)
> library(boot)
> library(AER)
> library(fitdistrplus)
> library(performance)
> library(randomForest)
> library(VGAM)
> library(DT) 
> library(car)
> data =read.delim("clipboard")
> str(data)
'data.frame':	38 obs. of  9 variables:
 $ Y : int  3 2 2 4 3 4 9 1 7 13 ...
 $ X1: num  414 680 592 966 711 ...
 $ X2: num  72.6 86.8 79.3 83.6 80.1 ...
 $ X3: num  67 82.6 61.1 67.6 77.6 ...
 $ X4: int  24 31 22 32 24 37 39 25 50 45 ...
 $ X5: int  86 81 69 80 51 78 60 113 191 73 ...
 $ X6: int  49 58 57 55 39 82 72 35 62 45 ...
 $ X7: num  59.6 47.7 55.5 49.9 53.1 ...
 $ X8: num  52.9 35.5 55.1 31.7 43 ...
> model = lm(Y~., data = data)
> vif(model)
       X1        X2        X3        X4        X5        X6        X7        X8 
 2.076124  2.185513  1.459299  2.611982  6.946034  7.777406 22.746175 24.518955 
> 
> #Regresi Poisson
> m<- glm(Y~X1+X2+X3+X4+X5+X6, family="poisson", data=data)
> summary(m)

Call:
glm(formula = Y ~ X1 + X2 + X3 + X4 + X5 + X6, family = "poisson", 
    data = data)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.0363278  0.7097980   4.278 1.89e-05 ***
X1           0.0003302  0.0002744   1.203  0.22882    
X2          -0.0157032  0.0078101  -2.011  0.04436 *  
X3          -0.0112659  0.0087234  -1.291  0.19654    
X4           0.0088234  0.0100709   0.876  0.38096    
X5          -0.0085299  0.0029605  -2.881  0.00396 ** 
X6           0.0144623  0.0050379   2.871  0.00410 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 125.837  on 37  degrees of freedom
Residual deviance:  99.257  on 31  degrees of freedom
AIC: 217.61

Number of Fisher Scoring iterations: 5

> check_overdispersion(m)
# Overdispersion test

       dispersion ratio =   3.097
  Pearson's Chi-Squared =  95.998
                p-value = < 0.001

Overdispersion detected.
> 
> #Regresi binomial negatif
> m2<- glm.nb(Y~X1+X2+X3+X4+X5+X6, data=data)
> summary(m2)

Call:
glm.nb(formula = Y ~ X1 + X2 + X3 + X4 + X5 + X6, data = data, 
    init.theta = 2.49761942, link = log)

Coefficients:
              Estimate Std. Error z value Pr(>|z|)   
(Intercept)  3.4869548  1.2235907   2.850  0.00438 **
X1           0.0003636  0.0004538   0.801  0.42311   
X2          -0.0211869  0.0133056  -1.592  0.11131   
X3          -0.0101257  0.0137516  -0.736  0.46153   
X4           0.0050402  0.0163732   0.308  0.75821   
X5          -0.0080181  0.0048001  -1.670  0.09484 . 
X6           0.0135199  0.0082603   1.637  0.10169   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Negative Binomial(2.4976) family taken to be 1)

    Null deviance: 53.169  on 37  degrees of freedom
Residual deviance: 42.400  on 31  degrees of freedom
AIC: 195.23

Number of Fisher Scoring iterations: 1


              Theta:  2.498 
          Std. Err.:  0.990 

 2 x log-likelihood:  -179.232 
> 
> #Membandingkan model
> model_performance(m,metrics = "all")
# Indices of model performance

AIC     |    AICc |     BIC | Nagelkerke's R2 |  RMSE | Sigma | Score_log | Score_spherical
-------------------------------------------------------------------------------------------
217.605 | 221.338 | 229.068 |           0.522 | 3.416 | 1.000 |    -2.679 |           0.137
> model_performance(m2,metrics = "all")
# Indices of model performance

AIC     |    AICc |     BIC | Nagelkerke's R2 |  RMSE | Sigma | Score_log | Score_spherical
-------------------------------------------------------------------------------------------
195.232 | 200.198 | 208.333 |           0.328 | 3.504 | 1.000 |    -2.363 |           0.144
