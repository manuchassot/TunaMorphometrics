
# FORK LENGTH - ROUND WEIGHT: BET
# LOG-NORMAL ERROR

LM_FL_RW_BET_IO = lm(log(whole_fish_weight) ~ log(fork_length), data = FULL_DATASET[species_code_fao == "BET" & ocean_code == "IO" & !is.na(fork_length) & !is.na(whole_fish_weight)])

aalbwtflio <- exp(coefficients(lmwtflalbio)[1]) * exp(var(residuals(lmwtflalbio))/2)

balbwtflio <- coefficients(lmwtflalbio)[2]

fl.to.predict <- seq(40,120,0.01)

lines(fl.to.predict,aalbwtflio*fl.to.predict^balbwtflio,col="red",lwd=1.5)

# ### Regression model LF-WT with nls
# lwalb.regio <- nls(whole_fishweight~ a * fork_length^b,data=alb.io,start=list(a=0.0000569070,b=2.75140))
# FL <- data.frame(fork_length=seq(30,160,1))
# #lines(FL[,1],predict(lwalb.reg,newdata=FL),col="black",lwd=1,lty=2)    
