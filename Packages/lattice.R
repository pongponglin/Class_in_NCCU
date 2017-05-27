###### lattice#####

library("lattice")
library("dplyr")

head(Puromycin)
plot(rate ~ conc,
     data = subset(Puromycin, state == "untreated"), type = "b", col = "red",
     xlim = c(0, 1.1), ylim = c(45, 210))
par(new = TRUE) 
plot(rate ~ conc,data = subset(Puromycin, state == "treated"), type = "b",
     col = "green", xlim = c(0, 1.1), ylim = c(45, 210))
legend("bottomright",
        legend = c("Untreated", "Treated"), col = c("red", "green"),
        lty = c(1, 1), pch = c(1,1))

xyplot(rate ~ conc,data = Puromycin, groups = state, auto.key = TRUE)
xyplot(rate ~ conc,data = Puromycin,
       groups = state, type = "b",auto.key = list(columns =2 ))
xyplot(rate ~ conc | state, data = Puromycin, type = "b")
dotplot(rate ~ conc | state, data = Puromycin)
stripplot(rate ~ conc | state, data = Puromycin)  
     
library("mangoTraining")
head(tubeData)
histogram( ~ Excess, data = tubeData, col = "red",
           main = "Histogram of Excess Time",
           xlab = "Excess Journey Times (Hours)",
           nint = 25)
histogram( ~ Excess | Type, data = tubeData ,col = "red",
           main = "Histogram of Excess Time",
           xlab = "Excess Journey Times (Hours)",
           nint = 25)

dotplot(Line ~ Length , data = tubeData)
dotplot(reorder(Line, Length) ~ Length , data = tubeData)

xyplot(Excess ~ Month , data = tubeData, 
       type = c("p", "smooth"))
       
xyplot(Excess ~ Month , data = tubeData, groups = Line,
      auto.key = list(space="right"),
       main = "Excess Journey Times vs Month by Line",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")
xyplot(Excess ~ Month | Line, data = tubeData, col = "blue",layout=c(5,2),
       main = "Excess Journey Times vs Month by Line",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")

xyplot(Excess ~ Month , data = tubeData, groups = Line,type="l",
       auto.key = list(space="right"),
       main = "Excess Journey Times vs Month by Line",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")
xyplot(Excess ~ Month | Line, data = tubeData, col = "blue",type = "l",layout=c(5,2),
       main = "Excess Journey Times vs Month by Line",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")
xyplot(Excess ~ Month , data = tubeData, subset = Line =="Central",
       col = "blue",type = "l",
       main = "Excess Journey Times vs Month of Central",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")
xyplot(Excess ~ Month | cut(Length, c(0, 20, 50, 100)), data = tubeData,
       col = "blue", layout = c(3, 1),
       main = "Excess Journey Times vs Month by Length (km)",
       xlab = "The Month of The Observation",
       ylab = "Excess Journey Times (Hours)")

bwplot( ~Excess, data = tubeData)
bwplot(Line ~ Excess  , data = tubeData,
       main = "Excess Journey Times by Line",
       xlab = "Excess Journey Times (Hours)")

bwplot(~ Excess | cut(Length, c(0, 20, 50, 100)) , data = tubeData, layout = c(1, 3),
       main = "Excess Journey Times by Length (km)",
       xlab = "Excess Journey Times (Hours)")

bwplot(Type ~ Excess | cut(Length, c(0, 20, 50, 100)) , data = tubeData,layout = c(1, 3),
       main = "Excess Journey Times by Length (km) & Type",
       xlab = "Excess Journey Times (Hours)")


densityplot(~ Excess, data = tubeData,lwd = 2, 
            main = "Density of Excess Time",
            xlab = "Excess Journey Times (Hours)") 

densityplot( ~ Excess,
             data = tubeData,
             groups = Line,
             type = "n",
             auto.key = list(space = "right"), lwd = 1,
             main = "Density of Excess Time Separated Plots by Lines", 
             xlab = "Excess Journey Times (Hours)")

qqmath( ~ Excess, data = tubeData, main = "QQplot of Excess Time")
qqmath( ~ Excess, data = tubeData, groups = Type,
        auto.key = list("top", columns=2),
        main = "QQplot of Excess Time by Type")

cloud(Excess ~  Length * Month, data = tubeData)

splom(~tubeData[2:5])
splom(~tubeData[2:5], groups = Type, data = tubeData,
      auto.key = list( "top", columns=2))

show.settings()

barchart( ~ Line, data = tubeData, groups = Type, auto.key = TRUE)

library("MASS")
head(housing)
barchart(Freq ~ Infl | Type + Cont, groups = Sat,
         data = housing, stack = TRUE)
barchart(xtabs(Freq ~ Sat + Infl + Type + Cont, data = housing))
dotplot(Sat+Infl ~ Freq | Type+Cont, data = housing , auto.key = TRUE)


dim(volcano)
head(volcano)

