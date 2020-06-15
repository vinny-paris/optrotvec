d4 <- read.csv('factor4_sim', header = TRUE)
(1:1000)*3 -> min_ind
d4min <- d4[min_ind,]
head(d4min)
n4_det_ratio <- d4min$Det.Ratio
n4_min <- d4min$No..of.Runs
n4_omega <- d4min$Omega.value
plot(n4_omega, n4_min)
plot(n4_min, n4_omega, jitter = TRUE)
n4_eff <- d4min$Omega.value/(4*4*3)
min4 <- data.frame(n4_min, n4_omega, n4_eff, n4_det_ratio)

library(ggplot2)
ggplot(min4, aes(n4_min, n4_omega/48)) + geom_jitter(aes(color = n4_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Minimum Run Counts', subtitle = "4 Factor Designs", color = "Det. Ratio")
ggplot(min4, aes(n4_min, n4_omega/48)) + geom_jitter(aes(color = n4_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Minimum Run Counts', subtitle = "4 Factor Designs", color = "Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')


(1:1000)*3 - 1 -> opt_ind                                                 
d4_opt <- d4[opt_ind,]
o4_det_ratio <- d4_opt$Det.Ratio
o4_min <- d4_opt$No..of.Runs
o4_omega <- d4_opt$Omega.value
o4_eff <- d4_opt$Omega.value/(4*4*3)
opt4 <- data.frame(o4_det_ratio, o4_eff, o4_min, o4_omega)
ggplot(opt4, aes(o4_min, o4_omega/48)) + geom_jitter(aes(color = o4_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "4 Factor Designs", color = "Det. Ratio")
ggplot(opt4, aes(o4_min, o4_omega/48)) + geom_jitter(aes(color = (o4_det_ratio > 1))) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "4 Factor Designs", color = "Det. Ratio > 1")
ggplot(opt4, aes(o4_min, o4_omega/48)) + geom_jitter(aes(color = o4_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "4 Factor Designs", color = "Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')


(1:1000)*3 - 2 -> max_ind                                                 
d4_max <- d4[max_ind,]
m4_det_ratio <- d4_max$Det.Ratio
m4_min <- d4_max$No..of.Runs
m4_omega <- d4_max$Omega.value
m4_eff <- d4_max$Omega.value/(4*4*3)
max4 <- data.frame(m4_det_ratio, m4_eff, m4_min, m4_omega)
ggplot(max4, aes(m4_det_ratio, o4_omega/48)) + geom_jitter(width = 0) + xlab("Det. Ratio") + ylab("Omega Eff.") + labs(title = "Largest Det. Designs", subtitle = "4 Factor Designs")



d5 <- read.csv('factor5_sim.csv', header = TRUE)
(1:1000)*3 -> min_ind
d5min <- d5[min_ind,]
head(d5min)
n5_det_ratio <- d5min$Det.Ratio
n5_min <- d5min$No..of.Runs
n5_omega <- d5min$Omega.value
plot(n5_omega, n5_min)
n5_eff <- d5min$Omega.value/(4*5*5)
min5 <- data.frame(n5_min, n5_omega, n5_eff, n5_det_ratio)
ggplot(min5, aes(n5_min, n5_omega/100)) + geom_jitter(aes(color = n5_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Minimum Run Counts', subtitle = "5 Factor Designs", color = "Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')

jack <- (d5_opt$Omega.value - d5min$Omega.value)/100
jill <- d5_opt$No..of.Runs - d5min$No..of.Runs
fucked <- d5_opt$Det.Ratio - d5min$Det.Ratio
hill <- data.frame(jack, jill, fucked)
ggplot(hill, aes(jill, jack)) + geom_jitter(aes(color = fucked)) + xlab('Difference in Run Count') + ylab('Difference in Omega Eff.') + labs(title  = 'Opt. Design vs. Min. Run Design', subtitle = "5 Factor Designs", color = "Diff. Det. Ratio")+ scale_color_gradient(low = 'purple', high = 'green')
(hill[which(fucked == 0 & jack == 0),])
table(hill$jack)
hill[116,]


jack4 <- (d4_opt$Omega.value - d4min$Omega.value)/48
jill4 <- d4_opt$No..of.Runs - d4min$No..of.Runs
fucked4 <- d4_opt$Det.Ratio - d4min$Det.Ratio
hill4 <- data.frame(jack4, jill4, fucked4)
ggplot(hill4, aes(jill4, jack4)) + geom_jitter(aes(color = fucked4)) + xlab('Difference in Run Count') + ylab('Difference in Omega Eff.') + labs(title  = 'Opt. Design vs. Min. Run Design', subtitle = "4 Factor Designs", color = "Diff. Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')
(hill[which(fucked4 == 0 & jack4 == 0),])
table(hill$jack)
hill[116,]


(1:1000)*3 - 1 -> opt_ind                                                 
d5_opt <- d5[opt_ind,]
o5_det_ratio <- d5_opt$Det.Ratio
o5_min <- d5_opt$No..of.Runs
o5_omega <- d5_opt$Omega.value
o5_eff <- d5_opt$Omega.value/(100)
opt5 <- data.frame(o5_det_ratio, o5_eff, o5_min, o5_omega)
ggplot(opt5, aes(o5_min, o5_omega/100)) + geom_jitter(aes(color = o5_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "5 Factor Designs", color = "Det. Ratio")
ggplot(opt5, aes(o5_min, o5_omega/100)) + geom_jitter(aes(color = (o5_det_ratio > 1))) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "5 Factor Designs", color = "Det. Ratio > 1")
ggplot(opt5, aes(o5_min, o5_omega/100)) + geom_jitter(aes(color = o5_det_ratio)) + ylab('Omega Eff.') + xlab('Run Count') + labs(title  = 'Optimal Designs', subtitle = "5 Factor Designs", color = "Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')




(1:1000)*3 - 2 -> max_ind                                                 
d5_max <- d5[max_ind,]
m5_det_ratio <- d5_max$Det.Ratio
m5_min <- d5_max$No..of.Runs
m5_omega <- d5_max$Omega.value
m5_eff <- d5_max$Omega.value/(100)
max5 <- data.frame(m5_det_ratio, m5_eff, m5_min, m5_omega)
ggplot(max5, aes(m5_det_ratio, o5_omega/100)) + geom_jitter(width = 0) + xlab("Det. Ratio") + ylab("Omega Eff.") + labs(title = "Largest Det. Designs", subtitle = '5 Factor Designs')

jackm <- (d5_opt$Omega.value - d5_max$Omega.value)/100
jillm <- d5_opt$No..of.Runs - d5_max$No..of.Runs
fuckedm <- d5_opt$Det.Ratio - d5_max$Det.Ratio
hillm <- data.frame(jackm, jillm, fuckedm)
ggplot(hillm, aes(jillm, jackm)) + geom_jitter(aes(color = fuckedm)) + xlab('Difference in Run Count') + ylab('Difference in Omega Eff.') + labs(title  = 'Opt. vs. Max. Det. Designs', subtitle = "5 Factor Designs", color = "Diff. Det. Ratio")+ scale_color_gradient(low = 'purple', high = 'green')
(hill[which(fucked == 0 & jack == 0),])
table(hill$jack)
hill[116,]


jack4m <- (d4_opt$Omega.value - d4_max$Omega.value)/48
jill4m <- d4_opt$No..of.Runs - d4_max$No..of.Runs
fucked4m <- d4_opt$Det.Ratio - d4_max$Det.Ratio
hill4m <- data.frame(jack4m, jill4m, fucked4m)
ggplot(hill4m, aes(jill4m, jack4m)) + geom_jitter(aes(color = fucked4m)) + xlab('Difference in Run Count') + ylab('Difference in Omega Eff.') + labs(title  = 'Opt. vs. Min. Run Designs', subtitle = "4 Factor Designs", color = "Diff. Det. Ratio") + scale_color_gradient(low = 'purple', high = 'green')
(hill[which(fucked4 == 0 & jack4 == 0),])
table(hill$jack)
hill[116,]




d50 <- subset(d5, Run.Reduced. == 0)[,-1]
d50_rv <- (d50[,6:10])
d51 <- subset(d5, Run.Reduced. == 1)[,-1]
d51_rv <- (d51[,6:10])
jjj <- merge(d50, d51, by = paste(c('Rotation.Vectors', 'V7', 'V8', 'V9', 'V10'), sep = "_"))
head(jjj)

head(d50)

