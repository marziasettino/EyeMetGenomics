dev.off()

graphics.off()

dev.new()

par(mfrow = c(4,2))
par(mar = c(5,5,5,5))

par(fig=c(0.1,0.6,0.5,0.8), new=TRUE)  

par(mar = c(1, 1, 1, 1))

par(mar = c(0.1, 0.1, 0.1, 0.1))

par(mar = rep(2, 4))

dev.new(width=30, height=20, unit="in")#OK!
par(mar=c(3.5, 3.5, 2, 1), mgp=c(2.4, 0.8, 0))


dev.new(width=50, height=40, unit="in")#OK!
dev.new(width=60, height=50, unit="in")#OK!
dev.new(width=70, height=60, unit="in")#OK!
