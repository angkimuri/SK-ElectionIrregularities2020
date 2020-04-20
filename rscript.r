setwd("C:/Users/user/Dropbox/ProduceKR")
library(eforensics)
set.seed(12345)
mcmc = list(burn.in=2000, n.adapt=2000, n.iter=2000, n.chains=10)

fnames <- c('Jongno2020','Jongno2016',
            'JungguSeongdongguA2020','JungguSeongdongguA2016',
            'JungguSeongdongguB2020','JungguSeongdongguB2016',
            'Yongsan2020','Yongsan2016',
            'GwangjinA2020','GwangjinA2016',
            'GwangjinB2020','GwangjinB2016',
            'SongpaA2020','SongpaA2016',
            'SongpaB2020','SongpaB2016',
            'SongpaC2020','SongpaC2016',
            'GoyangsiA2020','GoyangsiA2016',
            'YangcheonguA2020','YangcheonguA2016',
            'YangcheonguB2020','YangcheonguB2016',
            'Yeojusi2020','Yeojusi2016',
            'YonginsiC2020','YonginsiC2016',
            'YonginsiD2020','YonginsiD2016',
            'GwanakA2020','GwanakA2016')

for(fname in fnames) {

	data <-read.csv(file=paste('Data/',fname,'.csv',sep=''))

	samples = eforensics(
		n.w ~ 1,
		n.a ~ 1,
		mu.iota.m ~ 1,
		mu.iota.s ~ 1,
		mu.chi.m  ~ 1,
		mu.chi.s  ~ 1,
		data=data,
		eligible.voters="n.rv",
		model="qbl",
		mcmc=mcmc,
		parameters = "all",
		parComp = TRUE,
		autoConv = TRUE,
		max.auto = 0,
		mcmc.conv.diagnostic = "PSRF",
		mcmc.conv.parameters = "pi",
	)

	coefs <- summary(samples, join.chains=T)

	num_z <- samples[[1]]$piZi*100
	for (i in 2:mcmc$n.chains) {
		num_z <- num_z + samples[[i]]$piZi*100
	}
	num_z <- num_z/mcmc$n.chains
	max_z <- apply(num_z,1,which.max)
	
	write.csv(coefs,paste('Outputs/',fname,'-coefs.csv',sep=''))
	write.csv(num_z,paste('Outputs/',fname,'-Zvalues.csv',sep=''))
	write.csv(attr(samples,"frauds"),paste('Outputs/',fname,'-frauds.csv',sep=''))
	
}



