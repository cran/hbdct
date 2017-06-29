#This function plot the power finding results using the find.n results in the setted path

#find.n(del.cut=0.025, k=3, T=0.85, p.c=c(0.5, 0.4, 0.65,0.5), effect.size=0.2,alpha.target=0.1, power.target=0.9,tol.b1=0.02, tol.b2=0.02, tol.a=0.05, rep=500)
#plot.find.n(tol.b1=0.02,tol.b2=0.02,n.min=5,n.max=70,method='linear')

plot.find.n=function(tol.b1=0.02,tol.b2=0.02,n.min=5,n.max=70,pathout = getwd(),method='bisect'){
	data=as.matrix(read.table(file=paste('history_',method,'.txt',sep=''),skip=3));
	x=data[,1];y1=apply(data[,3:5],1,min);y2=apply(data[,3:5],1,max);
	#draw lines
	#plot(y=y1,x=x,type='l',ylim=c(0,1),xlim=c(n.min-2,n.max+2),xlab='n',ylab='power');
	plot(y=y1,x=x,type='l',ylim=c(0,1),xlim=c(min(x)-2,max(x)+2),xlab='n',ylab='power');

	lines(y=y2,x=x,col='green');
	#draw points
	y=(y1+y2)/2;
	points(y=y,x=x,pch=21,col='blue',bg='blue');
	points(y=y[length(x)],x=x[length(x)],pch=21,col='red',bg='red');
	abline(v=min(x),lty=2);abline(v=max(x),lty=2);
	#draw CI lines
	abline(h=y[length(y)],lty=2);abline(h=y[length(y)]-tol.b1,lty=3);abline(h=y[length(y)]+tol.b2,lty=3);
	#draw legend box
	legend(y=0.1,x=(x[1]+x[length(x)])/2,legend=c('max(power)','min(power'),col=c('green','black'),lty=1);
	if (method=='linear'){
		#point annotation
		this_text=NULL;
		for (i in 1:length(x)){this_text=c(this_text,paste('power(n',i-1,')',sep=''))};
		this_text[length(this_text)]='final.sample.size';
		text(y=y1-0.05,x=x+1,label=this_text)
		#first and last annotation
		text(y=0.1,x=min(x),label=paste('n.min=',min(x),sep=''));
		text(y=0.1,x=max(x),label=paste('n.max=',max(x),sep=''));
	}else{
		#point annotation
		this_text=NULL;
		for (i in 1:((length(x)-1)/2)){
			this_text=c(this_text,paste('power(a',i,')',sep=''))
			this_text=c(this_text,paste('power(b',i,')',sep=''))
			};
		this_text[length(this_text)]='final.sample.size';
		text(y=y1-0.05,x=x+1,label=this_text)
		#first and last annotation
		text(y=0.1,x=min(x),label=paste('a1=',min(x),sep=''));
		text(y=0.1,x=max(x),label=paste('b1=',max(x),sep=''));
	}
}
