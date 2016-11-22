
library('VGAM')

d2 = rexp(100, 1/300);
d3 = rpareto(100, 10, 10);


modelExp = function(data){
  
  n = length(data);
  dtau = (max(data)-min(data))/10000;
  tauvals = seq(from = min(data),to = max(data), by = dtau);
  sumdata = sum(data);
  prob = sum(dtau*(log(tauvals^-n)+(-sumdata/tauvals)));
  return(prob)
  
}

pme = modelExp(d2);

modelPareto = function(data){

  sumLogData = sum(log(data));
  xHat = min(data);
  alphaHat = length(data)/sum(log(data)-log(xHat));
  n = length(data);
  dx = (max(data)-min(data))/1000;
  dAlpha = (alphaHat/100);
  

  alphaVals = seq(0.5,alphaHat*5,dAlpha);
  xVals = seq(min(data),max(data),dx);
  
  l = sum((length(xVals)*n*log(alphaVals) + n*alphaVals*sum(log(xVals)) - length(xVals)*(alphaVals - 1)*sumLogData)*dx*dAlpha);
  
  #l= sum(l[l != (-Inf)]);
  return(l)
}
#underflowing! You're doing it fucking wrong!
pmp = modelPareto(d2);

Pm2 = pmp/pme;

