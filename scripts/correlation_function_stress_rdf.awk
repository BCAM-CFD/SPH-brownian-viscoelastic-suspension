#!/usr/bin/awk -f
{
    x[NR]=$3
    y[NR]=$4
    
    xm+=$3; ym+=$4

    xm2+=$3^2; ym2+=$4^2
    n++
}

END {
    xm/=n; ym/=n
    xm2/=n; ym2/=n

    sigmax = sqrt(xm2 - xm^2)
    sigmay = sqrt(ym2 - ym^2)
    for (dt=0; dt<n; dt++) {
	sum=q=0
	for (i=1; i<=n-dt; i++) {
	    q++
	    sum+= (x[i] - xm)*(y[i+dt] - ym)
	}
	print dt, sum/(sigmax*sigmay)/q
    }

}
