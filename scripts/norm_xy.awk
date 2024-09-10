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
    for (i=1; i<=n; i++) {
	print (x[i]-xm)/sigmax, (y[i]-ym)/sigmay
    }

}
