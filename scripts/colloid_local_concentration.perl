#############################################################
# Calculate local concentration of colloid according to 
# its distance to top and bottom boundaries.
# This routine is different from clolloid_local_number_density,
# as the latter counts the discrete integer number of colloids.
# Here we consider also a fraction of a colloid.
#############################################################
use Math::Trig;

$dir_name= $ARGV[0];
$file_in_prefix="mcf_colloid";
$file_out_prefix= $ARGV[1];

$R=1.0;
$pi=3.1415926;

opendir(DIR, $dir_name) || die ("can not open directory");
print "processing directory : ", $dir_name, "\n";
@file_names_temp = readdir(DIR);
@file_names_in=sort @file_names_temp;
closedir(DIR);


$num_file  = 0;
$step_start=300000;
$step_start=$ARGV[2];
$step_end  =99999999;
$step_end  =$ARGV[3];

$Lx=64.0;
$Ly=64.0;
$Lx=$ARGV[4];
$Ly=$ARGV[5];

#############################################################
# number of different resolutions.
#############################################################
$num_res=1;

#############################################################
#the 0th resolution 
#############################################################

$res[0]=40;
$res[0]=$ARGV[6];

$h[0]=$Ly/$res[0];

print "number of resolution : ", $num_res, "\n";
print "reslutions : ", $res[0], " ";

#############################################################
# further resolution is two times than previous one.
# Note it is questionable to use higher resolution
# than SPH/SDPD resolution.
#############################################################
for ($j=1;$j<$num_res;$j++)
{
    $res[$j]=$res[$j-1]*2;
    $h[$j]=$Ly/$res[$j];
    print $res[$j], " ";
}
print "\n";


#############################################################
# initialize mid location y and aera counter of each layer.
#############################################################

for ($j=0;$j<$num_res;$j++)
{
    for ($k=0; $k<$res[$j]; $k++)
    {
	$y[$k][$j] = $k*$h[$j]+ $h[$j]/2.0;
	$area[$k][$j] = 0.0;
    }
}

#############################################################
# Loop over all files according to starting and ending files. 
#############################################################

$num_file=0;

print "starting step : ", $step_start, "\n";
print "ending  step  : ", $step_end, "\n";
 
$f_start = $file_in_prefix . stepstring($step_start). ".out";
$f_end = $file_in_prefix . stepstring($step_end). ".out";

print "starting file : ", $f_start, "\n";
print "ending file   : ", $f_end, "\n";

$num_particle_tot=0.0;

foreach $f (@file_names_in)
{
    if (( $f ge $f_start) && ( $f lt $f_end ) )
    {
	$file_name = $dir_name . $f;
	print "processing file : ", $file_name, "\n";
	
#############################################################
# reset to zero: colloid is assumed be a disk.
#
# y_c : y position of the center of the colloid.
# y_b : y position of the bottom surface
# y_t : y position of the top surface
# in  : index of y position in array $aera and $y
# in_c, in_b, in_t
#############################################################
	
	open (IN, $file_name);
	
	$num_particle=0;

	while ($line = <IN>)
	{
	    @data = split(' ', $line);
	    
	    $y_c = $data[1];
	    $y_b = $y_c - $R;
	    $y_t = $y_c + $R;
	    
###################################
# Loop each resolution h[j]
###################################
	    for ($j=0; $j<$num_res; $j++)
	    {
###################################
# calculate indices
###################################
		$in_c = int($y_c/$h[$j]);
		$in_b = int($y_b/$h[$j]);
		$in_t = int($y_t/$h[$j]);
###################################################
# Loop each index from bottom to the center of a colloid
# Outside the loop is the first circular segment.
###################################################
		$d = $y_c-($in_b+1)*$h[$j];
		$theta = 2.0*acos($d/$R);
		$area_segment = $R**2*($theta-sin($theta))/2.0;
		$area[$in_b][$j] += $area_segment;
		$area_segment_pre = $area_segment;
		for ($i=$in_b+2;$i<=$in_c;$i++)
		{
###################################################
# Calculate area of fraction in each index:
# which is the area of a circular segment minus
# the area of another circular segment,
# according to Wikipedia:
###################################################
		    $d = $y_c-$i*$h[$j];
		    $theta = 2.0*acos($d/$R);
		    $area_segment = $R**2*($theta-sin($theta))/2.0;
		    $area[$i-1][$j] += ($area_segment-$area_segment_pre);
		    $area_segment_pre = $area_segment;
		}
		$area_segment = $pi*$R**2/2.0;
		$area[$in_c][$j] += ($area_segment - $area_segment_pre);
		
###################################################
# Loop each index from top to the center of a colloid
# Note that the center index is added here the 
# second times, as the center fraction is divided
# into two pieces.
###################################################
		$d = $in_t*$h[$j]-$y_c;
		$theta = 2.0*acos($d/$R);
		$area_segment = $R**2*($theta-sin($theta))/2.0;
		$area[$in_t][$j] += $area_segment;
		$area_segment_pre = $area_segment;	
		for ($i=$in_t-1;$i>=$in_c+1;$i--)
		{
###################################################
# Calculate area of fraction in each index:
# which is the area of a circular segment minus
# the area of another circular segment,
# according to Wikipedia:
###################################################
		    $d=$i*$h[$j]-$y_c;
		    $theta=2.0*acos($d/$R);
		    $area_segment=$R**2*($theta-sin($theta))/2.0;
		    $area[$i][$j] += ($area_segment-$area_segment_pre);
		    $area_segment_pre = $area_segment;
		}
		$area_segment = $pi*$R**2/2.0;
		$area[$in_c][$j] += ($area_segment-$area_segment_pre);
	    }
	    
	    $num_particle++;	    
	}
	$num_file ++;
	$num_particle_tot += $num_particle;
	
	close(IN);
	
    }
    
}

print "number of files processed: ", $num_file, "\n";

if ($num_file > 0) 
{
    for ($j=0; $j<$num_res; $j++)
    {
	$file_out_name = ">".$file_out_prefix."_".$h[$j].".dat";
	open(OUT, $file_out_name);
	
	for ($k=0;$k<$res[$j];$k++)
	{
	    $area[$k][$j]=$area[$k][$j]/$num_file/($h[$j]*$Lx);
	    print OUT $y[$k][$j],' ', $area[$k][$j], "\n";
	}
	print "writing file : ", $file_out_name, "\n";
	close(OUT);    
    }
}


sub stepstring
{

    $step=$_[0];
    $length_step=length($step);

    $string = $step;
    for ($i=$length_step;$i<8;$i++)
    {
	$string = "0" . $string;
    }

    return $string;
}

