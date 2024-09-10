#############################################################
#############################################################
#############################################################
# Calculate size distribution of colloid-clusters by positions.
# 
# Algorithm of recursive search.
#
# There are three types of output files:
#          1:) output colloid with position and cluster ID to
#              new colloid files in a new sub-folder,
#              which is useful for animation.
#          2:) output size distribution of colloid-clusters
#              at any step instant.
#          3:) output size distribution of colloid-clusters
#              averaged by overall time. 
#############################################################
#############################################################
#############################################################

use Math::Trig;


#############################################################
#############################################################
# Read in parameters
#############################################################
#############################################################

#############################################################
# dir_name                  :
#                 directory name with colloid files inside
# file_in_name_prefix       :
#                 name prefix of files.
# file_out_name_probability :
#                 output file name of time averaged probability.
#############################################################

$dir_name=$ARGV[0];
$file_in_name_prefix="mcf_colloid";
$file_out_name_probability=$ARGV[1];
print "processing directory : ", $dir_name, "\n";

#############################################################
#freq_output_ID: 
#                how frequent to output colloids with cluster ID
#                0: no output at all.
#############################################################

$freq_ID_output = 1;
$subdir_name_ID_output="cluster_position_ID_recursive_stepwise_output/";

#############################################################
#freq_probability_output:
#                 how frequent to output probability of
#                 cluster-size at certain time step.
#                0: no output at all.
#############################################################

$freq_probability_output = 1;
$subdir_name_probability_output="cluster_position_probability_recursive_stepwise_output/";

$pi=3.1415926;
$num_file  = 0;
$step_start=80000;
$step_start=$ARGV[2];
$step_end  =99999999;
$step_end=$ARGV[3];

#############################################################
# Lx,Ly: box size.
# V    : area 2D.
# N    : number of colloids, 224 is default for C=68.7% in [32,32].
# gap  : gap considered away from the walls.
# R    : R=1 by default.
# suppose x-direction periodic and y-direction wall.
#############################################################

$Lx    = $ARGV[4];
$Ly    = $ARGV[5];
$V     = $Lx*$Ly;
$N     = 224;
$N     = $ARGV[6];
$gap   = $ARGV[7];
$R     = 1.0;
$V_eff = $Lx*($Ly-2.0*($gap-$R));
print "Lx, Ly, N, gap, R: ", $Lx, ', ', $Ly, ', ', $N, ', ', $gap, ', ', $R, "\n";

#############################################################
# Surface distance threshold for defining cluster.
#
# sur_dist_threshold: 0.05 as default, repulsive force range.
#############################################################

$sur_dist_threshold = 0.05; 
$sur_dist_threshold = $ARGV[8];
print "surface distance threshold : ", $sur_dist_threshold, "\n";


#############################################################
#############################################################
# Start reading files and do processing.
#############################################################
#############################################################


#############################################################
# Open the directory.
#############################################################

opendir(DIR, $dir_name) || 
die   ("can not open directory    : " . $dir_name . ", as it does not exist !\n");
print "processing directory       : ", $dir_name, "\n";

#############################################################
# Sorting the files by names and count the total number.
#############################################################

@file_names_temp = readdir(DIR);
@file_names_in=sort @file_names_temp;
closedir(DIR);
print "number of files inside    : ", scalar(@file_names_in), "\n";


#############################################################
# for recording the averaged number of different sized clusters.
#
# $num_cluster[$i] stores how many clusters with size $i.
# 
#############################################################

for ( $i=1; $i<=$N; $i++ )
{
    $num_cluster[$i]=0.0;
}

#############################################################
# reset two dimensional matrix to indicate near-by relation.
# 1: related
# 0: unrelated.
#############################################################

for ( $i=1; $i<=$N; $i++ )
{
    for ( $j=1; $j<=$N; $j++ )
    {
	$near_by[$i][$j] = 0;
    }
}

#############################################################
#num_p: number of colloids in the file.
#num_o: number of colloids considered,  others are exclued,
#       such as the ones near walls.
#num_o_average: averaged number of colloids considered 
#               over time.
#############################################################

$num_p=0;
$num_o=0;
$num_o_average=0;

$num_pair_average=0;
#############################################################
# Loop over all files according to starting and ending files.
#############################################################

$num_file=0;

$f_start = $file_in_name_prefix . stepstring($step_start). ".out";
$f_end   = $file_in_name_prefix . stepstring($step_end). ".out";
print "starting file   : ", $f_start, "\n";
print "ending file     : ", $f_end, "\n";

foreach $f (@file_names_in)
{
    #print "checking file : ", $f, "\n";
    
    if (( $f ge $f_start) && ( $f le $f_end ) )
    {
	$file_name = $dir_name . $f;
	print "processing file : ", $file_name, "\n";
	
#############################################################
# Open a colloid file.
# reset counter $num_p to zero. 
# read each colloid position.
#############################################################
	
	open (IN, $file_name);	
	
	$num_p=0;
	
	while ($line = <IN>)
	{
	    @data = split(' ', $line);
	    
	    $num_p++;
	    $x[$num_p] = $data[0];
	    $y[$num_p] = $data[1];
	    
	}
	close(IN);
	#print "num_p : ", $num_p, "\n";
	
#############################################################
# get number denisty.
#############################################################

	$num_density   = $num_p/$V;
	$num_density_r = $V/$num_p;
	#print "num density : ", $num_density, "\n";
	
#############################################################
# reset counter $num_o to zero. 
# exclude the ones near the walls and
# record the ones, which are used for computation.
#############################################################

	$num_o=0;

	for ( $p=1; $p<=$num_p; $p++ )
	{
	    if ( ( $y[$p]>$gap ) && ( $y[$p]< $Ly-$gap) )
	    {
		$num_o++;
		$x_o[$num_o] = $x[$p];
		$y_o[$num_o] = $y[$p];
	    }
	}
	#print "num_o : ", $num_o, "\n";
	$num_o_average += $num_o;

#############################################################
# Decide whether a pair is considered as near-by by
# checking the surface distance.
#############################################################
	
	$num_pair = 0;

	for ( $i=1; $i<$num_o; $i++ )
	{
	    for ( $j=$i+1; $j<=$num_o; $j++ )
	    {

#############################################################
# reset relation to non-related
#############################################################
		$near_by[$i][$j] = 0;
		$near_by[$j][$i] = 0;

#############################################################
# check the surface distance of the pair to 
# decide if they are near-by.
#############################################################
		
		$x_ij = $x_o[$i] - $x_o[$j];
		$y_ij = $y_o[$i] - $y_o[$j];
		$r_ij = sqrt($x_ij**2 + $y_ij**2);
		$s_ij = $r_ij - 2.0*$R;
		
		if ( $s_ij <= $sur_dist_threshold )
		{
		    $near_by[$i][$j] = 1;
		    $near_by[$j][$i] = 1;
		    $num_pair++;
		    #print "s_ij : ", $s_ij, "\n";
		}
		else
		{
#############################################################
# check the surface distance of the pair by considering 
# periodic image in x direction to decide if they are near-by.
#############################################################

		    $x_ij = $x_o[$i] - $x_o[$j] - $Lx;
		    $y_ij = $y_o[$i] - $y_o[$j];
		    $r_ij = sqrt($x_ij**2 + $y_ij**2);
		    $s_ij = $r_ij - 2.0*$R;
		    
		    if ( $s_ij <= $sur_dist_threshold )
		    {
			$near_by[$i][$j] = 1;
			$near_by[$j][$i] = 1;
			$num_pair++;
			#print "s_ij : ", $s_ij, "\n";
		    }
		    else
		    {
#############################################################
# check the surface distance of the pair by considering 
# periodic image in x direction to decide if they are near-by.
#############################################################
			
			$x_ij = $x_o[$i] - $x_o[$j] + $Lx;
			$y_ij = $y_o[$i] - $y_o[$j];
			$r_ij = sqrt($x_ij**2 + $y_ij**2);
			$s_ij = $r_ij - 2.0*$R;
			
			if ( $s_ij <= $sur_dist_threshold )
			{
			    $near_by[$i][$j] = 1;	
			    $near_by[$j][$i] = 1;
			    $num_pair++;
			    #print "s_ij : ", $s_ij, "\n";
			}
		    } # else
		} # else
	    } # j
	}# i
	
#	print "number of near-by pairs: ", $num_pair, "\n";
	$num_pair_average+=$num_pair;
	
#############################################################
# Check two dimensional near_by matrix to find cluster.
# 1: reset each particle as un-assigned to any cluster.
#############################################################
	
	for ( $i=1; $i<=$num_o; $i++ )
	{
	    $assigned[$i] = 0;
	}
	
#############################################################
#2: assign every particle in each cluster with 
#   the minimum particle ID within this cluster.
#   This is done recursively.
#############################################################
		
	for ( $i=1; $i<=$num_o; $i++ )
	{
	    if ( $assigned[$i] == 0 )
	    {
		$assigned[$i] = $i;
		$id_current   = $i;
		recursive_detect($id_current, $i);
	    }
	    #print "calling recursive detection !\n";
	}# i
	    	
#############################################################	
# 2.9 to print for test
#############################################################	
#	for ( $i=1; $i<=$num_o; $i++ )
#	{
#	    for ( $j=$i+1; $j<=$num_o; $j++ )
#	    {
#		print $near_by[$i][$j], ' ';
#	    }
#	    print "\n";
#	}

#	for ( $i=1; $i<=$num_o; $i++ )
#	{
#	    print $assigned[$i], "\n";
#	}
#	exit;

#############################################################	
#3 Counter number of particles in each cluster.
#############################################################
	
	for ( $i=1; $i<=$num_o; $i++)
	{
	    $num_particle_cluster[$i] = 0;
	}
	for ( $i=1; $i<=$num_o; $i++)
	{
	    $num_particle_cluster[$assigned[$i]]++;
	}
	
#############################################################
#4 Change the color scheme.
#############################################################
	#for ( $i=1; $i<=$num_o; $i++)
	#{$assigned[$i] = $num_particle_cluster[$assigned[$i]];}
	
	#print "cluster ID, number of particles: \n";
	#for ($i=1;$i<=$num_o;$i++)
	#{
	#    print $i, ': ', $num_particle_cluster[$i], "\n";
	#}
	#last;
	
#############################################################
# for recording the number of different sized clusters at
# current time step.
#
# $num_cluster_current[$i] stores how many clusters with size $i.
# 
#############################################################

	for ( $i=1; $i<=$num_o; $i++ )
	{
	    $num_cluster_current[$i]=0.0;
	}
	
	for ( $i=1; $i<=$num_o; $i++)
	{
	    $num_cluster_current[$num_particle_cluster[$i]] ++;
	    $num_cluster[$num_particle_cluster[$i]] ++;
	}

#############################################################	
# 3.1 write certain snapshot with cluster ID for animation.
#############################################################	

	if ( $freq_ID_output !=0 && $num_file % $freq_ID_output == 0)
	{
	    $file_out_name_ID_output = ">". $dir_name. $subdir_name_ID_output . $f;
	    #print $file_out_name_ID_output, "\n";
	    open (OUTPUT_ID, $file_out_name_ID_output);
	    for ( $j=1; $j<=$num_o; $j++ )
	    {
		print OUTPUT_ID $x_o[$j], ' ', $y_o[$j], ' ', $assigned[$j], "\n";
	    }
	    close(OUTPUT_ID);	     
	}

#############################################################
# 3.2 output probability of cluster size at certain time steps.
#############################################################
	
	if ($freq_probability_output !=0 && $num_file % $freq_probability_output == 0)
	{
	    $file_out_pre=substr($file_out_name_probability,0,4);
	    $file_number = substr($f, 11, 8);
	    $file_out_name_probability_stepwise_output = ">". $dir_name. $subdir_name_probability_output . $file_out_pre . '_' .$file_number. ".dat";
	    #print $file_out_name_probability_stepwise_output, "\n";
	    open (OUTPUT_PROB, $file_out_name_probability_stepwise_output);
	    for ( $i=1; $i<=$num_o; $i++ )
	    {
		print OUTPUT_PROB $i, ' ', $num_cluster_current[$i] * $i / $num_o, "\n";
	    }
	    close(OUTPUT_ID); 
	    
	}

#############################################################	
# increase counter of files
#############################################################
	$num_file ++;

	#if ($num_file > 1 )
	#{ last;}
	
    } # if file is in consideration.
    
}# folder

print "total number of files processed: ", $num_file, "\n";
if ( $num_file < 1 )
{
    print "number of files is zero !\n";
    exit;
}

#############################################################
# 
#############################################################

$num_o_average    /= $num_file;
$num_pair_average /= $num_file;
print "averaged number of particles used in each file: ", $num_o_average, "\n";
print "averaged number of near-by pairs  in each file: ", $num_pair_average, "\n";
    
#############################################################
# Output file name.
#############################################################

$file_out_name_probability=">". $file_out_name_probability. ".dat";

open(OUT, $file_out_name_probability);
print OUT "#cluster size, number of this sized cluster \n";

$check=0;

for ( $i=1; $i<=$N; $i++ )
{
#############################################################
# First averaged over number of files processed.
#############################################################
    $num_cluster[$i]/=$num_file;
    
#############################################################
# Check the if averaged total number of particles conserved.
#############################################################

    $check+= $i*$num_cluster[$i];    

#############################################################
# Check the probability of particles in each-sized cluster. 
#############################################################
    
    $num_cluster[$i] =  $num_cluster[$i] * $i / $num_o_average;
    print OUT $i, ' ',$num_cluster[$i], "\n";
}

print "check sum of probability: ", $check, "\n";
close(OUT);

sub recursive_detect
{
#############################################################
# 0: id to assigned
# 1: particle id
#############################################################
    my $id_pass = $_[0];
    my $sp      = $_[1];
    
    for ( my $sq=1; $sq<=$num_o; $sq++ )
    {
	if ( $near_by[$sp][$sq] == 1 && $assigned[$sq] == 0 )
	{
	    $assigned[$sq] = $id_pass;
	    recursive_detect($id_pass, $sq);
	}
    }
    
    return;
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

