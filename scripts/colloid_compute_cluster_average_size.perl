#########################################
# post processing for cluster anaylsis
# main output is average cluster size
#########################################

$batch_mode=1;

$file_in_name=$ARGV[0];

open(file_in, $file_in_name);

$particles_probability_sum=0.0;
$particles_probability_max=0.0;
$particles_probability_max_cluster_size=1;

$particles_cluster_size_max=1.0;

$cluster_probability_sum=0.0;
$cluster_average_size=0.0;

#########################################
#remove the first comment line of the data.
#########################################

$line=<file_in>;

while ( $line=<file_in> )
{

    @data  = split(' ', $line);
    
    $particles_probability_sum += $data[1];

    $cluster_probability = $data[1]/$data[0];
    $cluster_probability_sum   +=  $cluster_probability;

    if ( $data[1] > 0.0 )
    {
	$particles_cluster_size_max = $data[0];
	
	if ($data[1] > $particles_probability_max )
	{
	    $particles_probability_max = $data[1];
	    $particles_probability_max_cluster_size = $data[0];
	}
    }
}


$cluster_average_size = 1.0/$cluster_probability_sum;

if ($batch_mode == 1)
{
    print $cluster_average_size, "\n";
}
else
{
    print "particles probability sum              : ", $particles_probability_sum, "\n";
    print "particles cluster size max             : ", $particles_cluster_size_max, "\n";
    print "particles probability max              : ", $particles_probability_max, "\n";
    print "particles probability max cluster size : ", $particles_probability_max_cluster_size, "\n";
    
    print "cluster probability sum : ", $cluster_probability_sum, "\n";
    print "cluster average size    : ", $cluster_average_size, "\n";
}
