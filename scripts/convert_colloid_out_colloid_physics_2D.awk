BEGIN{
    
}
{
    print "coll_shape  = 1"
    print "coll_radius = 1.0, 0.0"
    print "coll_x      =", $1, ",", $2
    print "coll_v      =", $3, ",", $4
    print "coll_omega  = 0.0, 0.0, ", $8
    print ""
}
END{
}