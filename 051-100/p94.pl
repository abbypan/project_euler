#!/usr/bin/perl
our $MAX = 1000000000;
#our $MAX = 50;
my $n = 0;
our @SQUARE=(1);
our $SQUARE_I = 1;
our $SQUARE_ID = 2;

my $min = 0;

my $i=0;
my $id = 0;
my $im= 0;
while(1){
    $id+=2;
    $im = $im + 2*$i + 1;
    $i++;
    #print "i: $i, $id, $im\n";

    my $len1 = 3*$id +2;
    last if($len1>$MAX);

    my $d2 = 3*$im + $id;
    my $d1 = $d2 + $id + 1;
    while(1){
        last unless(@SQUARE);
        last if($SQUARE[-1]>=$d1);
        my $sn = $SQUARE[-1] + $SQUARE_ID + 1;
        $SQUARE_I++;
        $SQUARE_ID+=2;
        push @SQUARE, $sn;
    }

    #print "$i, $j, $len1, $d1, @SQUARE\n";
    $n += $len1 if(check_sim_tri($d1,$id, $id+1));

    my $i2=$i+1;
    my $len2 = $len1+2;

    if(($i-1)% 1000000==0){
        print "$i: square $#SQUARE, $len1, $len2\n" ;
    }
    
    $min = $d2;

    while(1){
        last unless(@SQUARE);
        last if($SQUARE[0]>=$min);
        shift @SQUARE;
    }
    next if($len2>$MAX);
    #print "$i2, $j, $len2, $d2, @SQUARE\n";
    $n += $len2 if(check_sim_tri($d2, $id+2, $id+1));

}

print "result: $n\n";

sub check_sim_tri {
    my ($d, $x, $y) = @_;

    for(@SQUARE){
        last if($_>$d);
        next if($_<$d);
        print "sim: $x, $y, $y\n"; 
        return 1;
    }
    return;
}

sub get_tri_delta {
    my ($i,$j) =@_;
    return $j*$j - $i*$i;
}

sub get_tri_len {
    my ($i, $j) = @_;
    return 2*($i+$j);
}
