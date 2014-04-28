#!/usr/bin/perl
use bignum;
#use Scalar::Util::Numeric qw(isint);

# $b (blue) + $r (red) = $n (all)
# $b*($b-1)*2 = $n($n-1)
# $b*($b-1)*2 = ($b + $r) ($b + $r -1)
# $b^2 -(2*$r+1)*$b - ($r^2 - $r) = 0
# $b = (2*$r+1 + sqrt[(2*$r+1)^2 + 4($r^2 - $r)])/2
# $b = (2*$r+1 + sqrt(8*$r*$r + 1))/2
# 1, 9, 289  => #http://oeis.org/A055792
# x(n) = 34x(n-1) - x(n-2) - 16

sub iter_x {
    my ( $r, $j ) = @_;
    $r ||= 1;
    $j ||= 9;
    return sub {
        my $z = 34 * $j - $r - 16;
        $r = $j;
        $j = $z;
        return $z;
    };
}

my $MAX_N  = 10**12;
my $iter_x = iter_x();

while (1) {
    my $x = $iter_x->();
    my $r = sqrt( ( $x - 1 ) / 8 );
    my $b = ( 2 * $r + 1 + sqrt($x) ) / 2;
    my $n = $b + $r;
    print "blue : $b \n";
    last if ( $n > $MAX_N );
}
