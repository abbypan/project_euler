#!/usr/bin/perl
use Memoize;
memoize('summations_count');

my $MAX_CNT = 5000;

my @primes = ( 2, 3 );
my $n = 4;
my $c;
while (1) {
    push @primes, $n if ( is_prime( $n, \@primes ) );
    $c = summations_count( $n, \@primes, 0 );
    #print "$n, $c, @primes\n";
    last if ( $c > $MAX_CNT );
    $n++;
}
print "n: $n, cnt: $c\nprime list: @primes\n";

sub is_prime {
    my ( $n, $plist ) = @_;
    for my $p (@$plist) {
        return 0 if ( $n % $p == 0 );
    }
    return 1;
}

sub summations_count {
    my ( $n, $items, $i ) = @_;
    return 1 if ( $n == 0 );
    return 0 unless ( @$items and $i <= $#$items and $items->[$i] <= $n );

    #print "$i : $n, $items->[$i]\n";

    my $m = $items->[$i];
    $i++;
    my $s = 0;
    for ( my $x = 0 ; $x <= $n ; $x += $m ) {
        $s += summations_count( $n - $x, $items, $i );
    }
    return $s;
}
