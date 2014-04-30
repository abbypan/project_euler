#!/usr/bin/perl
use Memoize;
memoize('summations_count');

my $n = 100;
my @items = ( 1 .. ( $n - 1 ) );
print summations_count( $n, \@items, 0 ), "\n";

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
