#!/usr/bin/perl
use File::Slurp qw/slurp/;

my @lower = map { ord($_) } ( 'a' .. 'z' );
my $c = slurp('p59_cipher.txt');
chomp($c);
my @ciper = split ',', $c;

for my $x ( 0 .. $#lower ) {
    for my $y ( 0 .. $#lower ) {
        next if ( $x == $y );
        for my $z ( 0 .. $#lower ) {
            next if ( $x == $z or $y == $z );
            my ( $c, $n ) = decode_text( \@ciper, [ @lower[ $x, $y, $z ] ] );
            next unless ( $c =~ / the /i );
            print "$c\n$n\n";
        }
    }
}

sub decode_text {
    my ( $ciper_r, $key_r ) = @_;

    my @plain = ();
    my $sum   = 0;
    for ( my $i = 0 ; $i <= $#$ciper_r ; $i++ ) {
        my $j = $i % 3;
        my $n = $ciper_r->[$i] ^ $key_r->[$j];
        push @plain, chr($n);
        $sum += $n;
    }
    return ( join( "", @plain ), $sum );
}
