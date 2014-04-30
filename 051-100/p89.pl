#!/usr/bin/perl
#看 http://projecteuler.net/thread=89 中 arn.zarn的解法，更简单
our @DIGITS          = ( 1000, 500, 100, 50, 10, 5, 1 );
our $LARGEST_DIGIT   = $DIGITS[0];
our @SUBTRACT_DIGITS = ( 100, 10, 1 );
our %ROMAN_TO_DIGIT  = (
    'I' => 1,
    'V' => 5,
    'X' => 10,
    'L' => 50,
    'C' => 100,
    'D' => 500,
    'M' => 1000,
);
our %DIGIT_TO_ROMAN = reverse %ROMAN_TO_DIGIT;

my $all_short_len = 0;
open my $fh, '<', 'p89-roman.txt';
while (<$fh>) {
    chomp;
    my $n = roman_to_number($_);
    my $s = number_to_short_roman($n);

    #print "$_ -> $n -> $s\n";

    my $len = length($_) - length($s);
    $all_short_len += $len;
}
close $fh;
print "all short len: $all_short_len\n";

sub roman_to_number {
    my ($s) = @_;
    my @d = map { $ROMAN_TO_DIGIT{$_} } ( split //, $s );
    for my $i ( 0 .. ( $#d - 1 ) ) {
        $d[$i] = -$d[$i] if ( $d[$i] < $d[ $i + 1 ] );
    }
    my $res = 0;
    $res += $_ for @d;
    return $res;
}

sub number_to_short_roman {
    my ($n) = @_;

    # >= 1000
    my @res = ($LARGEST_DIGIT) x int( $n / $LARGEST_DIGIT );
    $n = $n % $LARGEST_DIGIT;

    # < 1000
    my $si = 0;
    for my $d (@DIGITS) {
        my @temp;
        if ( $n >= $d ) {
            ( $n, @temp ) = map_iiii_iv( $n, $d, $SUBTRACT_DIGIT[$si] );
            push @res, @temp;
        }

        $si++ while ( $n < $SUBTRACT_DIGITS[$si] );
        ( $n, @temp ) = map_viiii_ix( $n, $d, $SUBTRACT_DIGITS[$si] );
        push @res, @temp;

        last if ( $n == 0 );
    }

    my $new_s = join( "", map { $DIGIT_TO_ROMAN{$_} } @res );
    return $new_s;
}

sub map_iiii_iv {
    my ( $n, $d, $delta ) = @_;

    my $c = int( $n / $d );
    my @res = $c == 4 ? ( $delta, $d ) : ($d) x $c;
    $n %= $d;

    return ( $n, @res );
}

sub map_viiii_ix {
    my ( $n, $d, $delta ) = @_;

    my $new_n = $n + $delta;

    #$delta, $d : i x
    return ( $new_n >= $d ) ? ( $new_n - $d, $delta, $d ) : ($n);
}
