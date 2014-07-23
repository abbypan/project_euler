#!/usr/bin/perl
#3053105
use Data::Dump qw/dump/;
use strict;

my ($MAX) = @ARGV;

my $plist = get_prime_list($MAX);
our $MAX_P = ( sort { $b <=> $a } keys(%$plist) )[0];
my $m = get_matrix_23( $MAX, $MAX_P );

#dump($m);
my $s = update_matrix($m);

#dump($s);
my $sum = check_prime( $plist, $s );

#dump($sum);
print $sum, "\n";

sub check_prime {
    my ( $p, $m ) = @_;
    for my $x (@$m) {
        for my $r (@$x) {
            for my $n (@$r) {
                next unless ( exists $plist->{$n} );
                $plist->{$n}++;
                delete( $plist->{$n} ) if ( $plist->{$n} > 1 );
            }
        }
    }

    #    dump($plist);
    my @plist_uniq = grep { $plist->{$_} == 1 } keys %$plist;
    my $sum = 0;
    $sum += $_ for @plist_uniq;
    return $sum;
}

sub update_matrix {
    my ($m) = @_;

    #cell s : max_i , min_j
    my @s = ();
    my @c = ();

    for my $j ( 0 .. $#{ $m->[0] } ) {
        push @{ $s[0][$j] }, $m->[0][$j];
    }

    for my $i ( 1 .. $#$m ) {
        for my $j ( 0 .. $#{ $m->[$i] } ) {
            my $v    = $m->[$i][$j];
            my @temp = ($v);

            for my $i_last ( 0 .. $i - 1 ) {
                my $max_j = $#{ $m->[$i_last] };
                for my $j_last ( $j + 1 .. $max_j ) {
                    push @temp, merge_cell_values( $v, $s[$i_last][$j_last] );
                }
            }

            $s[$i][$j] = [ sort { $a <=> $b } @temp ];
        }
    }
    return \@s;
}

sub merge_cell_values {
    my ( $v, $v_list ) = @_;
    my @data;
    for my $vv (@$v_list) {
        my $k = $v + $vv;
        last if ( $k > $MAX_P );
        push @data, $k;
    }
    return @data;
}

sub get_matrix_23 {
    my ( $m, $max_p ) = @_;

    my @res;
    for ( my $i = 0, my $x = 1 ; $x < $m ; $i++, $x *= 2 ) {
        for ( my $j = 0, my $y = 1 ; $j < $m ; $j++, $y *= 3 ) {
            my $z = $x * $y;
            last if ( $z > $max_p );
            $res[$i][$j] = $z;
        }
    }

    $res[0][0] = 0;

    return \@res;
}

sub get_prime_list {
    my ($m) = @_;
    my @prime_list = ( 2, 3 );
    my $n = 3;
    while (1) {
        $n += 2;
        last if ( $n > $m );
        my $is_prime = 1;
        my $sqrt_n   = int( sqrt($n) );
        for my $p (@prime_list) {
            last if ( $p > $sqrt_n );
            next unless ( $n % $p == 0 );
            $is_prime = 0;
            last;
        }

        push @prime_list, $n if ($is_prime);
    }

    my %prime = map { $_ => 0 } @prime_list;
    return \%prime;
}
