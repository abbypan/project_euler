#!/usr/bin/perl
use strict;
use warnings;
use Data::Dump qw/dump/;
use Storable qw/dclone/;

my $sudoku_grids = read_sudoku_grids('p96-sudoku.txt');

my @r =
  map { $_->[1][1] * 100 + $_->[1][2] * 10 + $_->[1][3] }
  map { solve_sudoku($_) } @$sudoku_grids;
my $s = 0;
$s += $_ for @r;
print "sum : $s\n";

sub write_cell_uniq_number {
    my ($r) = @_;

    my $write_cell = 0;
    for my $i ( 1 .. 9 ) {
        for my $j ( 1 .. 9 ) {
            my $x = check_cell( $r, $i, $j );
            next unless (%$x);
            my @n = keys(%$x);
            if ( scalar(@n) == 1 ) {
                $r->[$i][$j] = $n[0];
                $write_cell = 1;
            }
        }
    }
    return $write_cell;
}

sub write_number_uniq_cell {
    my ($r) = @_;

    my @data;
    for my $i ( 1 .. 9 ) {
        for my $j ( 1 .. 9 ) {
            my $x = check_cell( $r, $i, $j );
            next unless (%$x);

            my ( $i0, $j0 ) = calc_block( $i, $j );
            my @n = keys(%$x);
            for my $n (@n) {
                push @{ $data[$i0][$j0][$n] }, [ $n, $i, $j ];
                push @{ $data[$i][0][$n] },    [ $n, $i, $j ];
                push @{ $data[0][$j][$n] },    [ $n, $i, $j ];
            }
        }
    }

    my $write_cell = 0;
    for my $ir (@data) {
        for my $jr (@$ir) {
            for my $nr (@$jr) {
                next unless ($nr);

                if ( $#$nr == 0 ) {
                    my ( $n, $i, $j ) = @{ $nr->[0] };
                    $r->[$i][$j] = $n;
                    $write_cell = 1;
                }
            }
        }
    }
    return $write_cell;
}

sub guess_cell {
    my ($r) = @_;

    my @guess;
    for my $i ( 1 .. 9 ) {
        for my $j ( 1 .. 9 ) {
            my $x = check_cell( $r, $i, $j );

            #guess fail
            return if ( $r->[$i][$j] == 0 and !%$x );

            next unless (%$x);

            my @n = keys(%$x);
            push @{ $guess[$#n] }, [ $i, $j, \@n ];
        }
    }

    for my $gr (@guess) {
        next unless ($gr);
        my ( $i, $j, $nr ) = @{ $gr->[0] };

        for my $n (@$nr) {
            my $dr = dclone($r);
            $dr->[$i][$j] = $n;

            my $s = solve_sudoku($dr);
            return $s if ($s);
        }
        return;
    }
}

sub is_solved_sudoku {
    my ($r) = @_;
    my $is_solved = 1;
    for my $i ( 1 .. 9 ) {
        for my $j ( 1 .. 9 ) {
            $is_solved = 0 if ( $r->[$i][$j] == 0 );
        }
    }

    return $is_solved;
}

sub solve_sudoku {
    my ($r) = @_;

    while (1) {
        my $solve_it = write_cell_uniq_number($r)
          || write_number_uniq_cell($r);
        last unless ($solve_it);
    }

    return $r if ( is_solved_sudoku($r) );

    return guess_cell($r);
}

sub read_sudoku_grids {
    my ($f) = @_;
    local $/ = undef;
    open my $fh, '<', $f;
    my $c = <$fh>;
    close $fh;
    my @data =
      map { read_sudoku( [ split /\n/ ] ) }
      grep { $_ } ( split /Grid.+?\n/, $c );
    return \@data;
}

sub calc_block {
    my ( $i, $j ) = @_;
    my $i0 = int( ( $i - 1 ) / 3 ) * 3;
    my $j0 = int( ( $j - 1 ) / 3 ) * 3;
    return ( $i0, $j0 );
}

sub check_cell {
    my ( $sudoku, $i, $j ) = @_;
    return {} if ( $sudoku->[$i][$j] );
    my %data = map { $_ => $_ } ( 1 .. 9 );

    #横
    for my $n ( 1 .. 9 ) {
        next if ( $sudoku->[$i][$n] == 0 );
        delete( $data{ $sudoku->[$i][$n] } );
    }

    #竖
    for my $n ( 1 .. 9 ) {
        next if ( $sudoku->[$n][$j] == 0 );
        delete( $data{ $sudoku->[$n][$j] } );
    }

    #块
    my ( $i0, $j0 ) = calc_block( $i, $j );

    for my $ix ( 1 .. 3 ) {
        for my $jx ( 1 .. 3 ) {
            my $y = $sudoku->[ $i0 + $ix ][ $j0 + $jx ];
            next if ( $y == 0 );
            delete( $data{$y} );
        }
    }
    return \%data;
}

sub read_sudoku {
    my ($arr) = @_;
    my @data = ( [ (0) x 10 ] );
    push @data, map {
        [ 0, map { $_ + 0 } ( split // ) ]
    } @$arr;
    return \@data;
}
