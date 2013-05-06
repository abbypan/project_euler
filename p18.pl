#!/usr/bin/perl
use strict;
use warnings;
 
my @data = (
[ qw/75/],
[ qw/95 64/],
[ qw/17 47 82/],
[ qw/18 35 87 10/],
[ qw/20 04 82 47 65/],
[ qw/19 01 23 75 03 34/],
[ qw/88 02 77 73 07 63 67/],
[ qw/99 65 04 28 06 16 70 92/],
[ qw/41 41 26 56 83 40 80 70 33/],
[ qw/41 48 72 33 47 32 37 16 94 29/],
[ qw/53 71 44 65 25 43 91 52 97 51 14/],
[ qw/70 11 33 28 77 73 17 78 39 68 17 57/],
[ qw/91 71 52 38 17 14 91 43 58 50 27 29 48/],
[ qw/63 66 04 68 89 53 67 30 73 16 69 87 40 31/],
[ qw/04 62 98 27 23 09 70 98 73 93 38 53 60 04 23/],
);
 
my @queue = ( [ [  ], 0 ] );
for my $i ( 0 .. $#data ){
    print "\nrow step : $i\n";
    my @max_queue = map { $_->[1] } @queue;
    for my $j ( 0 .. $i ){
        print "col step : $j\n";
        my $n = $data[$i][$j];
        my $q_old ;
 
        if($j==0){
            $q_old = $j;
        }elsif($j==$i){
            $q_old = $j-1;
        }else{
            print "col $j : $i, $j\n";
            my $q_l = $max_queue[$j-1];
            my $q_r = $max_queue[$j];
            print "l : $q_l\n";
            print "r : $q_r\n";
            $q_old = $q_l > $q_r ? ($j-1) : $j;
        }
        my @old_path = $i>0 ? @{$queue[$q_old][0]}[0 .. $i-1] : ();
        print "select $q_old : $n\n";
        $queue[$j] = [ [ @old_path, $n ], $max_queue[$q_old]+$n ];
        print "\nqueue[$j] : [ @{$queue[$j]->[0]} ], $queue[$j]->[1]\n";
    }
}
 
my $max_queue = ( sort { $b->[1] <=> $a->[1] } @queue )[0];
print "\nmax_queue : [ @{$max_queue->[0]} ], $max_queue->[1]\n";

