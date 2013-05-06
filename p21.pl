#!/usr/bin/perl
use strict;
use warnings;
 
my $n = 10000;
 
my %div;
for my $i (1 .. $n) {
    for(my $j=$i+$i; $j<=$n; $j+=$i){
        $div{$j} +=$i;
    }
}
 
my %amicable;
for my $i ( 1 .. $n ){
    my $d = $div{$i};
    next if($d == $i);
    next unless($d and $d <= $n);
 
    my $k = $div{$d};
    next unless($k and $k <= $n);
 
    next unless($k == $i);
 
    $amicable{$i} = 1;
    $amicable{$d} = 1;
}
 
my $sum = 0;
$sum += $_ for keys(%amicable);
print "sum : $sum\n";

