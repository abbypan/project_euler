#!/usr/bin/perl
use strict;
use warnings;
 
my $num = 2000000;
#my $num = 10;
 
my @data = (0, 1, 2 .. $num);
 
for my $i ( 2 .. $#data){
    next if($data[$i]==0);
    my $j = $i+$i;
    while($j<=$num){
        $data[$j]=0;
        $j+=$i;
    }
}
 
my $sum = 0;
$sum +=$data[$_] for(2..$num);
 
print "$num , $sum\n";
