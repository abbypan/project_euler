#!/usr/bin/perl

my $thr = 4000000;

my $sum = 0;
my $i = 1;
my $j = 1;
while($i<$thr){
   $sum += $i if($i % 2 == 1);
   my $s = $i + $j;
   $i = $j;
   $j = $s;
}
print " below $thr : $sum\n";
