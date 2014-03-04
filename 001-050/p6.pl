#!/usr/bin/perl

my $si = 0;
my $sj = 0;

for my $i (1 .. 100){
    $si += $i*$i;
    $sj += $i;
}

$sj *= $sj;

my $s = $sj - $si;

print "$s\n";
