#!/usr/bin/perl

for my $a (1 .. 1000){
    for my $b ( 1 .. 1000){
        my $c = 1000 - $a - $b;
        next if($c < $a or $c < $b);
        next unless($c*$c == $a*$a + $b*$b);
        print $a*$b*$c, "\n";
    }
}
