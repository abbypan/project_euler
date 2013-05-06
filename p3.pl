#!/usr/bin/perl

my $n = 600851475143;
#my $n = 13195;

my $p = 3;
while($n>1){
    while($n % $p == 0){
        $n /= $p;
    }
    $p+=2 if($n>$p);
}

print "$n : $p\n";
