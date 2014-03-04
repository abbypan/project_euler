#!/usr/bin/perl

my @primes = (2 , 3 );
my $n = 5;
while($#primes<10000){
    my $flag = 1;
    for my $p (@primes){
        if($n % $p == 0){
            $flag = 0;
            last;
        }
    }
    push @primes, $n if($flag==1);
    $n+=2;
}

print $primes[-1], "\n";
