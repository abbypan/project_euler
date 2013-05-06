#!/usr/bin/perl
use strict;
use warnings;
 
my $n = 1;
my $div_num = 500;
 
my %div;
while(1){
    print "\r$n";
    my $k = $n % 2 == 0 ? ($n/2) : $n;
    my $t = $n % 2 == 0 ? ($n+1) : ($n+1)/2;
    $div{$k} ||= get_divs($k);
    $div{$t} ||= get_divs($t);
    last if($div{$k} * $div{$t} > $div_num);
    $n++;
}
 
my $m = $n*($n+1)/2;
print "1 .. $n -> $m \n" ;
 
 
sub get_divs {
    my ($m) = @_;
 
    my @temp = ( 0 , 1 ..  $m );
    for my $i ( 2 .. $m ) {
        next if($temp[$i]==0);
        next if($m % $i == 0);
        for(my $j=$i; $j<$m; $j+=$i){
            $temp[$j]=0;
        }
    }
    my @divs = grep { $_ > 0 } @temp;
    return @divs;   
}

