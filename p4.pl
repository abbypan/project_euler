#!/usr/bin/perl
my $s = 0;
for (my $i = 999; $i>=100;$i--){
    for (my $j=999;$j>=100; $j-- ){
        my $n = $i * $j;
        last if($s > $n); 
        $s = $n if(check_palindromic($n));
    }
}

print $s, "\n";

sub check_palindromic {
    my ($n) = @_;
    my $m = $n;
    my $ds = 0;
    while($n>0){
        my $d = $n % 10;
        $ds = 10 * $ds + $d;
        $n = int($n/10);
    }
    return $ds == $m ? 1 : 0 ;
}
