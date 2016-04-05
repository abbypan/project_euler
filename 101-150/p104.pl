#!/usr/bin/perl 
use strict;
use warnings;

print pandigital_fib(), "\n";

sub pandigital_fib {
    my $i = [1];
    my $j = [1];
    my $n = 3;

    while(1){
        my $ov = 0;
        for my $id ( 0 .. $#$j){
            $i->[$id] ||= 0;
            my $k = $i->[$id] + $j->[$id] + $ov;
            if($k>9){
                $ov = 1;
                $k = $k % 10;
            }else{
                $ov = 0;
            }
            $i->[$id] = $k;
        }
        push @$i, 1 if($ov);
        print join("", @$i).", $n\n" if($n % 1000==0);

        if($#$i>=8){
            return $n if(
                check_pandigital($i, 0, 8) and check_pandigital($i, -9, -1)
            );
        }

        ($i, $j) = ($j, $i);
        $n++;
    }

}

sub check_pandigital {
    my ($r, $from, $to) = @_;

    my %flag;

    for my $x ( @{$r}[ $from .. $to ] ){
        return if($x==0);
        return if(exists $flag{$x});
        $flag{$x} = 1;
    }
    return 1;
}

