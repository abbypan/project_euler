#!/usr/bin/perl
use strict;
use warnings;
 
my $n = 1000000;
 
my %chain;
my %point_to_k;
my @data = ( 2 .. $n );
while(@data){
    my $i = shift @data;
    my $k = ($i % 2==0) ? ($i/2) : (3*$i+1);
    $chain{$i} = $k;
    $point_to_k{$k}++;
    push @data , $k if($k >$n);
}
 
my @no_mark = map { [ $_ , $chain{$_}, 1 ] }
                grep { ! exists $point_to_k{$_} }
                keys(%chain);
 
while(1){
    my @new_no_mark = grep { $_->[1]!=1 }
        map { [ $_->[0], $chain{$_->[1]}, $_->[2]+1 ] } @no_mark;
    last unless(@new_no_mark);
    @no_mark = @new_no_mark;
}
 
print "$_->[0] : $_->[2]\n" for @no_mark;

