#!/usr/bin/perl
use strict;
use warnings;
 
my @name;
open my $fh,'<', 'names.txt';
while(<$fh>){
    push @name, /"(.+?)"/g;
}
close $fh;
 
@name = sort map { uc $_ } @name;
 
my @CHAR = ( '0' , 'A' .. 'Z');
my %char = map { $CHAR[$_] => $_ } ( 1 .. $#CHAR);
 
my $sum=0;
for my $i ( 0 .. $#name){
    my $n = $name[$i];
    my @ch = $n=~/(.)/g;
    my $s = 0;
    $s+= $char{$_} for @ch;
    $sum+= $s*($i+1);
}
print "sum : $sum\n";

