#!/usr/bin/perl

my $sum = 0;

for(my $i=3, my $j = 5, my $s = 0; $s < 1000;){
     if($i<$j){
         $sum+=$i unless($s==$i or $i>=1000);
         $s = $i;
         $i+=3;
     }else{
         $sum+=$j unless($s==$j or $j>=1000);
         $s = $j;
         $j+=5;
     }
}
print " below 1000 : $sum\n";
