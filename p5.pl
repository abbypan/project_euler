#!/usr/bin/perl

my %s;
for my $i(2 .. 20){
    my $t = parse_num($i);
    while(my ($k, $c) = each %$t){
        $s{$k} ||= 0;
        $s{$k} = $c if($s{$k} < $c);
    }
}

my $n=1;
while(my ($k, $c) = each %s){
   $n *= $k ** $c; 
}
print "$n\n";




sub parse_num {
   my ($n) = @_;
   my %s;
   ($n, $s{2}) = parse_num_p($n, 2);
   delete($s{2}) unless($s{2});

   my $p = 3;
   while($n>1){
       ($n, $s{$p}) = parse_num_p($n, $p);
       delete($s{$p}) unless($s{$p});
       $p+=2;
   }
   return \%s;
}

sub parse_num_p {
    my ($n, $p) = @_;
    
    my $c = 0;
        while($n % $p == 0){
            $c++;
            $n /= $p;
        }
    return ($n, $c);
}
