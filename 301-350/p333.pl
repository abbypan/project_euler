#!/usr/bin/perl
#result: 3053105
use Data::Dump qw/dump/;
use strict;

my ($max) = @ARGV;
our $ROW_N = 2;
our $COL_N = 3;
our $PRIME = get_prime_list($max);
our $MAX_P = (sort { $b <=> $a } keys(%$PRIME))[0];
our (@m, @s, @c, @next_c);
init_head();
#dump('m', \@m, 's', \@s, 'c', \@c);

while(1){
#dump('m', \@m, 's', \@s, 'c', \@c);
check_prime(\@s);

calc_next_m();
last unless(@m);

@s = ();
for(my $col=$#m; $col>=0 ; $col--){
calc_next_s(\@m, \@s, \@c, $col);
calc_next_c(\@s, \@c, \@next_c, $col);
#dump('col', $col, 'm', \@m, 's', \@s, 'c', \@c, 'next_c', \@next_c);
}
$next_c[$#m+1] = $c[$#m+1] if($c[$#m+1]);
@c = @next_c;
@next_c=();
}
#dump($PRIME);
my $sum = 0;
while(my ($p, $cnt) = each %$PRIME){
$sum+=$p if($cnt==1);
}
print $sum, "\n";


sub check_prime {
my ($s) = @_;
#dump($s);
for my $r (@$s){
   for my $n (@$r){
	next unless(exists $PRIME->{$n});
	$PRIME->{$n}++;
	delete($PRIME->{$n}) if($PRIME->{$n}>1);
}
}
}

sub calc_next_s {
	my ($m, $s, $c, $col) = @_;
        my @data=($m->[$col]);
	push @data, map { $_ + $m->[$col] } @{$c->[$col+1]} if($c->[$col+1]);
        #$s->[$col] = [ sort { $a <=> $b } grep { $_<=$MAX_P } @data ];	
	@data = grep { $_<=$MAX_P } @data ;
        $s->[$col] = \@data;	
}

sub calc_next_c {
	my ($s,$c, $next_c, $col) = @_;
	my @data = ();
	push @data, @{$c->[$col]} if($c->[$col]);
	push @data, @{$s->[$_]} for ($col .. $#$s);
	#@data = sort { $a <=> $b } grep { $_<=$MAX_P } @data;
	@data = sort { $a <=> $b } @data;
	$next_c->[$col] = \@data;
}

sub calc_next_m {
	for my $i ( 0 .. $#m ){
		 $m[$i] *= $ROW_N;
		if($m[$i]>$MAX_P){
			$#m = $i-1;
			return;
		}
	} 
}


sub init_head {
	my $s=1;
	my $col=0;
	while($s<=$MAX_P){
		push @m, $s;
		push @{$s[$col]}, $s;

		$s*=$COL_N;
		$col++;
	}
	$s[0]=[];

	for(my $col = $#m; $col>=0; $col--){
		calc_next_c(\@s, \@c, \@next_c, $col); 
	}
	$next_c[0]=[];
	@c = @next_c;
	@next_c = ();
}


sub get_prime_list {
    my ($m) = @_;
    my @prime_list = (2, 3);
    my $n = 3;
    while(1){
        $n+=2;
        last if($n>$m);
        my $is_prime=1;
        my $sqrt_n = int(sqrt($n));
        for my $p (@prime_list){
            last if($p>$sqrt_n);
            next unless($n % $p == 0);
            $is_prime=0;
            last;
        }

        push @prime_list, $n if($is_prime);
    }

    my %prime = map { $_ => 0 } @prime_list;
    #return \@prime_list;
    return \%prime;
}
