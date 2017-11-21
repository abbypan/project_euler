#!/usr/bin/perl
use Tie::IxHash;
use Data::Dumper;
use Memoize;

memoize( 'all_c' );

my $src = 'thereisasyetinsufficientdataforameaningfulanswer';
my @clist = sort split //, $src;
our %s;
tie %s, 'Tie::IxHash';
for my $c ( @clist ) {
  $s{$c}++;
}
our @ck    = keys( %s );
our %ck_i  = map { $ck[$_] => $_ } @ck;
our $max_n = 15;

# u, uu, uuw, uw, uwu, uww, w, wu, wuu, wuw, ww, wwu
#print all_n({ 'u' => 2, 'w' => 2 }, [ 'u', 'w' ] ,  3, []), "\n";
#print limit_n({ 'u' => 2, 'w' => 2 }, [ 'u', 'w' ] ,  3, [ 'u', 'w', 'w' ], []),"\n";

my $cnt = P( 'legionary' ) + P( 'calorimeters' ) - P( 'annihilate' ) + P( 'orchestrated' ) - P( 'fluttering' );
print W( $cnt ), "\n";
#turnthestarson

sub P {
  my ( $str ) = @_;
  my @ss = split //, $str;
  my $c = limit_n( \%s, \@ck, $max_n, \@ss, [] );
  return $c;
}

sub limit_n {
  my ( $s, $ck, $max, $limit_arr, $arr ) = @_;

  #假设 uww

  unless ( @$limit_arr ) { #全排列，不含空字串

    #my $an = all_n($s, $ck, $max, $arr) ;
    my $an = all_c( $max, 0, grep { $_ > 0 } sort values( %$s ) );
    return $an;
  }

  my $cnt = 0;
  my $jc  = $limit_arr->[0];
  for my $c ( @$ck ) {
    last unless ( ( $c cmp $jc ) == -1 );  #低位开头的字符 axx < uww, bxx < uww
    next unless ( exists $s->{$c} );
    my %tmp = %$s;
    $tmp{$c}--;
    delete( $tmp{$c} ) if ( $tmp{$c} < 1 );

    #my $an = all_n(\%tmp, $ck, $max-1, [ @$arr, $c ]);
    my $an = all_c( $max - 1, 0, grep { $_ > 0 } sort values( %tmp ) );

    $cnt += $an;
  }

  my %tmp = %$s;
  $tmp{$jc}--;
  delete( $tmp{$jc} ) if ( $tmp{$jc} < 1 );
  my @x_arr = @$limit_arr;
  shift @x_arr;

  $cnt++; # u < uww

  $cnt += limit_n( \%tmp, $ck, $max - 1, \@x_arr, [ @$arr, $jc ] ) if ( @x_arr );  # 抽出u，剩下 ww

  return $cnt;
} ## end sub limit_n

sub all_n {
  my ( $s, $ck, $max, $arr ) = @_;
  return 0 if ( $max < 0 );
  my $c = $#$arr >= 0 ? 1 : 0;         # zero char
  if ( $#$arr >= 0 ) {

    #$main_i++;
    #print "$main_i arr: ",join("", @$arr),"\n";
  }
  for my $k ( @$ck ) {
    next unless ( exists $s->{$k} );
    my %tmp = %$s;
    $tmp{$k}--;
    delete( $tmp{$k} ) if ( $tmp{$k} == 0 );
    my $tmp_arr = [ @$arr, "$k" ];
    $c += all_n( \%tmp, $ck, $max - 1, $tmp_arr );
  }
  return $c;
} ## end sub all_n

sub all_c {
  my ( $max, $n, @d ) = @_;

  return 0 if ( $max < 0 );

  return 1 + scalar( @d ) if ( $max == 1 );

  my $cnt = $n >= 0 ? 1 : 0;
  for my $i ( 0 .. $#d ) {
    my $mm   = $max - 1;
    my $nn   = $n + 1;
    my @temp = @d;
    $temp[$i]--;
    @temp = grep { $_ > 0 } sort @temp;
    $cnt += all_c( $mm, $nn, @temp );
  }
  return $cnt;
} ## end sub all_c

#---------------------------

sub W {
  my ( $n ) = @_;
  my $str = $ck[0];
  my @ss = split //, $str;
  my $ssr = \@ss;

  my $i   = 1;
  my %tmp = %s;
  my $tr  = \%tmp;

  $tmp{ $ck[0] }--;
  my $cc = 0;
  my $new_s;
  while ( 1 ) {
    $cc++;
    if ( $i < $n ) {
      my ( $ts, $n_arr ) = next_s( $tr, $ssr );

      #$ss[-1] = $ck[$ck_i{$ss[-1]}+1];
      $new_s = join( "", @$n_arr );
      $i = P( $new_s );
      if ( $i > $n ) {
        ( $tr, $ssr ) = next_c( $tr, $ssr );
        $new_s = join( "", @$ssr );
        $i = P( $new_s );

        #\%tmp = $ts;
        #\@ss = $n_arr;
      } else {
        ( $tr, $ssr ) = ( $ts, $n_arr );
      }

    } ## end if ( $i < $n )
    return $new_s if ( $i == $n );

  } ## end while ( 1 )

} ## end sub W

sub next_s {
  my ( $s, $s_arr ) = @_;
  my %tmp   = %$s;
  my @t_arr = @$s_arr;

  my $c = $t_arr[-1];
  for my $k ( @ck ) {
    next unless ( exists $tmp{$k} );
    if ( ( $c cmp $k ) == -1 ) {
      $tmp{ $t_arr[-1] }++;
      $t_arr[-1] = $k;
      $tmp{$k}--;
      delete( $tmp{$k} ) if ( $tmp{$k} == 0 );
      return ( \%tmp, \@t_arr );
    }
  }
}

sub next_c {
  my ( $s, $s_arr ) = @_;

  my %tmp   = %$s;
  my @t_arr = @$s_arr;
  for my $k ( @ck ) {
    next unless ( exists $tmp{$k} );
    push @t_arr, $k;
    $tmp{$k}--;
    delete( $tmp{$k} ) if ( $tmp{$k} == 0 );
    return ( \%tmp, \@t_arr );
  }
}
