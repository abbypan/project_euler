#!/usr/bin/perl
#设 y 为 2^t 次方
#解方程 y^2 = y + k ，得 y = [ 1 + (1+4k)^0.5] / 2
#存在划分 => 1+4k 必须为 平方数 (xx)
#存在完美划分 => (2*y - 1)^2 == 4k + 1　(yy)
#迭代检查 4k+1 , xx , yy 重合情况即可

our ($k, $k4) = ( 1, 5 );
our ($x, $xx, $xn) = ( 1, 1, 0 );
our ($y, $yy, $yn ) = ( 2, 9, 0 );

while(1){
    my $check_x = check_k4_xx($k4, $xx, $xn);
    if($check_x){
        $xn++;
        my $check_y = check_k4_yy($k4, $y, $yy);
        $yn++ if($check_y); 
    }
    #print "$k: $xn, $yn\n";
    last if($xn>0 and $yn>0 and $yn*12345<$xn);

    next_k4();

    #last if($k>185);
}
print "$k\n";

sub next_k4 {
    my $delta = int(($xx - $k4)/4);
    $delta = 1 unless($delta>0);
    $k+=$delta;
    $k4+=4*$delta;
}

sub check_k4_yy {
    while($yy<$k4){
        $yy = 4*$yy + 8*$y - 3;
        $y *=2;
    }
    return unless($yy==$k4);
    return 1;
}

sub check_k4_xx {
    while($xx<$k4){
        $xx += 2*$x + 1;
        $x++;
    }
    return unless($xx==$k4);
    return 1;
}
