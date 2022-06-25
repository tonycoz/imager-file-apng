#!perl
use strict;
use warnings;
use Imager::File::APNG;
use Test::More;

use constant PI => 3.14159265358979;

# simple
{
  my @ims;
  my $r = 40;
  for my $theta_index (0 .. 7) {
    my $im = Imager->new(xsize => 100, ysize => 100);
    my $theta = $theta_index * (PI / 4) + PI / 6;
    my $rcos = $r * cos($theta);
    my $rsin = $r * sin($theta);
    $im->line(x1 => 50 + $rcos, y1 => 50 + $rsin,
              x2 => 50 - $rcos, y2 => 50 - $rsin,
              aa => 1, color => "#FF0", endp => 1);
    push @ims, $im;
    $im->write(file => "testout/frame$theta_index.png");
  }
  my $data;
  ok(Imager->write_multi({ data => \$data, type => "apng" }, @ims),
     "write APNG")
    or diag(Imager->errstr);
  -d "testout" or mkdir "testout";
  open my $fh, ">", "testout/myfirstapng.png"
    or die;
  binmode $fh;
  print $fh $data;
  close $fh;
}

done_testing();
