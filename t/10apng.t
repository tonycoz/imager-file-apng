#!perl
use strict;
use warnings;
use Imager::File::APNG;
use Test::More;
use Imager::Test qw(is_image test_image test_image_gray test_image_16);

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
  }
  my $data;
  ok(Imager->write_multi({
    data => \$data,
    type => "apng",
    apng_delay => 0.5,
  }, @ims),
     "write APNG")
    or diag(Imager->errstr);
  -d "testout" or mkdir "testout";
  open my $fh, ">", "testout/myfirstapng.png"
    or die;
  binmode $fh;
  print $fh $data;
  close $fh;

  {
    my @inims = Imager->read_multi(type => "apng", data => $data);
    is(@inims, @ims, "read back as many as we wrote");
    is_image($inims[2], $ims[2], "compare one of them");
    is(0+$inims[2]->tags(name => "apng_delay"), 0.5, "check delay tag set");
  }
}

# mix RGB and grey
{
  my $im1 = test_image;
  my $im2 = test_image_gray;
  my $data;
  ok(Imager->write_multi({ type => "apng", data => \$data }, $im1, $im2),
     "write mixed RGB and gray");
  my @im = Imager->read_multi(type => "apng", data => $data);
  is(@im, 2, "read both back");
  is($im[1]->getchannels, 3, "second image is now RGB");
  my $im2cmp = $im2->convert(preset => "rgb");
  is_image($im[1], $im2cmp, "check it is the RGB we expect");
}

# mix RGB and RGBA
{
  my $im1 = test_image;
  my $im2 = $im1->convert(preset => "addalpha");
  my $data;
  ok(Imager->write_multi({ type => "apng", data => \$data }, $im1, $im2),
     "write mixed RGB and RGBA");
  my @im = Imager->read_multi(type => "apng", data => $data);
  is(@im, 2, "read both back");
  is($im[0]->getchannels, 4, "second image is now RGBA");
  is_image($im[0], $im2, "check it is the RGBA we expect");
}

# 8-bit gray and 16-bit RGBA
{
  my $im1 = test_image_gray;
  my $im2 = test_image_16()->convert(preset => "addalpha");
  is($im2->bits, 16, "make sure we're still 16-bit");
  my $data;
  ok(Imager->write_multi({ type => "apng", data => \$data }, $im1, $im2),
     "write mixed gray and RGBA 16-bit");
  my @im = Imager->read_multi(type => "apng", data => $data);
  is(@im, 2, "read both back");
  my $im1cmp = $im1->convert(preset => "rgb")->convert(preset => "addalpha")->to_rgb16;
  is($im[0]->bits, 16, "written as 16-bit");
  is_image($im[0], $im1cmp, "check the contents");
}

done_testing();
