#!/usr/bin/perl -w

use strict;

my %times;
my %benchmarks;
my %runtimes;
my %gctimes;

{
  my $curtime;
  my $curbench;
  my $counter = 0;
  open F, "<log.time" or die;
  while(<F>){
    if (/^NOW: (.*)/){
      $curtime = $1;
      $times{$curtime} = ++$counter;
      next;
    }
    if(/^running stats for (.*):$/){
      $curbench = $1;
      $benchmarks{$curbench} ||= ++$counter;
      next;
    }
    if(/^ *(\d*) ms elapsed cpu time, including (\d*) ms collecting$/){
      $runtimes{$curbench}{$curtime} = $1;
      $gctimes{$curbench}{$curtime} = $2;
      next;
    }
  }
  close F;
}

my @times = sort { $times{$a} <=> $times{$b} } keys %times;
my @benchmarks = sort { $benchmarks{$a} <=> $benchmarks{$b} } keys %benchmarks;

foreach my $bench (@benchmarks){
  print "benchmark: $bench\n";
  foreach my $time (@times){
    printf "   %6s             on $time\n", $runtimes{$bench}{$time};
  }
}


