#!/usr/bin/perl -w

my @regs = 
  ('%eax', '%ecx', '%edx', '%ebx', '%esp', '%ebp', '%esi', '%edi');

print ".text\n";


sub gen1{
  my $tmpl = shift;
  foreach my $r1 (@regs){
      my $x = $tmpl;
      $x =~ s/r1/$r1/g;
      print $x;
  }
}




sub gen2{
  my $tmpl = shift;
  foreach my $r1 (@regs){
    foreach my $r2 (@regs){
      my $x = $tmpl;
      $x =~ s/r1/$r1/g;
      $x =~ s/r2/$r2/g;
      print $x;
    }
  }
}

print "sete %al\n";
print "sete %cl\n";
print "sete %dl\n";
print "sete %bl\n";
print "sete %ah\n";
print "sete %ch\n";
print "sete %dh\n";
print "sete %bh\n";

#gen1 "pop r1\n";
#gen1 "pop 12(r1)\n";
#gen1 "pop 10000(r1)\n";
#print "pushl \$0x5\n";
#print "pushl \$0x500\n";
#gen1 "pushl r1\n";
#gen1 "pushl 1(r1)\n";
#gen1 "pushl 1000(r1)\n";
#gen2 "orl 12(r2), r1\n";
#gen1 "orl \$0x400, r1\n";
#gen1 "cmpl \$0x4, r1\n";
#gen2 "cmpl 12(r2), r1\n";
#gen1 "cmpl \$0x400, 12(r1)\n";
#gen1 "cmpl \$0x4, 12000(r1)\n";
#gen1 "cmpl \$0x400, 12(r1)\n";
#gen2 "cmp r2, r1\n";
#gen1 "cmp \$0x312, r1\n";
#gen1 "cmp \$0x3, r1\n";
#gen2 "imull 0x10(r2), r1\n";
#gen2 "imull r2, r1\n";
#gen1 "imull \$0x1010, r1\n";
#gen1 "imull \$0x1000, r1\n";
#print "movl \$10, -1(%esp)\n";
#gen1 "jmp *-3(r1)\n";
#print "jmp L1+0x8\n";
#print "L1:\n";
#print "jmp .+0x8000\n";
#gen1 "negl r1\n";
#gen1 "notl r1\n";
#gen2 "andl 0x1200(r2), r1\n";
#gen2 "andl r1, r2\n";
#gen1 "andl \$0x10, r1\n";
#gen1 "sarl \$1, r1\n";
#gen1 "sarl %cl, r1\n";
#gen1 "sarl \$9, r1\n";
#gen2 "addl 0x10(r2), r1\n";
#gen2 "addl 0x100(r2), r1\n";
#gen1 "addl \$0x12, 0x10(r1)\n";
#gen1 "addl \$0x12, 0x100(r1)\n";
#gen1 "addl \$0x120, 0x10(r1)\n";
#gen1 "addl \$0x120, 0x100(r1)\n";
#gen2 "addl r1, r2\n";
#gen1 "addl \$0x10, r1\n";
#gen1 "addl \$0x1000, r1\n";

