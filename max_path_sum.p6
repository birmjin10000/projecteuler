v6;

my $file = open "/home/birmjin/p2.txt", :r;
my $lineCounter = 1;
my $maxPathSum = 0;

my $leftOffset = 0;
my $rightOffset = 0;

my @rows = lines $file;

$maxPathSum = @rows[0];
say $maxPathSum;

tread_triangle($lineCounter, $leftOffset, $rightOffset, @rows);

sub tread_triangle($lineCounter, $leftOffset is rw, $rightOffset is rw, @rows) {
  my @numbers;
  my $leftSum = 0;
  my $rightSum = 0;
  my $lineNumber = $lineCounter; 

  while (($lineNumber < @rows.elems) && (($lineNumber - $lineCounter) < 3) ) {
    @numbers = split(/' '+/, @rows[$lineNumber]);
    $leftSum += @numbers[$leftOffset] / ($lineNumber - $lineCounter + 1);
    $rightSum += @numbers[$lineNumber - $rightOffset] / ($lineNumber - $lineCounter + 1);
    $lineNumber++;
  }

#  if $lineCounter == 2 {
#    say '-----';
#    say $leftSum;
#    say $rightSum;
#  }
  
  my @selected = split(/' '+/, @rows[$lineCounter]);

  if (($leftSum <=> $rightSum) >= 0) {
    $maxPathSum += @selected[$leftOffset];
    say @selected[$leftOffset];
    $rightOffset++;
  }
  else {
    $maxPathSum += @selected[$lineCounter - $rightOffset];
    say @selected[$lineCounter - $rightOffset];
    $leftOffset++;
  }
 
  if ($lineCounter == (@rows.elems - 1)) {
    
  }
  else {
    tread_triangle($lineCounter + 1, $leftOffset, $rightOffset, @rows);
  }
}

say $maxPathSum;

=begin comment
for $file.lines -> $line {
  say $line;
  my @numbers = split(/' '+/, $line);
  
  if ($lineCounter != 1) {
    $leftSum += @numbers[0];
    $rightSum += @numbers[$lineCounter - 1]
  }
  $lineCounter++;
}

say $leftSum;
say $rightSum;

if (($leftSum <=> $rightSum) >= 0 ) {
  $rightOffset++;
}
=end comment
