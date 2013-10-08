v6;

my $file = open "/home/birmjin/projecteuler_perl6/p2.txt", :r;
my @rows = lines $file;
my $maxPathSum = @rows[0];
say $maxPathSum;

tread_triangle(1, 0, 0, @rows);

sub tread_triangle($lineNumber, $leftArrayOffset, $rightArrayOffset, @rows) {
  my @numbers;
  my $leftSum = 0;
  my $rightSum = 0;
  my $leftOffset = $leftArrayOffset;
  my $rightOffset = $rightArrayOffset;

  @numbers = split(/' '+/, @rows[$lineNumber]);
  $leftSum = @numbers[$leftOffset];
  $rightSum = @numbers[$lineNumber - $rightOffset];
 
  if ($lineNumber < @rows.elems- 1) {
    my @nextrow_numbers;
    @nextrow_numbers = split(/' '+/, @rows[$lineNumber + 1]);
    $leftSum += (@nextrow_numbers[$leftOffset] > @nextrow_numbers[$leftOffset + 1] ?? @nextrow_numbers[$leftOffset] !!@nextrow_numbers[$leftOffset + 1]);
    $rightSum += (@nextrow_numbers[$lineNumber + 1 - $rightOffset - 1] > @nextrow_numbers[$lineNumber + 1 - $rightOffset] ?? @nextrow_numbers[$lineNumber + 1 - $rightOffset - 1] !! @nextrow_numbers[$lineNumber + 1 - $rightOffset]);
  }

#  while (($lineNumber < @rows.elems) && (($lineNumber - $lineNumber) < 3) ) {
#    @numbers = split(/' '+/, @rows[$lineNumber]);
#    $leftSum += @numbers[$leftOffset] / ($lineNumber - $lineNumber + 1);
#    $rightSum += @numbers[$lineNumber - $rightOffset] / ($lineNumber - $lineNumber + 1);
#    $lineNumber++;
#  }

  if (($leftSum <=> $rightSum) >= 0) {
    $maxPathSum += @numbers[$leftOffset];
    say @numbers[$leftOffset];
    $rightOffset++;
  }
  else {
    $maxPathSum += @numbers[$lineNumber - $rightOffset];
    say @numbers[$lineNumber - $rightOffset];
    $leftOffset++;
  }
 
  if ($lineNumber == (@rows.elems - 1)) {
    say "End of triangle. Line number:" ~ $lineNumber; 
  }
  else {
    tread_triangle($lineNumber + 1, $leftOffset, $rightOffset, @rows);
  }
}

say $maxPathSum;
