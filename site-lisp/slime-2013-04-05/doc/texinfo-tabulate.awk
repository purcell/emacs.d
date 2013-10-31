#!/usr/bin/env awk -f
#
# Format input lines into a multi-column texinfo table.
# Note: does not do texinfo-escaping of the input.

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

BEGIN {
  columns = 3;
  printf("@multitable @columnfractions");
  for (i = 0; i < columns; i++)
    printf(" %f", 1.0/columns);
  print
}

{ if (NR % columns == 1) printf("\n@item %s", $0);
  else                   printf(" @tab %s", $0); }

END { printf("\n@end multitable\n"); }

