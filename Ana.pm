package Ana;

=encoding utf8

=head1 NAME

C<Ana> – Utility functions for anagrams

=head1 SYNOPSIS

        use Ana;
        if (Ana::are_anagrams($word1, $word2)) {
          print "This anagram has score ", Ana::score($word1, $word2), "\n";
        }

=head1 DESCRIPTION

=cut

use Carp 'confess';
use strict;


=head2 FUNCTIONS

=head3 C<normalize>

        $canonical_form = Ana::normalize($word)

Returns the canonical form of its argument.
Two words are guaranteed to be anagrams if and only if their
canonical forms are identical.

The canonical form contains the same letters as the input word,
converted to lowercase, in alphabetical order.

Inputs with non-letters are not handled.

=cut

sub normalize {
  my ($w) = @_;
  join "", sort split //, lc $w;
}

=head3 C<are_anagrams>

        $boolean = Ana::Are_anagrams($word1, $word2)

Returns true if the arguments are anagrams of one another.

=cut

sub are_anagrams {
  my ($a, $b) = @_;
  normalize($a) eq normalize($b);
}

=head3 C<score>

        $score = Ana::score($word1, $word2)

Returns the score of a given anagram pair.
The first word is cut into as few chunks as possible,
and the chnuks are rearranged to make the second word.
The score is the smallest possible number of chunks that
can do this.

Always returns a whole number between 1 and the length of the words.

=cut

sub score {
  my ($a, $b) = @_;
  confess "$a and $b are not anagrams" unless are_anagrams($a, $b);
  my $len = length($a);
  my $G = Ana::Graph->new_from_words($a, $b);
  my @mis = $G->mis;
  if (wantarray) {
    # return the simplest chunking that can map one word onto the other
    confess "unimplemented";
  } else {
    return $len - @mis;
  }
}

=head3 C<all_mappings>

        @mappings = Ana::all_mappings($word1, $word2);

Return a list of descriptions of mappings that transform
the first word into the second.

Say the words have length I<n>.

A mapping is simply an array containing a permutation of the indices 0
… I<n>-1 that describes where each letter of the first word goes to in
the second word.

For example, for the words

        stop post

there is exactly one  mapping, C<[2, 3, 1, 0]>,
because the letters C<s>, C<t>, C<o>, C<p>
in the first word
are mapped to the 2nd, 3rd, 1st, and 0th letters
in the second word.

Put another way,
if the letters of the first word are in C<@a>
and the letters of the second word are in C<@b>,
and the mapping is C<$m>,
then we always have:

        $a[$i] eq $b[ $m->[$i] ]

for each C<$i> between 0 and I<n>-1.

=cut

sub all_mappings {
  my ($a, $b) = @_;
  confess "$a and $b are not anagrams" unless are_anagrams($a, $b);
  my @a = split //, $a;
  my @b = split //, $b;
  my @mappings;

  my @queue = [ [], \@a, {} ];
  NODE: while (@queue) {
    my ($node) = shift @queue;
    my ($map, $left, $right_used) = @$node;
    if (@$left == 0) {
      push @mappings, $map;
      next NODE;
    }

    my ($l, @lrest) = @$left;
    for my $r_i (0 .. $#b) {
      my $r = $b[$r_i];
      next if $r ne $l or $right_used->{$r_i};
      push @queue, [ [ @$map, $r_i ], \@lrest, { %$right_used, $r_i => 1 } ];
    }
  }

  return @mappings;
}

=head3 C<mapping_score>

        $score = Ana::mapping_score($mapping);


Given a mapping
such as is returned by C<all_mappings>,
return its score.

The score is the number of contiguous chunks
into which it divides the two words
to rearrange one into the other.

=cut

sub mapping_score {
  my ($m) = @_;
  my $chunks = 1;
  for my $i (1 .. $#$m) {
    if ($m->[$i] != $m->[$i-1] + 1) {
      $chunks++;
    }
  }
  return $chunks;
}

1;
