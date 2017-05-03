package Ana;

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

package Ana::Graph;
use Carp 'confess';
use strict;

# A graph has:
#  A list of vertex names
#  A hash mapping names to vertex indices
#  A two-dimensional adjacency array of edges ($adj->[$i][$j])
#
# [ Vnames, Vmap, Adj ]

# new_graph([ vertex-names... ])
sub new_graph {
  my ($self, $vnames) = @_;
  my $class = ref($self) || $self;
  my @vnames = @$vnames;
  my %vmap = map { $vnames[$_] => $_ } 0 .. $#vnames;
  my $adj = [];
  bless [ \@vnames, \%vmap, $adj ] => $class;
}

sub V { $_[0][0] }
sub E {
  my ($self) = @_;
  my $adj = $self->adj;
  my @E;
  for my $i (0 .. $#$adj) {
    next unless my $adj_i = $adj->[$i];
    for my $j ($i+1 .. $#$adj) {
      push @E, [ $self->vnames($i, $j) ] if $adj_i->[$j];
    }
  }
  return wantarray ? @E : \@E;
}

sub adj { $_[0][2] }

sub add_vertices {
  my ($self, @vnames) = @_;
  push $self->V->@*, @vnames;
}

sub add_edges {
  my ($self, @edges) = @_;
  $self->add_edge($_) for @edges;
}

sub add_edge {
  my ($self, $e) = @_;
  my ($v1, $v2) = $self->vi(@$e);
  $self->adj->[$v1][$v2] = 1;
  $self->adj->[$v2][$v1] = 1;
}

sub vi {
  my ($self, @vnames) = @_;
  confess "need array context for multiple arguments to ->vi()"
    if !wantarray() && @vnames > 1;
  my @vi;
  push @vi, $self->[1]{$_} // confess "Unknown vertex '$_'" for @vnames;
  wantarray ? @vi : $vi[0];
}

sub vnames {
  my ($self, @indices) = @_;
  return ($self->V->@*)[@indices];
}

sub are_adjacent {
  my ($self, $v, $w) = @_;
  my ($vi, $wi) = $self->vi($v, $w);
  $self->adj->[$vi][$wi];
}

sub adjacent_any {
  my ($self, $v, @set) = @_;
  for my $w (@set) {
    return 1 if $self->are_adjacent($v, $w);
  }
  return;
}

sub neighbors {
  my ($self, $v) = @_;
  my $adj_to = $self->adj->[$self->vi($v)] // [];
  return $self->vnames(grep $adj_to->[$_], 0 .. $#$adj_to);
}

# Remove all the neighbors of $v from @$vlist
# and return the result
sub remove_neighbors_of {
  my ($self, $v, $vlist) = @_;
  $DB::single = 1;
  my ($vi, @vli) = $self->vi($v, @$vlist);
  my $adj = $self->adj->[$vi];
  return [$self->vnames(grep ! $adj->[$_], @vli)];
}

# Given words A and B:
# the vertices are pairs (i, j) such that A[i] = B[j] and A[i+1] = B[j+1]
# two vertices are connected if the corresponding pair mappings are compatible
sub new_from_words {
  my ($class, $a, $b) = @_;
  my (@a) = split //, $a;
  my (@b) = split //, $b;

  my @V;
  for my $i (0 .. $#a-1) {
    for my $j (0 .. $#b-1) {
      if ($a[$i] eq $b[$j] && $a[$i+1] eq $b[$j+1]) {
        push @V, "$i,$j";
      }
    }
  }

  my $G = $class->new_graph(\@V);

  for my $q1 (0 .. $#V-1) {
    for my $q2 ($q1+1 .. $#V) {
      my ($i, $j) = split /,/, $V[$q1];
      my ($k, $l) = split /,/, $V[$q2];
      my $BAD = 0;
      if ($i == $k && $j != $l) { $BAD = 1 }
      elsif ($i+1 == $k && $j+1 != $l) { $BAD = 1 }
      elsif ($i+1 < $k && !disjoint([$j, $j+1], [$l, $l+1])) { $BAD = 1 }

      if ($BAD) {
        $G->add_edges( [ @V[$q1, $q2] ] );
      }
    }
  }

  return $G;
}

sub disjoint {
  my ($S, $T) = @_;
  for my $s (@$S) {
    for my $t (@$T) {
      return if $s eq $t;
    }
  }
  return 1;
}

sub to_dot_mis {
  my ($self) = @_;
  my @vertex_lines = map qq["$_";], $self->V->@*;
  my @edge_lines = map { sprintf qq["%s" -- "%s";], $_->@[0,1] } $self->E;
  my $dot = join "\n", @vertex_lines, @edge_lines;
  return "graph G {\n$dot\n}\n";
}

sub to_dot {
  my ($self) = @_;
  my @V = $self->V->@*;
  my @vertex_lines = map qq["$_";], @V;

  my @edge_lines;
  for my $i (0 .. $#V-1) {
    for my $j ($i+1 .. $#V) {
      push @edge_lines, sprintf qq["%s" -- "%s";], $V[$i], $V[$j]
        if $self->are_adjacent($V[$i], $V[$j]);
    }
  }
  my $dot = join "\n", @vertex_lines, @edge_lines;
  return "graph G {\n$dot\n}\n";
}

sub components {
  my ($self) = @_;
  my @components;
  my @V = $self->V->@*;
  my %used;
  while (@V) {
    my $V = shift @V;
    next if $used{$V};
    my @queue = ($V);
    my @component;

    # DFS starting from V
    while (@queue) {
      my $v = shift @queue;
      next if $used{$v}++;
      push @component, $v;
      push @queue, $self->neighbors($v);
    }
    push @components, \@component;
  }
  return @components;
}

sub subgraph {
  my ($self, @subV) = @_;
  my %V = map { $_ => 1 } @subV;
  my $S = $self->new_graph(\@subV);
  for my $e ($self->E) {
    if ($V{$e->[0]} && $V{$e->[1]}) {
      $S->add_edge($e);
    }
  }
  return $S;
}

# This finds a maximal independent set of a possibly not-connected graph
# be decomposing it into components and solving the problem on each subcomponent
sub mis {
  my ($self) = @_;
  my @mis;
  for my $c ($self->components) {
    my $s = $self->subgraph(@$c);
    push @mis, $s->mis_component->@*;
  }
  return wantarray ? @mis : \@mis;
}

# This finds a maximal independent set in a graph, but for disconnected
# graphs it may be a lot faster to use ->mis
use Time::HiRes ();
sub mis_component {
  my ($self) = @_;
  my @V = sort special_vertex_sort $self->V->@*;

  my $start = Time::HiRes::time();

  my $best_mis = [];
  my @queue = ([[], \@V]);
  my $count = 0;

  # DFS starting from the empty set
  while (@queue) {
    my ($set, $pool) = pop(@queue)->@*;
    next if @$set + @$pool <= @$best_mis;

    $best_mis = $set if @$set > @$best_mis;

    my ($first, @new_pool) = @$pool;
    next unless defined $first;
    push @queue, [   $set          , \@new_pool ];

    unless ($self->adjacent_any($first, @$set)) {
      my $smaller_pool = $self->remove_neighbors_of($first, \@new_pool);
      push @queue, [ [ @$set, $first], $smaller_pool ] if
        @$set + 1 + @$smaller_pool > @$best_mis;
    }

    die "Timed out" if ++$count % 100000 == 0 && Time::HiRes::time() - $start > 10;

  }
  return wantarray ? @$best_mis : $best_mis;
}

sub special_vertex_sort {
  my ($amatch, $bmatch) = (scalar($a =~ /^(\d+),\1$/),
                           scalar($b =~ /^(\d+),\1$/));
  if    (  $amatch && ! $bmatch) { return -1 }
  elsif (! $amatch &&   $bmatch) { return 1 }
  else { return 0 }
}

1;
