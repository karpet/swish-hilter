
=pod

=head1 NAME

SWISH::HiLiter - simple interface to SWISH::API and HTML::HiLiter


=head1 VERSION

0.04


=head1 SYNOPSIS

   my $query = "foo OR bar";
   
   require SWISH::API;
   my $swish = SWISH::API->new( 'my_index' );
   
   require SWISH::HiLiter;
   
   # create an object
   
   my $hiliter = SWISH::HiLiter->new( swish=>$swish, query=>$query );
      
   # search and display highlighted results
   
   my $results = $swish->Query( $query );
   
   while ( my $result = $results->NextResult ) {
	
	my $path 	= $result->Property( "swishdocpath" );
	my $title 	= $hiliter->light(
				$result->Property( "swishtitle" )
			  );
	my $snip 	= $hiliter->light(
			    $hiliter->snip(
				$result->Property( "swishdescription" )
			    )
			  );
	my $rank 	= $result->Property( "swishrank" );
	my $file	= $result->Property( "swishreccount" );
        
	print join("\n", $file, $path, $title, $rank, $snip );
	
   }
   

=head1 DESCRIPTION

SWISH::HiLiter is a simple interface to the HTML::HiLiter module. It is designed
to work specifically with the SWISH::API module for searching SWISH indexes and displaying
snippets of highlighted text from the stored SWISH properties.

SWISH::HiLiter is B<NOT> a drop-in replacement for the highlighting modules that
come with the SWISH-E distribution. Instead, it is intended to be used when programming
with SWISH::API.

=head1 REQUIREMENTS

HTML::HiLiter of course, which can also be used for full-page highlighting.

If you intend to use full-page highlighting, also get the HTML::Parser and its
required modules.

Perl 5.6.1 or later.

SWISH::API 0.04 or later.

=cut

package SWISH::HiLiter;

use 5.006001;
use strict;
use sigtrap qw(die normal-signals error-signals);

require HTML::HiLiter;
eval { require SWISH::API };    # eval per cpan bug request 14003

# the fuzzy word access was fixed in SWISH::API 0.04
# but this module will still work under 0.03 if not using fuzzy
if ($@ or $SWISH::API::VERSION < 0.03)
{
    die "SWISH::HiLiter requires SWISH::API version 0.03 or newer\n";
}

our ($VERSION, $Context, $Occurrences, $Max_Chars, $Debug, $ellip, $Word_Ave);

$VERSION = 0.04;

$ellip       = ' ... ';    # for snipping
$Debug       = 0;          # duh.
$Max_Chars   = 300;        # for dumb_snip
$Occurrences = 5;          # number of instances
$Context     = 8;          # number of words before and after match to include
$Word_Ave = 5;    # assume the average word is 5 chars (used in re_snip() )

# optimizing help
my $ticker;

eval { require Pubs::Times; };

unless ($@)
{

    $ticker = 1;
    Pubs::Times::tick('start swish::hiliter');
}

$ticker = 0;    # set to 0 to turn off timing report

=pod

=head1 VARIABLES

You may set the following package variables. The default values are listed after each.

=over

=item

$SWISH::HiLiter::Debug [ 0 ]

=item

$SWISH::HiLiter::Max_Chars [ 300 ]

=item

$SWISH::HiLiter::Occurrences [ 5 ]

=item

$SWISH::HiLiter::Context [ 7 ]

=back

=head1 METHODS


=head2 new()

Create a SWISH::HiLiter object. The new() method takes either a single parameter (a
SWISH::API object), or a hash of parameter key/values. Available parameters include:

=over

=item swish

A SWISH::API object. Version 0.03 or newer. [ Required ]

=item colors

A reference to an array of HTML color names.

=item query

The query string you want highlighted.


=item metanames

A reference to an array of SWISH metanames in the index you are searching.
This list is used in setq() to accomodate searches like 'metafoo=bar'. If you
do not provide a list, it will be automatically retrieved from the index you are
searching using the SWISH::API object you pass to new().

=item occurrences

How many query matches to display when running snip(). See $SWISH::HiLiter::Occurrences.

=item max_chars

Number of words around match to return in snip(). See $SWISH::HiLiter::Context.

=item noshow

Bashful setting. If snip() fails to match any of your query (as can happen if the match
is beyond the range of SwishDescription as set in your index), don't show anything. The
default is to show the first $Max_Chars of the text. See also snip=>'dumb'.


=item snip

There are three different snipping approaches. Each has its advantages.

=over

=item dumb

Use the brute force dumb_snip approach in snippet(). This is fastest but likely won't show
any matches to your query unless they occur in the first $Max_Chars of your text.

=item re

Uses the regular expression snip. It's slower but a little smarter. B<NOTE:> The
regexp snip is used by default if any phrases are in your query.

=item loop [ default ]

Splits your text into 'swish words' and compares each against your query. A medium (speed and accuracy)
approach for non-phrase queries.

=back


=item escape

Your text is assumed not to contain HTML markup and so it is HTML-escaped by default.
If you have included markup in your text and want it left as-is, set 'escape' to 0. Highlighting
should still work, but snip() might break...

=back

=cut

sub new
{
    my $package = shift;
    my $self    = {};
    bless($self, $package);
    $self->_init(@_);
    return $self;
}

sub _init
{
    my $self = shift;
    $self->{'start'} = time;

    # if one param, assume it's a swish object
    # otherwise treat it as a hash

    if (scalar @_ == 1)
    {

        $self->{swish} = shift @_;

    }
    elsif (@_)
    {

        my %extra = @_;
        @$self{keys %extra} = values %extra;

    }
    else
    {

        die "new() needs at least a SWISH::API object\n";

    }

    # unless we are running under util=>1,
    # create a HTML::HiLiter object and remember it
    unless ($self->{util})
    {
        require HTML::HiLiter;
        $self->{hiliter} = new HTML::HiLiter(
            debug => $self->{debug} || $Debug,
            SWISH => $self->{swish},
            Colors => $self->{colors} || $self->{Colors} || undef,
            Parser => 0    # don't require HTML::Parser etc.
                                            );

    }

    _word_regexp($self);

    $self->{occurrences} ||= $Occurrences;
    $self->{max_chars}   ||= $Max_Chars;
    $self->{context}     ||= $Context;
    $self->{debug}       ||= $Debug;
    $self->{escape} = 1 unless defined $self->{escape};
    $self->{snip} ||= '';
    $self->{snipfunc} = \&loop_snip;                               # default
    $self->{snipfunc} = \&re_snip if $self->{snip} =~ /re/i;
    $self->{snipfunc} = \&dumb_snip if $self->{snip} =~ /dumb/i;

    if ($self->{query})
    {

        return $self->setq($self->{query});

    }
    1;

}

sub stem
{

=pod

=head2 stem( I<word> )

Return the stemmed version of a word. Only works if your first index in SWISH::API
object used Fuzzy Mode.

This method is just a wrapper around SWISH::API::Fuzzify.

B<NOTE:> stem() requires SWISH::API version 0.04 or newer.

=cut

    if ($SWISH::API::VERSION < 0.04)
    {
        die "stem() requires SWISH::API version 0.04 or newer\n";
    }
    my $self  = shift;
    my $w     = shift;
    my $index = $self->{index} || ($self->{swish}->IndexNames)[0];
    $self->{index} ||= $index;

    my $fw = $self->{swish}->Fuzzify($index, $w);

    my @fuzz = $fw->WordList;

    if (my $e = $fw->WordError)
    {

        warn "Error in Fuzzy WordList ($e): $!\n";
        return undef;

    }

    return $fuzz[0];    # we ignore possible doublemetaphone

}

sub light
{

=pod

=head2 light( I<text> )

Returns highlighted I<text>. See new() for ways to control context, length, etc.

=cut

    my $self = shift;
    my $t    = shift;
    Pubs::Times::tick('light()') if $ticker;

    return $self->{hiliter}->hilite($t);

    #return $t;

}

sub setq
{

=pod

=head2 setq( I<query> )

Set the query in the highlighting object. Called automatically by new() if
'query' is present in the new() call.

With no I<query>, returns the parsed query string (scalar) currently in the object.
Otherwise, returns the array of query terms that will be highlighted. Identical to calling
HTML::HiLiter::Queries in array context.

You should only call setq() if changing the query value (as under mod_perl or in a loop)
or to simply see what the parsed query looks like.

B<NOTE>: setq() is not the same as the ParsedWords() SWISH::API method. The chief
differences:

=over

=item stemming

ParsedWords() returns the query as it was actually used for the search, which means that the 
words were stemmed if your index used stemming. SWISH::HiLiter needs the query B<as you entered it>,
not as it was stemmed. SWISH::HiLiter will handle the stemming internally, by abusing some regexp trickery.

=item phrases

Phrases are kept together in setq(), while they are broken up by white space in ParsedWords().

=back

Example:

	my (@q) = $hiliter->setq( 'my query' );

=cut

    my $self    = shift;
    my $q       = shift || return $self->{query};
    my $hiliter = $self->{hiliter};

    delete $hiliter->{sortedq}; # MUST do this to allow plaintext to work right.
        # fixes bug reported by sylvain.amrani@gendarmerie.org
        # on 2/22/06, where setq() would fail in a loop of queries

    # get list of metanames for correct setq()
    my @meta = $self->{metanames} || $hiliter->_metanames;

    my @q       = $hiliter->Queries($q, \@meta);
    my $regexps = $hiliter->Queries;

    #print "Q: $_ -> $Q->{$_}\n" for sort keys %$Q;

    $self->{query}       = join(' ', @q);
    $self->{query_array} = [@q];

    # create regexp for loop_snip()
    # other regexp come from HTML::HiLiter
    my @re;
    my $wc = $hiliter->{WordCharacters};

    for (@q)
    {

        #warn "q before: $_\n";

        my $q = quotemeta($_);    # quotemeta speeds up the match, too
                                  # even though we have to unquote below

        $q =~ s/\\\*/[$wc]*/;  # wildcard -- HTML::HiLiter does a better regexp.
                               # this is just in the ballpark.

        push(@re, $q);

        $self->{safe}->{$_}    = $q;
        $self->{simple}->{$_}  = $regexps->{$_}->[1];
        $self->{complex}->{$_} = $regexps->{$_}->[0];

    }
    my $j = join('|', @re);
    $self->{qre} = qr/($self->{ignoref}$j$self->{ignorel})/i;

    warn __PACKAGE__, " qre: $self->{qre}\n" if $self->{debug};

    # set up the style
    $hiliter->Inline;

    return @q;

}

sub _word_regexp
{

    # this based on SWISH::PhraseHighlight::set_match_regexp()

    my $self = shift;
    my $wc = $self->{hiliter}->{WordCharacters} || '\w\-\.\/';
    $self->{wc_regexp} = qr/[^$wc]+/io;  # regexp for splitting into swish-words

    my $igf = $self->{hiliter}->{IgnoreFirstChar};
    my $igl = $self->{hiliter}->{IgnoreLastChar};
    for ($igf, $igl)
    {
        if ($_)
        {

            # $_ = quotemeta;  # already quoted
            $_ = "[^$_]*";
        }
        else
        {
            $_ = '';
        }
    }

    $self->{ignoref} = $igf;
    $self->{ignorel} = $igl;
}

#######################################################################
# pick a snipper, any snipper
# I tried Text::Context but that was too slow
# here are several different models.
# I have found that loop_snip() is faster for single-word queries,
# while re_snip() seems to be the best compromise between speed and accuracy

sub snipper
{

    # backwards compatible naming
    return snip(@_);

}

sub snip
{

=pod

=head2 snip( I<text> )

Return a snippet of text from I<text> that matches
I<query> plus N words of context. N is defined in config as C<context>.

Gives you the google(tm)-like context for queries in search results.

B<NOTE:> This method can be a real bottleneck. Consider the snip=>'dumb' option
in new() if you see it slowing down your code.

=cut

    my ($self, $t) = @_;
    my $q = $self->{query_array} || undef;
    $self->{snipfunc} = \&dumb_snip unless $q;

    my $func = $self->{snipfunc};
    $func = \&re_snip if grep { /\ / } @$q and $self->{snip} ne 'dumb';

    #phrases must use re_snip

    # don't snip if we're less than the threshold
    return $t if length($t) < $self->{max_chars};

    my $s = &$func(@_);    # sanity check again

    $s = $self->dumb_snip($s) if length($s) > ($self->{max_chars} * 2);

    return $s;

}

sub _re_match
{

    # the .{0,$char} regexp slows things WAY down. so just match, then use pos() to get
    # chars before and after.

    # if escape = 0 and if prefix or suffix contains a < or >, try to include entire tagset.
    # but don't try too hard.

    my ($self, $text, $re, $cnt, $total, $snips, $ranges, $occur, $char) = @_;

    Pubs::Times::tick('start re_match') if $ticker;

    my $t_len = length $$text;

  RE: while ($$text =~ m/$re/gi)
    {

        #		warn "re: '$re'\n";
        #		warn "\$1 = '$1' = ", ord( $1 ), "\n";
        #		warn "\$2 = '$2'\n";
        #		warn "\$3 = '$3' = ", ord( $3 ), "\n";

        my $match = $2;
        $$cnt++;
        my $pos = pos $$text;

        #warn "already found $pos\n" if exists $ranges->{$pos};
        next RE if exists $ranges->{$pos};

        my $len = length $match;

        my $start_match = $pos - $len - 1;    # -1 to offset $1
        $start_match = 0 if $start_match < 0;

        # sanity
        #warn "match should be: '", substr( $$text, $start_match, $len ), "'\n";

        my $prefix_start =
          $start_match < $$char
          ? 0
          : $start_match - $$char;

        my $prefix_len = $start_match - $prefix_start;

        #$prefix_len++; $prefix_len++;

        my $suffix_start = $pos - 1;                      # -1 to offset $3
        my $suffix_len   = $$char;
        my $end          = $suffix_start + $suffix_len;

        # if $end extends beyond, that's ok, substr compensates

        $ranges->{$_}++ for ($prefix_start .. $end);

        #		warn "prefix_start = $prefix_start\n";
        #		warn "prefix_len = $prefix_len\n";
        #		warn "start_match = $start_match\n";
        #		warn "len = $len\n";
        #		warn "pos = $pos\n";
        #		warn "char = $$char\n";
        #		warn "suffix_start = $suffix_start\n";
        #		warn "suffix_len = $suffix_len\n";
        #		warn "end = $end\n";

        my $prefix = substr($$text, $prefix_start, $prefix_len);
        my $suffix = substr($$text, $suffix_start, $suffix_len);

        #		warn "prefix: '$prefix'\n";
        #		warn "match:  '$match'\n";
        #		warn "suffix: '$suffix'\n";

        # try and get whole words if we split one up
        # _no_*_partial does this more rudely

        # might be faster to do m/(\S)*$prefix/i
        # but we couldn't guarantee position accuracy
        # e.g. if $prefix matched more than once in $$text, we might pull the wrong \S*

        unless ($prefix =~ m/^\s/
                or substr($$text, $prefix_start - 1, 1) =~ m/(\s)/)
        {
            while (--$prefix_start >= 0
                   and substr($$text, $prefix_start, 1) =~ m/(\S)/)
            {
                my $onemorechar = $1;

                #warn "adding $onemorechar to prefix\n";
                $prefix = $onemorechar . $prefix;

                #last if $prefix_start <= 0 or $onemorechar !~ /\S/;
            }
        }

        # do same for suffix

        # We get error here under -w
        # about substr outside of string -- is $end undefined sometimes??

        unless ($suffix =~ m/\s$/ or substr($$text, $end, 1) =~ m/(\s)/)
        {
            while ($end <= $t_len and substr($$text, $end++, 1) =~ m/(\S)/)
            {

                my $onemore = $1;

                #warn "adding $onemore to suffix\n";
                #warn "before '$suffix'\n";
                $suffix .= $onemore;

                #warn "after  '$suffix'\n";
            }
        }

        #$self->{escape} = 1;

        # will likely fail to include one half of tagset if other is complete
        unless ($self->{escape})
        {
            my $sanity = 0;
            my @l      = ($prefix =~ /(<)/g);
            my @r      = ($prefix =~ /(>)/g);
            while (scalar @l != scalar @r)
            {

                @l = ($prefix =~ /(<)/g);
                @r = ($prefix =~ /(>)/g);
                last
                  if scalar @l ==
                  scalar @r;    # don't take any more than we need to

                my $onemorechar = substr($$text, $prefix_start--, 1);

                #warn "tagfix: adding $onemorechar to prefix\n";
                $prefix = $onemorechar . $prefix;
                last if $prefix_start <= 0;
                last if $sanity++ > 100;

            }

            $sanity = 0;
            while ($suffix =~ /<(\w+)/ && $suffix !~ /<\/$1>/)
            {

                my $onemorechar = substr($$text, $end, 1);

                #warn "tagfix: adding $onemorechar to suffix\n";
                $suffix .= $onemorechar;
                last if ++$end > $t_len;
                last if $sanity++ > 100;

            }
        }

        #		warn "prefix: '$prefix'\n";
        #		warn "match:  '$match'\n";
        #		warn "suffix: '$suffix'\n";

        my $context = join('', $prefix, $match, $suffix);

        #warn "context is '$context'\n";

        push(@$snips, $context);

        $$total++;

        #		warn '-' x 40, "\n";

        last if $$cnt == $$occur;
    }

    Pubs::Times::tick("end re_match - total is $$total") if $ticker;
    1;
}

sub re_snip
{

    # get first N matches for each q, then take one of each till we have $occur

    Pubs::Times::tick('start re_snip()') if $ticker;

    my $self = shift;
    my $t    = shift;
    my $q    = shift || $self->{query_array};

    my $occur   = $self->{occurrences};
    my $hiliter = $self->{hiliter};
    my $char    = $self->{context} * $Word_Ave;
    my %snips;
    my @snips;
    my $total = 0;
    my $notwc = $self->{wc_regexp};
    my %ranges;    # keep track of all pos() in @snips
                   # so that we don't overlap snips
                   # that contain multiple query words (like a virtual phrase)

  Q: for my $q (@$q)
    {
        my $cnt = 0;
        $snips{$q} = [];

        # try simple regexp first, then more complex if we don't match
        $self->_re_match(\$t, $self->{simple}->{$q},
                         \$cnt, \$total, $snips{$q}, \%ranges, \$occur, \$char);

        next Q if $cnt;    # if we have at least some simple, go to next q

        pos $t = 0;        # do we really need to reset this?

        $self->_re_match(\$t, $self->{complex}->{$q},
                         \$cnt, \$total, $snips{$q}, \%ranges, \$occur, \$char);

    }

    return $self->dumb_snip($t) unless $total;

    # get all snips into one array in order retrieved
    # should be a max of $occur in any one $q snip array
  N: for (0 .. $occur)
    {

        #warn "total is $total\n";
      Q: for my $q (@$q)
        {
            next Q unless exists $snips{$q};
            next Q unless scalar @{$snips{$q}};
            push(@snips, shift @{$snips{$q}});
        }
    }
    @snips = splice @snips, 0, $occur;

    #warn "0: '$snips[0]'\n";
    #warn "t: ", substr($t, 0, 20), "\n";

    $snips[0] = $ellip . $snips[0] unless $t =~ m/^\Q$snips[0]/i;
    $snips[-1] .= $ellip unless $t =~ m/\Q$snips[-1]$/i;

    my $snip = join($ellip, @snips);

    _escape($snip) if $self->{escape};

    return $snip;

}

sub loop_snip
{

    my $self = shift;
    my $txt  = shift || return '';

    my $regexp = $self->{qre};

    #warn "regexp: $regexp\n";
    #warn "match!\n" if $txt =~ m/$regexp/;

    # no matches
    return $self->dumb_snip($txt) unless $txt =~ m/$regexp/;

    Pubs::Times::tick('start loop_snip()') if $ticker;

    my $context = $self->{context};
    my $max     = $self->{max_chars};
    my $occur   = $self->{occurrences};
    my @snips;

    my $notwc = $self->{wc_regexp};

    my @words       = split(/($notwc)/, $txt);
    my $count       = -1;
    my $start_again = $count;
    my $total       = 0;

  WORD: for my $w (@words)
    {

        #warn ">>\n" if ($count / 2) =~ m/\./ ;
        #warn "word: '$w'\n";
        $count++;
        next WORD if $count < $start_again;

        # the next WORD lets us skip past the last frag we excerpted

        my $last = $count - 1;
        my $next = $count + 1;

        #warn '-' x 30 . "\n";
        if ($w =~ m/^$regexp$/)
        {

            #warn "w: '$w' match: '$1'\n";

            my $before = $last - $context;
            $before = 0 if $before < 0;
            my $after = $next + $context;
            $after = $#words if $after > $#words;

            #warn "$before .. $last, $count, $next .. $after\n";

            my @before = @words[$before .. $last];
            my @after  = @words[$next .. $after];

            $total += grep { m/^$regexp$/i } (@before, @after);
            $total++;    # for current $w

            my $t = join('', @before, $w, @after);

            $t .= $ellip unless $count == $#words;

            #$t = $ellip . $t unless $count == 0;

            #warn "t: $t\n";

            #warn "total: $total\n";

            push(@snips, $t);
            last WORD if scalar @snips >= $occur;

            $start_again = $after;
        }

        last WORD if $total >= $occur;

    }

    #warn "snips: " . scalar @snips;
    #warn "words: $count\n";
    #warn "grandtotal: $total\n";
    #warn "occur: $occur\n";

    #warn '-' x 50 . "\n";

    my $snippet = join('', @snips);
    $snippet = $ellip . $snippet unless $snippet =~ m/^$words[0]/;

    # convert special HTML characters
    _escape($snippet) if $self->{escape};

    Pubs::Times::tick('end loop_snip()') if $ticker;

    return $snippet;

}

sub _escape
{
    my %c = (
        '>' => '&gt;',
        '<' => '&lt;',
        '&' => '&amp;',

        #"\xa0" 	=> '&nbsp;',	# this should be optional
        # since HTML::HiLiter won't like it
        '"' => '&quot;'
            );
    my $C = join '', keys %c;
    $_[0] =~ s/([$C])/$c{$1}/og;
    1;
}

sub _no_start_partial
{
    $_[0] =~ s/^\S+\s+//gs;
}

sub _no_end_partial
{
    $_[0] =~ s/\s+\S+$//gs;
}

sub dumb_snip
{

    # just grap the first X chars and return

    Pubs::Times::tick('start dumb_snip()') if $ticker;
    my $self   = shift;
    my $txt    = shift;
    my $noshow = $self->{noshow} || 0;
    my $max    = $self->{max_chars};

    my $show = $noshow ? '' : substr $txt, 0, $max;

    $show =~ s/\s+\S+$//gs;    # no ending partial words
    $show .= $ellip;

    _escape($show)                       if $self->{escape};
    Pubs::Times::tick('end dumb_snip()') if $ticker;
    return $show;

}

1;
__END__


=pod

=head1 LIMITATIONS

If your text contains HTML markup and escape = 0, snip() may fail to return
valid HTML. I don't consider this a bug, but listing here in case it happens to you.

Stemming and regular expression building considers only the first index's header values
from your SWISH::API object. If those header values differ (for example, WordCharacters
is defined differently), be aware that only the first index from SWISH::API::IndexNames is used.

B<REMINDER:> Use HTML::HiLiter to highlight HTML markup; use SWISH::HiLiter to highlight plain text.



=head1 AUTHOR

Peter Karman, karman@cray.com

Thanks to the SWISH-E developers, in particular Bill Moseley for graciously
sharing time, advice and code examples.

Comments and suggestions are welcome.


=head1 COPYRIGHT

 ###############################################################################
 #    CrayDoc 4
 #    Copyright (C) 2004 Cray Inc swpubs@cray.com
 #
 #    This program is free software; you can redistribute it and/or modify
 #    it under the terms of the GNU General Public License as published by
 #    the Free Software Foundation; either version 2 of the License, or
 #    (at your option) any later version.
 #
 #    This program is distributed in the hope that it will be useful,
 #    but WITHOUT ANY WARRANTY; without even the implied warranty of
 #    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 #    GNU General Public License for more details.
 #
 #    You should have received a copy of the GNU General Public License
 #    along with this program; if not, write to the Free Software
 #    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 ###############################################################################


=head1 SUPPORT

Send email to swpubs@cray.com.


=head1 SEE ALSO

L<HTML::HiLiter>, L<SWISH::API>

=cut
