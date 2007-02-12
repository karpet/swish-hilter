#!/usr/bin/perl -w

use strict;
#use Data::Dumper;
use Test::More tests => 5;  # number of @queries
use SWISH::HiLiter;
use SWISH::API;

my $debug = shift @ARGV || 0;

# make temp index
my $index = 'test.index';
open(S, "| swish-e -S prog -i stdin -f $index 1>/dev/null 2>/dev/null")
    or die "can't exec swish-e: $!\n";
print S doc();
close(S) or die "can't close swish-e: $!\n";

my $swish = SWISH::API->new( $index );
my $hiliter = SWISH::HiLiter->new( swish=>$swish, debug=>$debug );

# query index
my @queries = qw/ foo bar title is this /;
Q: for my $q ( @queries )
{

my $results = $swish->Query( $q );
my @b = $hiliter->setq( $q );
#print "query = " . join(' ',@b) . "\n";

while (my $r = $results->NextResult )
{
    like( $hiliter->light( $hiliter->snip( $r->Property('swishtitle') ) ), qr/<span/, "hilite works for $q" );
    #print $hiliter->light( $hiliter->snip( $r->Property('swishtitle') ) );
    #print $/;
}


} # end Q


unlink $index; 
unlink "$index.prop";

#print Dumper \%INC;
#print $SWISH::HiLiter::VERSION, $/;
#print $SWISH::API::VERSION, $/;
#print Dumper $hiliter;
#print "HTML::HiLiter Debug = " . $HTML::HiLiter::Debug . $/;

exit;

sub doc
{
    my $t = "<html><title>this is foo bar title</title><body>foo bar</body></html>";
    my $now = time();
    my $l = length($t) + 1;
    return <<EOF;
Path-Name: foo
Content-Length: $l
Last-Mtime: $now

$t
EOF
}

