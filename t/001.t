use strict;
use Test::More tests => 1;

SKIP: {

    eval { require SWISH::API };

    skip "SWISH::API is not installed - can't test SWISH::HiLiter", 21 if $@;

    skip "SWISH::API 0.04 or higher required", 21
        unless ( $SWISH::API::VERSION && $SWISH::API::VERSION ge '0.03' );

    require_ok('SWISH::HiLiter');
    diag("Testing SWISH::HiLiter version $SWISH::HiLiter::VERSION");

}
