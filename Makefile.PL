use 5.006001;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'SWISH::HiLiter',
    VERSION_FROM      => 'HiLiter.pm', # finds $VERSION
    PREREQ_PM         => { 
	HTML::HiLiter => 0.11, 
	#SWISH::API => 0.03, 
	Test::More => 0.1 
    }, 
);
