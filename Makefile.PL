use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'HTML::Filter::Easy',
    'VERSION' => '1.0.0', # finds $VERSION
    'PREREQ_PM' => {
		'Error' => 0,
		'HTML::PullParser' => 0,
    },
);
