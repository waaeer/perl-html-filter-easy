use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME'	=> 'HTML::Filter::Easy',
    'VERSION' => '1.0.0', 
    'PREREQ_PM' => {
		'Error' => 0,
		'HTML::PullParser' => 0,
    },
);
