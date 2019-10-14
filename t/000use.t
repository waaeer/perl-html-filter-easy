use strict; my $was_error;
 eval {require 'Easy.pm';};  if($@) { $was_error = 1; print STDERR "Compilation failed : $@\n"; }  
print "1..1
"; print $was_error ? "not ok 1
Bail out!
" : "ok 1
";
