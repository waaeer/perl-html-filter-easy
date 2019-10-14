use strict;
print "1..1\n";
my $txt = '<p class="ba"><span class="man" style="gor">Вау</span></p>';

use HTML::Filter::Easy;
my $dst ;

HTML::Filter::Easy->filter(src=>\$txt, dest=>\$dst, allowed_tags=>'all', forbidden_tag_options=>'span:style');
if ($dst eq '<p class="ba"><span class="man">Вау</span></p>') {
   print "ok 1\n";
} else {
   print STDERR "DEST=$dst\n";
   print "not ok 1\n";
}
