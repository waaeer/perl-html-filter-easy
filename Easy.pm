package HTML::Filter::Easy;
use strict;
use HTML::PullParser;
use HTML::Entities;
use IO::File;
use Error;
use File::Basename;

my $default_literal_tags                  = "style script";	
my $default_delete_content                = "form frameset frame";
my $default_delete_content_if_not_allowed = "title style script applet";
my $default_noclose     = "br img hr tex param link xsubst meta tr th td li dt dd input"; # Tags which do not need to be closed
my $default_nonest      = "a p";        # Tags which cannot be nested
my $default_nobreak     = "sub sup";    # Tags ....

sub filter { 
  my ($class,%opts) = @_;
  my $self = $class->new(%opts); 
  $self->process;
  if($self->{out_fh}) { 
     $self->{out_fh}->close;
  }
  return $self;
}

sub lineno {
  return int($_[0]->{current_line});
}

sub default_allowed_tags          { return "" } ;
sub default_allowed_tag_options   { return "" } ;
sub default_forbidden_tags        { return "" } ;
sub default_forbidden_tag_options { return "" } ;


sub new { 
  my $self = {};
  my ($class, %opts )  = @_;
  $opts{src}  	         					|| throw Error::Simple('HTML::Filter::Easy::new: required parameter src is missing');
##print "Start parsing $opts{src}\n";
  $self->{__boolean_attribute_value} = 'Dis is SIG-nature 4 b(0)(0)lean vaLue';
  $self->{__parser}     = HTML::PullParser->new( (ref $opts{src} ? (doc=>$opts{src}): (file=>$opts{src})) ,
	          start => '"S",tagname,attr,attrseq,line',
			  end   => '"E",tagname,line',
			  text  => '"T",text,line',
			  comment=>'"C",text,line',
			  boolean_attribute_value => $self->{__boolean_attribute_value},
  )   || throw Error::Simple("HTML::Filter::Easy::new: could not construct HTML::PullParser($opts{src}): $!");
  
  if ($opts{dest}) {
       if( ref $opts{dest}) { 
         my $ref = $opts{dest};
         $self->{__out} = sub { $$ref.=$_[0] };
       } else {
         my $out_fh = $self->{__out_fh} = IO::File->new($opts{dest},'w')  || throw Error::Simple("HTML::Filter::Easy::new: could not construct IO::File(>$opts{dest}): $!");
         $self->{__out} = sub { $out_fh->print($_[0]) };
       }
  } elsif($opts{out})  {
       $self->{__out} = $opts{out};
  } else {
       throw Error::Simple('HTML::Filter::Easy::new: required parameters dest or out are missing');
  }

  $opts{allowed_tags}          ||= $class->default_allowed_tags;
  $opts{allowed_tag_options}   ||= $class->default_allowed_tag_options;
  $opts{forbidden_tags}        ||= $class->default_forbidden_tags;
  $opts{forbidden_tag_options} ||= $class->default_forbidden_tag_options;

  $self->{__encode_high_entities} = defined ($opts{encode_high_entities}) ? $opts{encode_high_entities} : 1;

  $self->{__allowed}           = { map { $_ => 1 } ref $opts{allowed_tags}          eq 'ARRAY' ? @{$opts{allowed_tags}}         : split(/\s+/, $opts{allowed_tags}         )  } ;
  $self->{__allowed_options}   = { map { $_ => 1 } ref $opts{allowed_tag_options}   eq 'ARRAY' ? @{$opts{allowed_tag_options}}  : split(/\s+/, $opts{allowed_tag_options}  )  } ;
  $self->{__forbidden}         = { map { $_ => 1 } ref $opts{forbidden_tags}        eq 'ARRAY' ? @{$opts{forbidden_tags}}       : split(/\s+/, $opts{forbidden_tags}       )  } ;
  $self->{__forbidden_options} = { map { $_ => 1 } ref $opts{forbidden_tag_options} eq 'ARRAY' ? @{$opts{forbidden_tag_options}}: split(/\s+/, $opts{forbidden_tag_options})  } ;

  $self->{__delete_content}     = { map { $_ => 1 } ref $opts{delete_content}         eq 'ARRAY' ? @{$opts{delete_content}}        : split(/\s+/, $opts{delete_content} || $default_delete_content ) };  
  $self->{__delete_content_if_not_allowed} = { map { $_ => 1 } ref $opts{delete_content_if_not_allowed} eq 'ARRAY' ? @{$opts{delete_content_if_not_allowed}} : split(/\s+/, $opts{delete_content_if_not_allowed} || $default_delete_content_if_not_allowed ) };  

  $self->{__no_close}        = { map { $_ => 1 } ref $opts{no_close}            eq 'ARRAY' ? @{$opts{no_close}}           : split(/\s+/, $opts{no_close}    || $default_noclose     ) };
  $self->{__no_nest}         = { map { $_ => 1 } ref $opts{no_nest}             eq 'ARRAY' ? @{$opts{no_nest}}            : split(/\s+/, $opts{no_nest}     || $default_nonest     ) };
  $self->{__no_break}        = [                 ref $opts{no_break}            eq 'ARRAY' ? @{$opts{no_break}}           : split(/\s+/, $opts{no_break}    || $default_nobreak    ) ];

  $self->{__literal_tags}    = [                 ref $opts{literal_tags}        eq 'ARRAY' ? @{$opts{literal_tags}}       : split(/\s+/, $opts{literal_tags}|| $default_literal_tags )];

  $self->{__trim}            = $opts{trim};
  $self->{__length}          = $opts{length};
  $self->{__empty_lines}     = $opts{empty_lines};
  $self->{__separate_tail}   = $opts{return_tail_separately};                

  $self->{__allowed}           = {} if $self->{__allowed}          ->{none};
  $self->{__allowed_options}   = {} if $self->{__allowed_options}  ->{none};
  $self->{__forbidden}         = {} if $self->{__forbidden}        ->{none};
  $self->{__forbidden_options} = {} if $self->{__forbidden_options}->{none};
  $self->{__delete_content}       = {} if $self->{__delete_content}      ->{none};
  $self->{__delete_content_if_not_allowed} = {} if $self->{__delete_content_if_not_allowed}->{none};

  bless $self, $class ; 

  $self->__init( \%opts ) if $self->can('__init');  
  $self->{__pos} = 0;            # Position in the output stream

  return $self;

}

sub __out { 
  my ($self, $txt)=@_;
  $self->{__pos} += length($txt); 
  &{$self->{__out}} ($txt);  
}

sub xml_mode {
  $_[0]->{__parser}->xml_mode($_[1]);
}

sub AUTOTAG { 					# default tag  handler
  my $self = shift;
  return $self->__tag_text(@_);
}

sub __escape { 
  my $x = shift;
  $x =~ s/&/&amp;/gs;
  $x =~ s/"/&quot;/gs;
  $x =~ s/</&lt;/gs;
  $x =~ s/>/&gt;/gs;
  return $x;
}
sub __tag_text { 				# Create a text representation of a tag with options
  my ($self, $tag, $attr_hash, $attr_seq) = @_;

  my $tag_text = '<'.join(' ', $tag, map { 
     $attr_hash->{$_} eq $self->{__boolean_attribute_value}    
     ? $_ 
	 : qq!$_="!.__escape($attr_hash->{$_}).'"'
  } grep { $_ ne '/' } @$attr_seq ) .'>' ; 
  $tag_text =~ s/[\n\r]+/ /g;
  return $tag_text;
}


sub handle_text {                                    # default text handler : simply output the text
   my ($self,$outputter,$text,$empty_lines)=@_;
   if($empty_lines) { 
     $text =~ s/\n[\s\n]*\n/$empty_lines/g;
   }
   $outputter->($text);
}

sub handle_comment { 
   &{$_[1]} ($_[2]);
#   return;
}


sub __override_options {              # Override old options by the new ones, old options being in %$old and new ones in the string $new in the form "a b -hr"
  my ($old, $new)=@_; 		      #   which means  that opts a and b should be set, and hr should be reset.
  foreach (split(/\s+/, $new)) { 
    if ($_ eq 'none') { map { delete $old -> {$_} } keys %$old; } 
    if (substr($_,0,1) eq '-') { 
	delete $old->{substr($_,1)};
    } else { 
        $old->{$_} = 1;
    }  
  }
}

sub is_tag_allowed { 
  my ($self,$tag)=@_;
  my $cx = $self->{current_context};
  return ( $cx->{allowed_tags}->{$tag} || $cx->{allowed_tags}->{all}) && !($cx->{forbidden_tags}->{$tag} || $cx->{forbidden_tags}->{all})
}
sub is_close_tag_allowed { 
  my ($self,$tag,$n_opened)=@_;
  return $self->is_tag_allowed($tag) && $n_opened;
}

sub is_delete_content {
  my ($self,$tag)=@_;
  my $cx = $self->{current_context};
  return $cx->{delete_content}{$tag} || (!$self->is_tag_allowed($tag) && $cx->{delete_content_if_not_allowed}{$tag});
}

sub filter_tag_options {
  my ($self,$tag, $opts) = @_;
  my $cx = $self->{current_context};
  return grep { 
     ($cx->{allowed_tag_options}->{ $tag.':'.$_ } ||  
      $cx->{allowed_tag_options}->{ $tag.':all' } ||
      $cx->{allowed_tags}       ->{all}           ||
      $cx->{allowed_tag_options}->{all}
     ) && !(
      $cx->{forbidden_tag_options}->{ $tag.':'.$_ } ||  
      $cx->{forbidden_tag_options}->{all}
     ) } @$opts ; 
}

sub issue_warning($$$) {
# Pure virtual method!
}

sub parser_get_token {
  my $t = $_[0]->{__parser}->get_token;
  return $t;
}

sub process { 
  my ($self, %opts ) = @_;
  my %opened;
  my $blocktext = 0;
  my $accum_length = 0;                         
  my $tail_length  = 0;                           # The length of closing tags tail, if document will break just at current position.
  my $before_first_nonspace = 1;
  my $parser               = $self->{__parser};
  my $out                  = $opts{out}             || sub { my ( $txt) = @_; $self->__out($txt) } ; 
  my %no_close             = %{$self->{__no_close}};           __override_options (\%no_close            , $opts{no_close});
  my %no_nest              = %{$self->{__no_nest}};            __override_options (\%no_nest             , $opts{no_nest});
  my $literal_tags         = $self->{__literal_tags};
  my %allowed_tags         = %{$self->{__allowed}};            __override_options (\%allowed_tags        , $opts{allowed_tags}); 
  my %allowed_tag_options  = %{$self->{__allowed_options}};    __override_options (\%allowed_tag_options , $opts{allowed_tag_options}); 
  my %forbidden_tags       = %{$self->{__forbidden}};          __override_options (\%forbidden_tags        , $opts{forbidden_tags}); 
  my %forbidden_tag_options= %{$self->{__forbidden_options}};  __override_options (\%forbidden_tag_options , $opts{forbidden_tag_options}); 
  my %delete_content       = %{$self->{__delete_content}};     __override_options (\%delete_content         , $opts{delete_content}); 
  my %delete_content_if_not_allowed = %{$self->{__delete_content_if_not_allowed}};      
                                                               __override_options(\%delete_content_if_not_allowed, $opts{delete_content_if_not_allowed}); 
  my $length               = $opts{length}          || $self->{__length}        ;
  my $empty_lines          = $self->{__empty_lines};
  my $stop_if              = { map {$_=>1} split(/\s+/, $opts{stop_if})};
  my $stop_ifend           = { map {$_=>1} split(/\s+/, $opts{stop_ifend})};
  my $forbidden_close      = { map {$_=>1} split(/\s+/, $opts{forbidden_close})};

  my $stop_attheendof      = $opts{stop_attheendof};
  my $separate_tail        = exists $opts{separate_tail} ? $opts{separate_tail} : $self->{__separate_tail};
  
  my %do_not_close ;    # Если обработчик открывающего тэга попросил его не печатать, этот счетчик поможет нам не напечатать также и его закрывающий тэг

  my $token; 
  my $old_context = $self->{current_context};
  $self->{current_context} = { allowed_tags => \%allowed_tags, forbidden_tags=>\%forbidden_tags, allowed_tag_options=>\%allowed_tag_options, forbidden_tag_options=>\%forbidden_tag_options, delete_content => \%delete_content, delete_content_if_not_allowed => \%delete_content_if_not_allowed };
  my %context = (
    opened=>\%opened,
    out=> $out,
  );

   while ($token = $self->parser_get_token ) { 

##	print STDERR "Tok  ".join(',',keys(%allowed_tags))." $token->[0]:$token->[1]  attr=".(ref $token->[2]?join(",",map {"$_ => $token->[2]->{$_}"} keys %{$token->[2]}):'')." op=".join(',',grep {$opened{$_}} keys  %opened) ." \n";

        if ($token->[0] eq 'S') { 
            my (undef, $tag, $attr_hash, $attr_seq,$line) = @$token;
            $self->{current_line}    = $line;
            if ($stop_if->{$tag} || 
                ($no_nest{$tag} && $stop_attheendof && $tag eq $stop_attheendof)
            ) { 
                    $parser->unget_token($token);
                    last;
            }
            $blocktext++ if $self->is_delete_content($tag);  # Increment the delete_content semaphore if such a tag
            if (! $blocktext && $self->is_tag_allowed($tag) ) { 						          # If tag is to be output, 
                my @filtered_options = $self->filter_tag_options($tag,$attr_seq);
		        my $optproc = "options_${tag}";
         		$self->$optproc($attr_hash, \@filtered_options) if $self->can($optproc);
                my $tag_handler = "tag_$tag";
                my ($textrepr, $database_info) =                                                                   #   Find its text representation and, may be, additional data for a hypotetic database 
                	$self->can( $tag_handler )                                                                 #   if a tag handler routine is defined, 
		     		?  $self->$tag_handler( $attr_hash,       \@filtered_options,\%context)				   #     by calling it,
		     		:  $self-> AUTOTAG    ( $tag, $attr_hash, \@filtered_options,\%context);			   #     otherwise by calling default function
                unless (defined $textrepr) {                                                                       #   exit tag processing  if text representation is undef (probably tag handler did not allow to print the tag)
                	$do_not_close{$tag}++ unless $no_close{$tag};    
                }
                if ($textrepr eq '') {
                    next;
                }
											  			   #   Would the tag fit into the allowed document length ? 
                if ($no_nest{$tag} && $opened{$tag}>0 ) {  # Close the previous occurence of the same tag if necessary
		            last if (defined $length && $accum_length + 3 + length($tag) > $length );
         		    &$out("</$tag>");  
		            $opened{$tag}--; 
                    $accum_length += 3 + length($tag);
                }			
                my $new_tail_length = $tail_length  +  ($no_close{$tag} ? 0 :  (3 + length($tag)));
		my $tag_length = length(ref $textrepr ? $$textrepr: $textrepr);
    	        last if (defined $length   &&   $tag_length + $accum_length + $new_tail_length  > $length ) ;  #  no, would not: finish parsing
	        &$out (ref $textrepr ? $$textrepr: $textrepr ); 									   # yes: output text and update current lengths
		$accum_length += $tag_length;
                $tail_length   = $new_tail_length;
		$before_first_nonspace = 0 if $tag_length;
		my $commit_method = $tag.'_commit';								  # åÓÌÉ ÎÁÄÏ ÚÁÐÉÓÁÔØ ÒÅÚÕÌØÔÁÔ ÏÂÒÁÂÏÔËÉ ÔÜÇÁ × ÂÁÚÕ ÄÁÎÎÙÈ, ÓÄÅÌÁÔØ ÜÔÏ ÓÅÊÞÁÓ.
		if ( ! $self->can( $commit_method ) || 
                     ( $self->can( $commit_method ) && $self-> $commit_method ($database_info) )
                ) { 
    		  		$opened{$tag} ++;
                } 
	    } else {
				$self->issue_warning("Tag <$tag> erased","ôÅÇ <$tag> ÕÄÁÌÅÎ");
	    }
        }
        elsif ( $token->[0] eq 'E' ) { 
	    my $tag=$token->[1];
            $self->{current_line} = $token->[2];   
	    if ($stop_ifend->{$tag} || ($stop_attheendof && $tag eq $stop_attheendof && $opened{$tag}==0)) { 
            	     if($opts{with_end}) {    
                        &$out ("</$tag>");
                        $accum_length += 3 + length($tag);
                     } else { 
                        $parser->unget_token($token);
                     }
	       	     last;
	    }
            if (! $blocktext && $do_not_close{$tag}) { 
               $do_not_close{$tag}--;  next;
            }
## HARMFUL ?            next if(!$blocktext && $no_close{$tag});  # This is for the case when we replace a pair of opening/closing tags by one no-closed tag (eg. <tex>...</tex> => <img ...>)  --- Vadim
            if (! $blocktext && $self->is_close_tag_allowed($tag,$opened{$tag}) && ! $forbidden_close->{$tag}) {    
                &$out ("</$tag>"); 
                $accum_length += 3 + length($tag);
                $tail_length  -= 3 + length($tag) unless  $no_close{$tag};
            } else {
		if (!$self->is_close_tag_allowed($tag,$opened{$tag}) && $self->is_tag_allowed($tag) && !$opened{$tag}) {
                      $self->issue_warning("Closing tag </$tag> has not been opened before");
		}
                if (! $forbidden_close->{$tag}) {
	              $self->issue_warning("Closing tag </$tag> erased");
                }
	    }
	    $opened{$tag}-- if $opened{$tag} && !($blocktext && !$self->is_delete_content($tag)); 
            $blocktext--    if $blocktext && $self->is_delete_content($tag);
        }
	elsif ( $token->[0] eq 'T' && ! $blocktext) {
            my $text = $token->[1];
	    if ($self->{__trim} && $before_first_nonspace) { 
	       $text=~s/^\s+//g;
            }
	    $before_first_nonspace=0 if $text;
            my $text_length = length ( $text );
            if ( defined $length   &&  $text_length + $accum_length + $tail_length > $length ) {   # Does not fit in length
		  		 # Can the text be split here ?
                    unless (scalar (grep { $opened{$_}} @{$self->{__no_break}} )) { 
                         my $substr= substr  ( $text , 0, $length - $accum_length - $tail_length );
   
      	                 $substr=~ s/\s\S*$//os; # Посмотрим, не оборвано ли там слово - если фрагмент текста кончается на непробел - скорее всего - так и есть

			 $substr=~ s{([-/,.\s]|&nbsp;)+$}{}os;
                         $substr=~ s/\s+(ÎÁ|ÐÏÄ|ÎÅ|ÉÚ|×|Ó|\d+)\s*$//os;
		    	 $self->handle_text($out, $substr, $empty_lines);
	            }   #  Если текст в данном месте нельзя разрывать, вообще его не выдаем 
                    $self->{__overflow} = 1;
                    last;
            } else { 
                    # Если есть хоть один открытый "literal" тег - выводим текст без подстановок
                    if (grep { $opened{$_} } @$literal_tags) {
                         $self->HTML::Filter::Easy::handle_text($out, $text);
                    } else {
                         $self->handle_text($out, $text, $empty_lines);
                    }
                    $accum_length += $text_length ; 
            }
        }
	elsif ($token->[0] eq 'C') {
           if($self->handle_comment($out, $token->[1]))
	   {
                 $accum_length += length($token->[1]);
	   }
	}
    }  

   
    unless(defined($token)){     # HTML::PullParser::get_token returns undef at the EOF
# some eof processing...
    }
    my $tail = '';
		# Закроем тэги, которые забыли закрыть
    foreach my $tag ( grep {  $self->is_tag_allowed($_) && !$self->is_delete_content($_) && $opened{$_}>0 && !$no_close{$_} }  keys %opened ) {   
##print STDERR "Close forgotten tag $tag n= $opened{$tag}\n";
	if ($separate_tail) { 
		$tail.=  "</$tag>"   x $opened{$tag} ; 
        } else { 
	        &$out   ("</$tag>"   x $opened{$tag} ) ; 
        }
    }    
    $self->end_of_file if ! $stop_attheendof && $self->can('end_of_file');
    $self->{current_context} = $old_context;
    return $tail if  ($separate_tail) ;

}

sub __match_close { 
  my ($self, $tag)=@_;
  my $token = $self->{__parser}->get_token;
  unless($token->[0] eq 'E' && $token->[1] eq $tag) {
      $self->{__parser}->unget_token($token);
  }
}
sub was_overflow {
  return shift->{__overflow};
}
sub __content {
    my ($self, $tag, %opts ) = @_;
    my $content;
    $self->process( %opts ,   stop_attheendof => $tag, out => sub {$content.=$_[0]; } );
    return $content;
}

# Function which validates options
sub options_a {
  my ($self, $att_hash, $opt_sec) = @_;

  $att_hash->{href} =~ s/^\s+//;
  if($att_hash->{href} =~ m!^http://\w! || $att_hash->{href} =~ m!^mailto:!){
     $att_hash->{href} =~ s/\s*\n\s*//g;
     $att_hash->{href} =~ s/ /\%20/g;
  }
}


1;


__END__

=head1 NAME

HTML::Filter::Easy;  -   Base class for creating HTML filters for tag processing and length limiting.

=head1 SYNOPSIS

 use HTML::Filter::Easy;
 $p = HTML::Filter::Easy->new ( src =>  $file_or_doc, 
					 out =>  sub { print $_[0] }, 
                                         allowed_tags => 'a b br',
					 allowed_tag_options => 'a:href',
					 delete_content => 'script', 
					 no_close    => 'img br',
					 length      => 300,
					 empty_lines  => 1,
					 return_tail_separately => 1,
 );                                          


=head1 DESCRIPTION

=item   HTML::Filter::Easy->new(%opts)

Transforms HTML, using HTML::PullParser as input tool and user-defined output function for output.
While transforming, obeys the following rules:

- Only allowed HTML tags are passed.
- Only allowed options in these tags are passed
- Tags marked as C<delete_content> (there are defaults!) are not passed as well as their content
- Tags are automatically closed. It means that if no closing tag is found for some tag in the document, 
the closing tag would be  issued automatically in the end of the document. Tags marked as C<no_close> 
are not closed automatically.  Default no_close tags are C<br img hr>.
- The automatically issued closing tags are referred to as TAIL. 
This TAIL is output in the end of the document unless parser is created with C<return_tail_separately> option.
In this case, TAIL will be returned by the parser, not printed to output.
- If parser is  created with C<length> option, the text is cut so that the result length is no more than 
specified length, including TAIL.
- If parser is  created with C<empty_lines> option, the multiple empty lines in the document will be 
converted to double line breaks;

- A subroutine "end_of_file" is called after input stream ends

Has the following options:

src                  -- is passed to HTML::PullParser constructor (required)
out                  -- code reference to an output function (required)
allowed_tags         -- space separated list or a reference to array of allowed HTML tags. Only these tags will be passed by the parser. all and none are the special values that alllow or forbid  all the tags.
allowed_tag_options  -- space separated list or a reference to array of allowed HTML tag options in the form "$tag:$option", such as 'a:href' . Only these tag options will be passed by the parser. all and none can be used here.
delete_content          -- space separated list or a reference to array of HTML tags, content of which is not to be passed by the parser. Default is "form frameset frame". all and none can be used here.
delete_content_if_not_allowed -- space separated list or a reference to array of HTML tags, content of which is not to be passed by the parser if these tags are not allowed. Default is "script applet". all and none can be used here.
no_close             -- space separated list or a reference to array of HTML tags which should not be closed automatically.
length		     -- maximal allowed length of the output
empty_lines          -- if true, more than one empty line will be transformed to <br><br>
return_tail_separately -- if true, the automatically closing tags will be returned, not printed.
trim 		     -- if true, the leading whitespaces will not be output. This option is useful if you strictly limit the length and do not want your document to contain only leading whitespaces.


=head1 Subclassing guidelines

=item Initialization

If a subclass needs some initialization, define __init function in the subclass. It will be called by the constructor with the 
reference to the constructor options hash.

=item Tag handlers

To add any specific behaiviour to a tag, 
define in the subclass a subroutine named  tag_<tag name>. It will be called for any occurence of the tag
if it is allowed and is not inside a tag from C<delete_content>.

The arguments of tag handler are: 
  ($self, %$attr_hash, @$attrseq );

The handler should return the tag textual representation to be output, or undef if tag should not be output, 
or an array C<(textual representation, database_info)> (see explanation below).

If any user database should be updated by the tag handler, it is unwise to do this in the handler itself, 
because after the tag handler the document length is checked, and if the 
tag does not fit into the length, it would be not output.
So the database update should be done only after the length check, in a separate subroutine.
This subroutine should be named C<${tag}_commit>. 
The tag handler can return an array of C<(textual representation, database_info)>. This database info 
is to be passed to the commit subroutine.

==item AUTOTAG handler

The AUTOTAG handler is used to process the  tags which are not handled by named tag handlers.
The arguments of AUTOTAG are ($self, $tag, %$attr_hash, @$attrseq ).
The default implementation of AUTOTAG simply calls the __tag_text to return the tag textual representation.
You can overload it in subclasses to implement your own processing.
In this case,  it would be nice to call SUPER::AUTOTAG for the tags you do not have an idea what to do with.

=head1 SEE ALSO

L<HTML::PullParser>

=head1 AUTHOR

Ivan Panchenko <wao@mail.ru>

=cut

