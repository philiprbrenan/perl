#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Convert From Html to Dita
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2019
#-------------------------------------------------------------------------------
package Data::Edit::Xml::To::Dita::From::Html;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use utf8;
use feature qw(say current_sub);

sub acceptConversion($$$)                                                       # Decide whether to accept the conversion or not
 {my ($project, $x, $tags) = @_;                                                # Project, parse tree, tag counts
  return qw(cleanUpCutOutTopics convertDocument spelling);                       # Accept with modified versions of these routines if there is a html tag present
  ()                                                                            # Return empty list to reject
 }

sub clientSpecificConversionStart($$)                                           #r Called at the start of a conversion for a first look at the parse tree
 {my ($project, $X) = @_;                                                       # Project, parse tree
 }


sub convertDocument($$)                                                         # Convert an input xml document
 {my ($project, $X) = @_;                                                       # Project, parse tree
  my $projectName = $project->name;                                             # Project name

  my $styles =  $project->styles = &scanStyles($X);                             # Extract information about each style

  my $x = $X->go(qq(office:body));                                              # This where the tags of interest start

  clientSpecificConversionStart($project, $x);                                  # Scan parse tree for significant items before they are obliterated below

  my %changeTags =                                                              # Obvious tag transformations
   (qw(draw:image       image),
    qw(text:h           section),                                               # Points at which to create sections which will then be cut out and converted to topics
    qw(text:section     section),
    qw(text:a           xref),                                                  # Obvious changes
    qw(text:p           p),
    qw(table:table      table),
    qw(table:table-row  row),
    qw(table:table-cell entry));

  my %deleteTags = map {$_=>1}                                                  # Delete these tags if they have no content
   (qw(style:style text:outline-level-style),                                   # Fonts and exact text positioning
    qw(text:soft-page-break text:s text:tab),
    qw(text:alphabetical-index-mark),                                           # We should be able to generate a better index
   );

  my %deleteTagsRegardless = map {$_=>1}                                        # Delete these tags even if they do have content
   (qw(office:annotation office:annotation-end),                                # Annotations discovered in ACT files
   );

  my %unwrapTags = map {$_=>1}                                                  # Unwrap these tags
   (qw(text:sequence-decl text:sequence-decls text:soft-page-break text:span text:tab),
    qw(draw:frame draw:g draw:line draw:polygon draw:polyline draw:text-box),
   );

  my %deleteAttrs = map {$_=>1}                                                 # Delete these attributes
   (qw(office:value-type));

  $x->by(sub                                                                    # Choose cut out points - probably invalidating some change_tags above
   {my ($o, $p, $q, $r) = @_;
    my $t = $o->tag;
    if ($t =~ m(\A(text:h|text:section)\Z)is)
     {$o->change_section;
     }
    elsif ($t =~ m(\A(text:p)\Z)is)                                             # Paragraphs marked in style that indicate headings
     {my $s = -p $o;
      if (my $s = $o->attr(q(text:style-name)))
       {if ($styles->{ChapNum}{$s})
         {$o->change_section;
          if ($o->isOnlyChildToDepth(2, qw(section text:list-item text:list)))  # Unwrap containing list that is sometimes present to generate the numbering
           {$_->unwrap for $p, $q;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # Make some obvious changes before we store the parse tree for later reuse
   {my ($o) = @_;

    $o->deleteAttr($_) for keys %deleteAttrs;                                   # Remove superfluous attributes

    if ($o->at(qw(text:span)) and $o->isEmpty)                                  # The input documents contain individual words wrapped in text:span and separated not by blanks but by empty text::spans.
     {$o->replaceWithBlank;
     }
    elsif (my $c = $changeTags{$o->tag})                                        # Change obvious tags
     {$o->change($c);
     }
    elsif ($deleteTags{$o->tag} and $o->isEmpty)                                # Delete unneeded tags if they have no content
     {$o->cut;
     }
    elsif ($deleteTagsRegardless{$o->tag})                                      # Delete unneeded tags regardless of content
     {$o->cut;
     }
    elsif ($unwrapTags{$o->tag})                                                # Unwrap these tags
     {$o->unwrap
     }
   });

  $x->by(sub                                                                    # Remove empty p
   {my ($o) = @_;
    if ($o->isAllBlankText_p)
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # Normalize image names from downloads to out
   {my ($i) = @_;
    if ($i->at_image)                                                           # Image
     {if (my $h = $i->attr(q(xlink:href)))                                      # Image file name
       {my $source = absFromAbsPlusRel($project->source, $h);                   # Image source file
        my $o      = gbBinaryStandardCopyFile($source, &out);                   # Normalized file name
        $i->href   = fne $o;                                                    # Record image name - Xref will make relative
       }
     }
   });

  $project->moveIntoSections($x);                                               # Structure into topics

  my $bookTitle = extractBookTitle($project, $x);                               # Book title

  my $bookMeta = sub                                                            # Construct book change history from first table
   {my $t = $x->firstContextOf(qw(table));
    if ($t)
     {return $project->bookChangeHistory($t);
     }
    else
     {#warn "No book change history for project $projectName";
     }
    ''
   }->();

  my $bookMap = cutOutSectionsLeavingBookMap($project, $x);                     # Cut out the sections leaving the topicrefs

  $bookMap->by(sub                                                              # Cut out anything else that is not a topic ref
   {my ($o) = @_;
    if ($o != $bookMap and !$o->at(qw(topicref)))
     {$o->cut;
     }
   });

  $bookMap->by(sub                                                              # Convert outer topic refs to chapters
   {my ($o, $p) = @_;
    if ($o->at(qw(topicref)))
     {$o->change(qw(chapter)) if $p and !$p->at(qw(topicref));
     }
    else                                                                        # Remove everything that is not a topic ref - for some as yet unknown reason the above loop leaves three items behind - which needs explaining
     {$o->cut if $p;
     }

    if (my $h = $o->href)                                                       # All topics will be in the global area while the bookmap is in the local area for the project.
     {$o->set(href=>q(../).$h);
      $o->set(href=>$h);                                                        # According to Bill SDL cannot look up
     }
   });

  my $topicRefs = -p $bookMap;                                                  # Format the topicrefs as a string - they are currently wrapped in an office:body

  my $drh  =  sub                                                               # Revision history file
   {return q() unless my $d = $project->revisionHistory;
    <<END
   <chapter href="$d" navtitle="Document Revision History"/>
END
   }->();

  my $fm = <<END;                                                               # Front matter
 <frontmatter conref="bm_collection.dita#collection/frontmatter"/>
END
     $fm = '';                                                                  # PS2-536

  my $bm = Data::Edit::Xml::new(<<END);                                         # Create book map
<bookmap>
  $bookTitle
  $bookMeta$fm
  $topicRefs
  $drh
</bookmap>
END

  if (my $b = $bm->go(q(office:body)))                                          # Format the topicrefs as a string - they are currently wrapped in an office:body
   {$b->unwrap;
   }

  $bookMap->createGuidId;                                                       # Guid for book map

  clientSpecificCleanupBookMap($project, $bm);                                  # Clean up the bookmap

  $bookMap                                                                      # Return project
 } # convertDocument

sub scanStyles($)                                                               # Extract style information
 {my ($x) = @_;                                                                 # Entire parse tree
  my $styles;

  $x->by(sub                                                                    # Check that the headers are not in the table of contents
   {my ($o, $p) = @_;
    if ($o->at(qw(text:list-level-style-bullet text:list-style)))               # Bulleted lists
     {my ($level, $name) = ($o->attr(q(text:level)), $p->attr(q(style:name)));
      if ($level and $level == 1 and $name)
       {$styles->{bulletedList}{$name}++;
       }
     }
    elsif ($o->at(q(style:style)))
     {my $n = $o->attr(qw(style:name));
      my $p = $o->attrX(qw(style:parent-style-name));
      if ($n and $p =~ m(\AChapNum\Z)s)
       {$styles->{ChapNum}{$n}++;
       }
     }
   });
  $styles
 }

sub getHeadingLevel($)                                                          # Extract the heading level from a tag reference
 {my ($o) = @_;                                                                 # Node
  return undef unless $o->at(qq(section));
  $o->attr(qq(text:outline-level)) // 1;
 }

sub Project::moveIntoSections($$)                                               # Structure the document into sections
 {my ($project, $x) = @_;                                                       # Input project, parse tree

  clientSpecificFilterSections($project, $x);                                   # Filter sections

  $x->by(sub                                                                    # Move each section definition upwards out of any containing list being used to number it
   {my ($o) = @_;
    if ($o->at_section)
     {my @p;
      for(my $p = $o->parent; $p; $p = $p->parent)
       {last unless $p->tag =~ m(\Atext:list)s;                                 # The headings after on embedded in lists to number them
        push @p, $p;
       }
      $_->unwrap for @p;
     }
   });

  $x->by(sub                                                                    # Place each non heading node in its corresponding heading node
   {my ($o) = @_;
    if (my $h = getHeadingLevel($o))
     {while(my $n = $o->next)
       {last if getHeadingLevel($n);
        $n->cut;
        $o->putLast($n);
       }
     }
   });

  $x->by(sub                                                                    # Place each sub section in its containing section
   {my ($o) = @_;
    if (my $h = getHeadingLevel($o))
     {while(my $n = $o->next)
       {my $i = getHeadingLevel($n);
        last if !defined($i) or $i <= $h;
        $o->putLast($n->cut);
       }
     }
   });

  $x->by(sub                                                                    # Unwrap section headers use solely for wrapping link targets
   {my ($o) = @_;
    if ($o->at_section)
     {my $c = $o->countTagNames(qw(section text:bookmark text:bookmark-start text:bookmark-end CDATA));

      for my $k(keys  %$c)                                                      # To small for a section - heuristic
       {next if $$c{$k} > 1;
        delete $$c{$k};
       }

      if (!keys %$c)                                                            # Unwrap sections deemed too small
       {$o->unwrap;
       }
     }
   });

  owf(swapFilePrefix($project->source, &in, &topicTrees), -p $x) if topicTrees; # Write topic trees if requested
 }  # moveIntoSections

sub deTag($)                                                                    # Remove any tags from a string
 {my ($string) = @_;                                                            # String to detag
  $string =~ s(<[^>]*?>) ()gr
 }

sub Project::getTitleForSection($$)                                             # Get the title for a section
 {my ($project, $x) = @_;                                                       # Project, parse tree starting at section

  if (my $t = $x->attr(qq(text:name)))                                          # Title on attribute
   {return deTag($t);
   }

  my @t;
  for my $o(@$x)                                                                # Items immediately under section
   {if (my $t = $o->isText)
     {my $s = $t->text;
      if ($s =~ m(\A\s*(NextGen HQM Data Assumptions)\s*(The following list.*)\Z)is) # Phrases that do not seem to be correctly broken
       {push @t, $1;                                                            # Title
        $t->text = $2;                                                          # Remaining text
        last;
       }
      elsif ($s =~ m(\A\s*For more information on HQM)is)
       {last;
       }
      else                                                                      # accumulate title
       {push @t, $t->text;
        $t->cut;
       }
     }
    elsif ($o->at(q(text:bookmark-end)))                                        # Sometimes the indicator that we have finished the title
     {if (my $s = deTag(join " ", @t))
       {if (length($s) > 8)                                                     # Guess!
         {return $s;
         }
       }
     }
   }

  deTag(join " ", @t);                                                          # Title minus any embedded tags
 }

sub allowConversionToTask($$$)                                                  #r Return true to allow conversion to task to be attempted, return false to prevent conversion to task.
 {my ($project, $x, $title) = @_;                                               # Project, parse, title
  1                                                                             # Allow conversion to task where possible by default
 }

sub cutOutSectionsLeavingBookMap($$)                                            #r Cut out and save the sections
 {my ($project, $x) = @_;                                                       # Project, parse
  my $styles        = $project->styles;

  $x->by(sub                                                                    # Cut out and save each section
   {my ($o) = @_;
    if ($o->at(qq(section)))
     {my $topicRef = $o->wrapWith(qq(topicref));                                # Topic ref

      $o->by(sub                                                                # Unfortunately the topic refs are occasionally buried so we have to go and pull them out.
       {my ($t) = @_;
        if ($t->at_topicref)
         {if (my $T = $t->parent)
           {if (!$T->at_topicref)                                               # Make sure we are on the highest topicref
             {$topicRef->putLastCut($t);
             }
           }
         }
       });

      my $title  = $project->getTitleForSection($o);                            # Get the title of the piece

      my ($topicPrefix, $topicType, $topicBody) = (qw(c concept conbody));      # Decide on a topic type for section

      $topicRef->setAttr(navtitle=>                                             # Set nav title for topic ref. AM87 Remove topic type from title
       $title =~ s(\A(concept|principle|procedure|\[Sub-Topic\])\s*:?\s*) ()isr);

      $o->change($topicType);                                                   # We have to format it as something, so try a concept first
      $o->wrapContentWith($topicBody);                                          # Push the existing content into the body

      if (my $t = $o->newTag(q(title)))
       {$o->putFirst($t);                                                       # EO54: add title without id
        $t->putFirstAsText($title);
       }

      $project->cleanUpCutOutTopic($o);                                         # Clean up the newly cut out topic

      $o->moveAttrs($topicRef, qw(audience props));                             # Transfer audience and props attributes to bookmap topic

      if (allowConversionToTask($project, $x, $title))                          # Convert to task unless prevented by title or content
       {if ($o->ditaConvertConceptToTask)                                       # Convert task if possible
         {$project->cleanUpCutOutTask($o);
         }
       }

      $o->cut;                                                                  # Cut out referenced section
      $o->createGuidId;                                                         # Give the topic a guid representing the content of the topic as an id

      my $source = sub                                                          # Create output string
       {my $s = $o->ditaPrettyPrintWithHeaders;                                 # Format string
        &spelling($s);                                                          # Improve spelling
       }->();

      my $file = gbStandardFileName($source, outExtTopic);                      # Standardized name for topic file
      my $path = fpf(&out, $file);                                              # Full path name to topic
      $topicRef->setAttr(href=>$file);                                          # Bookmap entry
      $project->titleToFile->{$title} = [$file, $o->id];                        # Map title to [file, id]

      my $l = $project->linter;                                                 # Linter for new file
      $l->file     = $path;                                                     # File for topic
      $l->source   = $source;
      $l->labels   = $o;                                                        # Add label information to the output file so when all the files are written they can be retargeted by Data::Edit::Xml::Lint
      $l->ditaType = -t $o;
      $l->guid     = $o->id;
      $l->title    = $title;
      $l->lint;                                                                 # Lint topic
      push @{$project->titleFiles}, [$o->tag, length($source), $title, $file];  # Title list

     }
   });

  $x->by(sub                                                                    # Clean up the remaining bookmap
   {my ($o, $p) = @_;
    return unless $p;
    my %unwrap = map {$_=>1} qw(office:text office:body);
    my %cut    = map {$_=>1} qw(office:forms text:sequence-decls);

    my $t = $o->tag;
    $o->unwrap if $unwrap{$t};
    $o->cut    if $cut{$t};
   });

  $x                                                                            # The residue will be the book map
 } # cutOutSectionsLeavingBookMap

sub extractDate($)                                                              # Extract a date
 {my ($date) = @_;
  return ('', '', '') unless $date;

  my (undef, undef, undef, $day, $month0, $year1900) = strptime($date);
  my $month = !defined($month0) ? undef :
   [qw(January February March April May June July August
       September October November December)]->[$month0];

  my $year = defined($year1900) ? $year1900 + 1900 : undef;

  unless($year)
   {#warn "Unable to convert date $date because it lacks a year";
    return ('', '', '');
   }

  ($year, $month, $day)
 }

sub extractBookTitle($$)                                                        # Extract book title
 {my ($project, $x) = @_;                                                       # Project, parse
  my $projectName = $project->name;

  my @t;                                                                        # Titles

  $x->by(sub                                                                    # Warn if there is an image in the title
   {my ($o, $p, $q, $r, $s) = @_;

    if ($o->at(qw(image office:binary-data draw:image
                  draw:frame p office:text)))
     {$s->cut;
      if (my $h = $o->href)
       {lll "Image $h specified on title page";
       }
      else
       {lll "No href= on image:\n".$r->prettyString;
       }
     }

    $o->unwrap if $o->at(qw(text:keywords p office:text));                      # Various items left over in the book map
    $o->unwrap if $o->at(qw(text:subject  p office:text));

    if ($o->at(qw(text:bookmark)) or
        $o->at(qw(text:bookmark-start)) or
        $o->at(qw(text:bookmark-end)))
     {$o->addConditions(qw(notBookMap));
     }

    $o->cut if $o->at(qw(draw:line));                                           # The position of this line marks the end of the alt book titles and the start of a disclosure which should be a topic
    if ($o->at(qw(p office:text)))
     {if (!$o->isEmpty)
       {push @t, $o->stringContent;
       }
     }
   });

  if (@t == 0)                                                                  # Warn if there is no title
   {ddd "No Title for  $projectName";
    return '';
   }

  for(@t)                                                                       # Edit titles so they do not contain xml and are not too long
   {s(<.+?>) ()gs;                                                              # Remove xml
    s(&)     ( and )gs;
    $_ = substr($_, 0, 80)  if length($_) > 80
   }

#  my $titles = sub
#   {my $t = join "\n", map {"<mainbooktitle>$_</mainbooktitle>"} (shift @t);   # Construct titles - nextgen
#    my $a = join "\n", map {"<booktitlealt><ph>$_</ph></booktitlealt>"} @t;
#    qq($t\n$a)
#   }->();

  my $titles = sub
   {join "\n", q(<mainbooktitle>), @t, q(</mainbooktitle>);                     # Construct titles - pure
   }->();

  my $X = Data::Edit::Xml::new(spelling(<<END));
<booktitle>
$titles
</booktitle>
END

  $X->by(sub {$_->cut if $_->tag =~ /\Atext:/});                                # Remove bookmarks from the cloned bookmap

  $X->prettyString;
 } # bookTitle

sub Project::bookChangeHistory($$)                                              # Extract book change history from residue
 {my ($project, $x) = @_;                                                       # Project, parse

  my @r = ({});                                                                 # Revisions

  $x->by(sub                                                                    # Cut out and save each section
   {my ($o, $p) = @_;

    if ($o->at(qw(p entry row table)))
     {if   ($p->attr(qq(table:style-name)) =~ /Table.\.([A-E])/)                # Extract fields from each row
       {my $column =
         (qw(revisionid completed data1 summary data2))[index("ABCDE", $1)];
        my $c = $o->stringContent;
           $c =~ s(<[^>]*>) ()gs;                                               # Remove embedded tags
        $r[-1]->{$column} = $c;
       }
     }
    elsif ($o->at(qw(row table))) {push @r, {}}                                 # Start next row
   });

  my @t;                                                                        # Text of edits
  for(@r)
   {my %t = %$_;
    my ($year, $month, $day) = extractDate($t{completed});
    my    ($summary, $revisionid, $data1, $data2) =
      @t{qw(summary   revisionid   data1   data2)};

    if ($revisionid or $day or $month or $year or $summary or $data1 or $data2)
     {my $t = '';
      $t .= "<edited>\n";
      $t .= "  <revisionid>$revisionid</revisionid>\n" if $revisionid;
      if ($day or $month or $year)
       {$t .= "  <completed>\n";
        $t .= "    <month>$month</month>\n"   if $month;
        $t .= "    <day>$day</day>\n"         if $day;
        $t .= "    <year>$year</year>\n"      if $year;
        $t .= "  </completed>\n";
       }
      $t .= "  <summary>$summary</summary>\n" if $summary;
      $t .= "  <data>$data1</data>\n"         if $data1;
      $t .= "  <data>$data2</data>\n"         if $data2;
      $t .= "</edited>\n";
      push @t, $t;
     }
   }

  my ($bm, $bch) = qw(bookmeta bookchangehistory);                              # Book meta
  join "\n", "<$bm><$bch>", @t, "</$bch></$bm>";
 } # bookChangeHistory


sub Project::chooseListType($$)                                                 # Choose list type for a text:list node
 {my ($project, $o) = @_;                                                       # Project, node
  my $styles        = $project->styles;                                         # Styles for this project
  if (my $styleName = $o->attr(q(text:style-name)))                             # Style name
   {return q(ul) if $styles->{bulletedList}{$styleName};                        # Bulleted list goes to ul
   }
  return q(ol);                                                                 # ol by default
 }

sub Project::cleanUpCutOutTopic($$)                                             # Clean up a topic after it has been cut out
 {my ($project, $x) = @_;                                                       # Project, parse tree

  $x->by(sub                                                                    # NEX135 at 2019.01.03 01:15:24
   {my ($t, $l) = @_;
    if ($t->at(q(text:list-item)))
     {$t->change_li;
#     $t->ditaMergeLists;
     }
   });

  $x->by(sub                                                                    # NEX150 - Unwrap singleton lists at 2019.01.09 17:46:54
   {my ($l, $o) = @_;
    if ($l->isOnlyChild(q(li), qr(\A(ol|ul)\Z)))
     {$_->unwrap for $l, $o;
     }
   });

  $x->by(sub                                                                    # PLEX11 at 2018.12.17 19:07:51
   {my ($c, $t) = @_;
    if ($c->at_context_taskbody)
     {if (my $s = $t->firstIn(qw(steps steps-unordered)))
       {for my $p($c->contentAfter)
         {last if $p == $s;
          $c->putLast($p->cut);
         }
       }
     }
   });

  $x->by(sub                                                                    # Remove table of contents at 2018.12.11 20:44:38
   {if (my ($t) = @_)
     {if ($t->at(qr(\A(text:table-of-content|text:illustration-index|draw:contour-polygon)\Z)i))  # Draw contour removed to simplify following transformations
       {$t->cut;
       }
     }
   });

  $x->by(sub                                                                    # Table
   {my ($o) = @_;

    if ($o->at(qw(table)))
     {my $g = $o->wrapContentWith(qw(tgroup));
      my $b = $g->wrapContentWith(qw(tbody));
      $g->setAttr(qw(cols 99));
     }
   });

  $x->by(sub                                                                    # table:table-header-rows
   {my ($o, $p) = @_;

    if ($o->at(qw(table:table-header-rows tbody tgroup table)))
     {$p->putPrev($o->change(qw(thead))->cut);                                  # make into thead
     }
   });

  $x->by(sub                                                                    # text:list
   {my ($o) = @_;
    if    ($o->at(qw(text:list-item text:list))) {$o->change(qq(li))}           # List item
    elsif ($o->at(qw(               text:list)))                                # List
     {$o->change($project->chooseListType($o));                                 # List type
     }
   });

  $x->by(sub                                                                    # text:list-item under conbody - this needs investigating, see line: 82 of /home/phil/wordToDita/out/chase/dita/c__0001.dita
   {my ($o) = @_;
    $o->unwrap          if $o->at(qw(text:list-item conbody));
    $o->wrapWith(qq(p)) if $o->at(qw(CDATA conbody));
   });

  $x->by(sub                                                                    # Foot note from text:note
   {my ($o) = @_;
    if ($o->at(qw(text:note-citation text:note))) {$o->cut}
    elsif ($o->at(qw(text:note-body text:note)))  {$o->unwrap}
    elsif ($o->at(qw(text:note)))                 {$o->change(qq(fn))}
   });

  $x->by(sub                                                                    # Images
   {my ($o, $p) = @_;
    if ($o->at(qw(image)))
     {$p->unwrap if $p->at(qw(office:binary-data));                             # Image under office:binary-data
      if (my $n = $o->next)
       {if ($n->at(qq(svg:desc)))
         {my $f = $o->wrapWith(qq(fig))->putFirst($n->change(qq(desc))->cut);
          $f->createGuidId unless $f->id;                                       # Give the figure an id
         }
       }
     }
   });

  $x->by(sub                                                                    # Adjust column count on tables
   {my ($o, $p, $q) = @_;
    if ($o->at(qw(tbody tgroup table)))
     {my @c;
      $o->by(sub                                                                # Count number of rows in each entry
       {my ($O) = @_;
        for my $r($O->c(qw(row)))
         {push @c, scalar($r->c(qw(entry)));
         }
       });
      if (@c)
       {my ($c) = sort {$b <=> $a} grep {defined $_} @c;
        $p->setAttr(cols=>$c);
       }
      else
       {ddd "tbody with no columns". $o->prettyString;
       }
     }
   });

  $x->by(sub                                                                    # Bookmarks
   {my ($o, $p, $q) = @_;
    if ($o->at(qw(text:bookmark)) or                                            # Bookmark
        $o->at(qw(text:bookmark-start)))                                        # Corresponding end tag has same label which produces duplicate definitions
     {$p or eee "No context for bookmark";                                      # Unlikely
      my $b = $o->attr(qw(text:name));
      if ($p->at(qw(conbody  concept)) or                                       # Set the bookmark on the containing concept
          $p->at(qw(taskbody task))    or
          $p->at(qw(refbody  reference)))
       {$q->createGuidId unless $q->id;
        $q->addLabels($b);
       }
      elsif ($p->at(qw(p)))                                                     # Set the bookmark on the containing paragraph
       {my $label = $p;
        if ($o->isOnlyChild and $o->isAllBlankText and $q)                      # Likely to be unwrapped so go up a level
         {$label = $q;
         }
        if ($label->id)                                                         # Add the bookmark name either as a label or as an id
         {$label->addLabels($b);
         }
        else
         {$label->id = $b;
         }
       }
      else
       {#lll "Bookmark in unexpected context ". $o->context;
       }
     }

    if ($o->at(qw(xref)))                                                       # xref
     {if (my $h = $o->attr(qw(xlink:href)))                                     # xref href keyword
       {$o->changeAttr(qw(xlink:href href));
        if (my $f = $o->containsSingleText)                                     # Merge text of identical sequential xrefs
         {if (my $p = $o->prev)
           {if ($p->at(qw(xref)))
             {if (my $F = $p->containsSingleText)
               {$F->text = trim($F->text).trim($f->text);
                $o->cut;
               }
             }
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # Bulleted list to <ul>
   {if ($_->at(qw(p)) and my $t = $_->containsSingleText)                       # <p> with single text
     {if ($t->text =~ m/\A\x{2022}/s)                                           # Starting with a bullet
       {$t->text =~ s(\A.\s*)()s;                                               # Remove bullet and leading spaces
        $_->change(qw(li));                                                     # <p> to <li>
        if (my $p = $_->prev)                                                   # Previous element
         {$p->at(qw(ul)) ? $p->putLast($_->cut) : $_->wrapWith(qw(ul));         # Put in preceding list or create a new list
         }
       }
     }
   });

  $x->by(sub                                                                    # Convert: text:bookmark-ref to xref
   {my ($o) = @_;
    if ($o->at(qw(text:bookmark-ref)))
     {$o->change(q(xref));
      my $ref = $o->attr(q(text:ref-name));
      $o->href = '#'.$ref;
     }
   });

  my %unwrapTag = map {$_=>1}                                                   # Unwrap these tags
    qw(text:span),
    qw(draw:frame draw:object-ole draw:image text:soft-page-break),
    qw(field:fieldmark-start field:fieldmark-end  text:sequence),               # From concept
    qw(text:line-break text:s text:tab),

    qw(text:bookmark),                                                          # A point bookmark
    qw(text:bookmark-start),                                                    # Start/end of bookmark segment of text
    qw(text:bookmark-end),
    qw(text:bookmark-ref),

    qw(table:table-column table:covered-table-cell);

  if (1)                                                                        # Execute changes specified above
   {my @u;
    $x->by(sub
     {my ($o) = @_;
      if ($unwrapTag{$o->tag})
       {push @u, $o;                                                            # Unwrap specified tags
       }
      else
       {$o->changeAttr(qw(text:style-name outputclass));                        # Change the style name to outputclass if possible

        for(keys %{$o->attributes})                                             # Delete attributes with :
         {$o->deleteAttr($_) if m/:/;
         }
       }
     });
    $_->unwrap for @u;
   }

  $x->by(sub                                                                    # Embed mathML
   {my ($o, $p) = @_;
    if ($o->at(qw(math draw:object)))
     {$p->unwrap;
      $o->change(qw(mathml));
     }
   });

  $x->by(sub{if ($_ <= [qw(p p)]){$_++}});                                      # p\p unwrap inner p

  $x->by(sub                                                                    # table-p\refbody with p empty => remove p
   {my ($o) = @_;
    if ($o->isAllBlankText(qw(p)))
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # Place everything inside a refbody in a section
   {my ($o) = @_;
    if ($o->at(q(refbody)) and !$o->over(qr(\A(data | sort-as | data-about | example | foreign | svg-container | mathml | unknown | refbodydiv | refsyn | properties | section | simpletable | table)\Z)x))
     {$o->wrapContentWith(qw(section));
     }
   });

  $x->by(sub                                                                    # PLEX12 Important to note
   {my ($o, $p) = @_;
    if ($o->matchesText(qr(\A\s*IMPORTANT\s*[!:]?)i, qw(p)))
     {$o->text =~ s(\A\s*IMPORTANT\s*[!:]?\s*) ()igs;
      $p->change(q(note));
      $p->type = q(important);
      if (my $q = $p->parent)
       {if ($q->at_taskbody or $q->at_conbody)
         {$p->wrapWith_p;
         }
       }
     }
   });

  $x->by(sub                                                                    # First p in task body becomes context if it ends with :
   {my ($o, $p, $q) = @_;
    if ($o->isFirst(qw(p taskbody)) and $o->stringContent =~ m(:\s*\Z)s)
     {$o->wrapWith(q(context));
     }
   });

  $x->by(sub                                                                    # ol in task body becomes steps
   {my ($o, $p) = @_;
    if    ($o->at(qw(p li ol taskbody)))
     {$o->change(q(cmd));
     }
    elsif ($o->at(qw(  li ol taskbody)))
     {$o->change(q(step));
     }
    elsif ($o->at(qw(     ol taskbody)))
     {$o->change(q(steps));
     }
   });

  $x->by(sub                                                                    # p in task body after steps becomes result
   {my ($o, $p) = @_;
    if    ($o->at(qw(p taskbody)) and $o->prev(qw(steps)))
     {$o->change(q(result));
      if (my $n = $o->next(qw(note)))
       {$o->putLast($n->cut);
       }
     }
   });

  $x->by(sub                                                                    # p in task body after steps becomes result
   {my ($o, $p) = @_;
    if ($o->at(qw(taskbody)) and $o->over(qr(\A(context|p)( p)+\Z)))
     {my $P = $o->go(qw(p));
      my $Q = $o->go(qw(p -1));
      my $S = $P->wrapTo($Q, q(steps));
      $S->by(sub                                                                # p in task body after steps becomes result
       {my ($O) = @_;
        if ($O->at(qw(p steps)))
         {$O->change(q(cmd));
          $O->wrapWith(q(step));
         }
       });
     }
    elsif ($o->isOnlyChild(qw(p taskbody)))
     {$o->change(q(cmd))->wrapUp(qw(step steps));
     }
   });

  $x x= sub                                                                     # taskbody/p+ to taskbody/context
   {my ($o) = @_;
    if ($o->at(qw(taskbody)))
     {if ($o->over2(qr(\A(\sp\s)?)))
       {my $c = $o->newTag(q(context));
        for my $p($o->contents)
         {if ($p->at(q(p)))
           {$c->putLast($p->cut);
           }
          else {last}
         }
        $o->putFirst($c);
       }
      if ($o->over(qr(\sresult\sp\sul\Z)))                                      # p,ul after result
       {if (my $r = $o->go(qw(result -1)))
         {for my $p($r->next->from)
           {$r->putLast($p->cut);
           }
         }
       }
     }
   };

  $x->byReverse(sub
   {my ($o) = @_;
    if ($o->at(qw(context taskbody)) and $o->isAllBlankText)                    # Empty context
     {$o->cut;
     }
    if ($o->at(qw(ul taskbody)) and $o->prev(q(context)))                       # taskbody(context, ul, ul ...)
     {if (my $N = $o->next(q(ul)))
       {$o->putLast($N->cut);
        $N->unwrap;
       }
      $o->convertListToSteps;
     }
    if ($o->at(qw(steps taskbody)))                                             # Merge: steps, result, step - has to be done in reverse order to avoid early changes blocking later recognition
     {if ($o->matchAfter(qr(result\ssteps)))
       {my (undef, $r, $s) = $o->from;
        $o->last->putLast($r->change(q(stepresult))->cut);
        $o->putLast($s->cut);
        $s->unwrap;
       }
      if ($o->matchAfter2(qr((\s(p|ul)\s)+\Z)))                                 # Mixture of p,ul after steps -> result
       {$o->next->wrapTo($o->lastSibling, q(result));
       }
     }
   });

  $x x= sub                                                                     # taskbody/p+ to taskbody/context
   {my ($o, $p, $q) = @_;
    if ($o->at(qw(result result)))                                              # Result/result
     {$o->unwrap;
     }
    if ($o->at(qw(ol step)) and my $C = $o->prev)                               # step(cmd, ol)
     {$o->convertListToSteps(q(substep));
     }
    if ($o->at(qw(fig cmd step)) and my $S = $q->prev)                          # fig\cmd\step
     {$S->putLast($p->change(q(stepresult))->cut);
      $q->cut;
     }
   };

  $x x= sub                                                                     # As a result of AM-39 one topic has p\steps, convert each to cmd\step
   {my ($para, $steps) = @_;
    if ($para->at(qw(p steps)))
     {my $step = $para->wrapWith(q(step));
      $para->change(q(cmd));
     }
   };

  $x->by(sub                                                                    # AM66 - Unordered steps
   {my ($o, $p) = @_;
    if ($o->at(qw(steps taskbody)))
     {if (my $O = $o->outputclass)
       {if ($project->styles->{bulletedList}{$O})
         {$o->change(q(steps-unordered));
          $o->crc(66);
         }
       }
     }
   });

  $x->by(sub                                                                    # AM67 - Note
   {my ($o, $p, $q) = @_;
    if ($o->matchesText(qr(\A\s*Note:), qw(p)))
     {$p->change(q(note));
      $p->wrapContentWith(q(p));
      $o->text =~ s(\A\s*Note:\s*) ();
     }
    elsif ($o->matchesText(qr(\A\s*Note:)) and $p->tag !~ m(\A(cmd)\Z)s)        # AM92 More notes but not under cmd
     {my $para = $o->wrapWith(q(p));
      my $note = $para->wrapWith(q(note));
      $o->text =~ s(\A\s*Note:\s*) ();
     }
   });

  $x->by(sub                                                                    # AM92, AM96 - As a result of bolding more text some notes are now wrapped in <b> which can be removed
   {my ($o, $p) = @_;
    if ($o->at(qw(note b)))
     {$p->unwrap;
     }
   });

  $x->by(sub
   {my ($note, $cmd, $step) = @_;
    if ($note->at(qw(note cmd step)))                                           # AM92 Note in cmd becomes info/note in previous step
     {$cmd->change(q(info));
      if (my $s = $step->prev)
       {$s->putLast($step->cut);
        $step->unwrap;
        $note->crc(q(AM92));
       }
     }
   });

  $x->by(sub                                                                    # AM69 »» to steps when it appears as an info list
   {my ($t, $li, $ol, $info, $step, $steps) = @_;
    if ($t->matchesText(qr(\A\s*»»), qw(li ol info step steps)))
     {my $Cmd = $li->change(q(cmd));
      my $Step = $Cmd->wrapWith(q(step));
      $Cmd->putNext(my $Info = $Cmd->newTag(qw(info outputclass verbal)));
      $Info->putFirst(my $Ol = $Cmd->newTag(qw(ol)));
      for my $n($Step->contentAfter)
       {last if $n->stringContent =~ m(\A\s*»»)s;
        $Ol->putLast($n->cut);
       }
      $steps->putLast($Step->cut);
      $Step->crc(q(AM69a li consolidated));
     }
   });

  $x->by(sub                                                                    # AM69 »» to consolidated  steps
   {my ($t, $cmd, $step) = @_;
    if ($t->matchesText(qr(\A\s*»»), qw(cmd step)))
     {my @steps;
      for(my $nStep = $step->next; $nStep; $nStep = $nStep->next)
       {if (my $nCmd = $nStep->first(q(cmd)))
         {if ($nCmd->firstText)
           {my $text = $nCmd->first->text;
            last if $text =~ qr(\A\s*»»);
            $nCmd->change(q(li));
            if (my $info = $cmd->next(qw(info)))
             {if (my $ol = $info->last)
               {$ol->putLast($nCmd->cut);
               }
              else
               {my $ol = $info->newTag(qw(ol));
                $info->putLast($ol);
                $ol->putLast($nCmd->cut);
               }
             }
            else
             {my $info = $cmd->newTag(qw(info));
              $cmd ->putNext($info);
              my $ol   = $info->newTag(qw(ol));
              $info->putLast($ol);
              $ol->putLast($nCmd->cut);
             }
            push @steps, $nStep;
           }
         }
       }
      $step->crc(q(AM69b Steps consolidated)) if @steps;
      $_->unwrap for @steps;
     }
   });

  $x->by(sub                                                                    # AM69c »» remove leading spaces
   {my ($t, $p) = @_;
    if ($t->isText and $t->text =~ m(\A\s+»»))
     {$t->text =~ s(\A\s+) ();
      $t->crc(q(AM69c leading whitespace removed));
     }
   });

  $x->by(sub                                                                    # AM74 table header
   {my ($r, $b) = @_;
    if ($r->at(qw(row tbody tgroup table)))
     {if ($r->id and $r->id eq q(9_225))
       {$b->putPrevAsText(<<END);
        <colspec colname="c1" colnum="1"/>
        <colspec colname="c2" colnum="2"/>
        <colspec colname="c3" colnum="3"/>
        <thead>
          <row id="9_225">
            <entry id="9_226" namest="c1" nameend="c2">
              <p id="9_227" outputclass="P12">Number of Examinees</p>
            </entry>
            <entry id="9_229" morerows="1">
              <p id="9_230" outputclass="P12">Proctors Needed</p>
            </entry>
          </row>
          <row id="9_231">
            <entry id="9_232">
              <p id="9_233" outputclass="P12">Standard Time</p>
            </entry>
            <entry id="9_234">
              <p id="9_235" outputclass="P12"> Accommodations/Supports</p>
            </entry>
          </row>
        </thead>
END
        $r->next->cut;
        $r->cut;
       }
     }
   });

  $x->by(sub                                                                    # AM74 table header
   {my ($c, $s) = @_;
    if ($c->at(qw(cmd step)))
     {if ($c->id and $c->id eq q(2_11))
       {$s->deleteAttr(q(administration));
       }
     }
   });

  $x->by(sub                                                                    # AM76 Bold content
   {my ($t, $p) = @_;
    if ($t->isText(qw(p)) and $t->text =~
                  m(\A\s*Number\s+required:\s*)s)
     {$t->text =~ s(\A\s*Number\s+required:\s*) (<b>Number required:</b> )gs;
      $p->crc(q(AM76));
     }
    if ($t->isText(qw(p)) and $t->text =~                                       # AM82 super
                  m(PearsonAccessnext))
     {$t->text =~ s(PearsonAccessnext) (PearsonAccess<sup>next</sup>)gs;
      $p->crc(q(AM82));
     }
   });

  $x->by(sub                                                                    # AM83 Merge list
   {my ($u) = @_;
    if ($u->at(qw(ul)) and $u->id and $u->id eq q(9_311))
     {if (my $p = $u->prev(qw(ul)))
       {$p->putLast($u->cut);
        $u->unwrap;
       }
     }
   });

  $x->by(sub                                                                    # AM84 steps-ul-note-steps
   {my ($ul, $taskBody) = @_;
    if ($ul->at(qw(ul taskbody)) and
        $taskBody->over2(qr( context  steps  ul  note  steps  result )))
     {my ($context, $steps, $ul2, $note, $steps2, $result) = $taskBody->contents;
#     $ul == $ul2 or confess ;
      my $stepInfo = $ul->wrapWith(qw(info));
      $ul->putNext($note->cut);
      $steps->last->putLast($stepInfo->cut);
      $steps->putLast($steps2->cut);
      $steps2->unwrap;
     }
   });

  $x->by(sub                                                                    # AM93, AM97 remove outputclass=standard
   {$_->deleteAttr(q(outputclass))
      if ($_->outputclass//'') =~ m(\A(heading.*|p.*|standard|t.*|ww.*)\Z)i;
   });

  $x->by(sub                                                                    # AM102 empty <b>
   {if ($_->isAllBlankText(q(b)))
     {$_->unwrap;
     }
   });

  $x->by(sub                                                                    # AM166 li/text -> li/p/text
   {if ($_->isText(qw(li)))
     {$_->wrapWith(qw(p));
     }
   });

  $x->by(sub                                                                    # AM165 remove generated ids - make sure that you run removeGeneratedIdsFromReplacementFiles.pl first to update the replacements first.
   {if ($_->id and $_->id =~ m(\A\d+_\d+\Z)s)
     {$_->id = undef;
     }
   });

  $x->by(sub                                                                    # AM165 remove generated ids - make sure that you run removeGeneratedIdsFromReplacementFiles.pl first to update the replacements first.
   {if ($_->isAllBlankText_tbody)
     {$_->putFirstAsTree(<<END);
<row><entry/></row>
END
     }
   });

  $x->by(sub                                                                    # AM165 remove generated ids - make sure that you run removeGeneratedIdsFromReplacementFiles.pl first to update the replacements first.
   {my ($t) = @_;
     if ($t->at_topicref)
     {$t->change_xref;
      if (my $n = $t->attr_navtitle)
       {$t->putFirstAsText($n);
        $t->deleteAttrs_navtitle;
       }
     }
   });

  $x->by(sub                                                                    # Wrap table first under task body with context  at 2018.12.13 20:49:28
   {my ($t) = @_;
    if ($t->isFirst_table_taskbody)
     {$t->wrapWith_context;
     }
   });

  $x->by(sub                                                                    # PLEX22 at 2018.12.21 19:41:28  Move anything after a result in a task body into the result
   {my ($r) = @_;
    if ($r->at_result_taskbody)
     {for my $o($r->contentAfter)
       {$r->putLast($o->cut);
        if ($o->at_steps)
         {$o->ditaStepsToList;
         }
        elsif ($o->at_result)
         {$o->unwrap;
         }
       }
     }
   });

  $x->by(sub                                                                    # PLEX22 at 2018.12.21 19:41:28  step result under li as a knock on effect
   {my ($r) = @_;
    if ($r->at_stepresult_li)
     {$r->change_p;
     }
   });

  $x->by(sub                                                                    # Move anything after a result in a task body into the result
   {my ($s, $t) = @_;
    if ($t and $t->at_taskbody)
     {if ($s->tag =~ m(\A(steps|steps-unordered)\Z)s)
       {for my $n($s->contentAfter)
         {my $N = $n->tag;
          if ($N =~ m(\A(steps|steps-unordered)\Z))
           {$s->putLast($n->cut);
            $n->unwrap;
           }
          elsif ($N =~ m(\A(ul|p|table)\Z)s)
           {my $step = $s->last;
            my $info = $step->addLast_info;
            $info->putLast($n->cut);
           }
          else
           {last;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # ol first under step to substeps
   {my ($o, $s) = @_;
    if ($o->isFirst_ol_step)
     {$o->ditaListToSubSteps;
      $o->putPrevAsText(q(<cmd>Do the the following:</cmd>));
     }
   });

  $x->by(sub                                                                    # Move p after context in taskbody to context at 2018.12.13 21:49:41
   {my ($c, $t) = @_;
    if ($c->at_context_taskbody)
     {for my $n($c->contentAfter)
       {my $N = $n->tag;
        if ($N =~ m(\A(p|ul|note)\Z))
         {$c->putLast($n->cut);
          $n->unwrap;
         }
        elsif ($N =~ m(\A(table)\Z))
         {$c->putLast($n->cut);
         }
        else
         {last;
         }
       }
     }
   });

  $x->by(sub                                                                    # li not under ol/sl/ul
   {my ($l, $p) = @_;
    if ($l->at_li and $p and $p->tag !~ m(\A(ol|ul)\Z)i)
     {if (my $P = $l->prev)
       {if ($P->tag =~ m(\A(ol|ul)\Z)is)
         {$P->putLast($l->cut);
         }
        else
         {$l->wrapWith_ul;
         }
       }
     }
   });

  $x->by(sub                                                                    # ol under command
   {my ($o, $c, $s) = @_;
    if ($o->isLast_ol_cmd_step)
     {my $S = $o->ditaListToSubSteps;
      $c->putNext($S->cut);
      $c->putFirstAsText(q(Do the following:));
     }
   });

  $x->by(sub                                                                    # desc under li
   {my ($d, $l) = @_;
    if ($d->at_desc_li)
     {if (my $i = $d->next_image)
       {$d->wrapTo($i, q(fig));
       }
      else
       {$d->wrapWith_fig;
       }
     }
   });

  $x->by(sub                                                                    # AW10 image under p
   {my ($i, $p) = @_;
    if ($i->isOnlyChild_image_p)
     {$p->change_fig;
     }
   });

  $x->by(sub                                                                    # ol\cmd\substeps\step after cmd to substeps
   {my ($o, $c, $substep, $substeps, $step) = @_;
    if ($o->isOnlyChildToDepth_3_ol_cmd_substep_substeps_step)
     {$_->unwrap for $c, $substep, $substeps;
      $o->ditaListToSubSteps;
     }
   });

  $x->by(sub                                                                    # PLEX12 at 2018.12.17 20:17:21
   {my ($t, $p) = @_;
    if ($t->isText_p)
     {if ($t->text =~ m(\A\s*important:)is)
       {$p->change_note;
        $p->set(type=>q(important));
        $t->text =~ s(\A\s*important:) ()is;
       }
     }
   });

  $x->by(sub
   {my ($n) = @_;
    if ($n->at_note)
     {if (my $t = $n->attr_type)
       {if ($t =~ m(\A(fastpath|notice|trouble)\Z)s)
         {$n->set(type=>q(important));
          $n->putPrevRequiredCleanUp(qq(PLEX18 - was $t but DTD does not follow OASIS));
         }
       }
     }
   });

  $x->by(sub                                                                    # xref under conbody
   {my ($x, $c) = @_;
    if ($x->at_xref_conbody)
     {$x->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # PLEX23 at 2018.12.21 20:51:23
   {my ($l, $o, $s) = @_;
    if ($l->isOnlyChild_li_ol_steps)
     {$l->change_cmd;
      $o->change_step;
     }
   });

  $x->by(sub                                                                    # PLEX23 at 2018.12.21 20:51:23
   {my ($t, $c, $s, $S, $l) = @_;
    if ($t->isText_cmd_substep_substeps_li)
     {if ($t->text =~ m(Make sure that))
       {$c->change_note;
        $_->unwrap for $s, $S;
       }
     }
   });

  $x->by(sub                                                                    # PLEX6 at 2018.12.21 23:18:47 steps - ol
   {my ($s) = @_;
    if (my $o = $s->an_steps_ol_taskbody)
     {$s->putLastCut($o);
      $o->change_steps;
      $o->unwrap;
     }
   });

  $x->by(sub                                                                    # PLEX20 at 2018.12.27 20:51:48 unwrap bold under cmd
   {my ($b) = @_;
    if ($b->at_b_cmd)
     {$b->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX114 at 2018.12.28 01:37:01
   {my ($p, $l, $u, $e) = @_;
    if ($p->isOnlyChildToDepth_3_p_li_ul_entry)
     {$_->unwrap for $l, $u;
     }
   });

  $x->by(sub                                                                    # NEX115 at 2018.12.28 01:00:12
   {my ($g, $t) = @_;
    if ($g->at_tgroup_table)
     {if (my $cols = $g->attrX_cols)
       {for my $bh($g->c_tbody, $g->c_thead)
         {for my $r($bh->c_row)
           {my @e = $r->c_entry;
            if (@e < $cols)
             {my $nameSE = 0;
              for my $e(@e)
               {++$nameSE if $e->attr_namest or $e->attrX_nameend;
               }
              unless($nameSE)
               {$r->putLast($r->newTag_entry) for @e..$cols;
               }
             }
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX116 at 2018.12.28 01:10:40
   {my ($r) = @_;
    if ($r->at_xref)
     {if (my $h = $r->href)
       {if ($h =~ m(\Ahttps?://|\Awww\.|\.com\Z|\.org\Z|\Amailto:)s)            # Added mailto: at 2019.01.09 18:54:45
         {$r->set(scope=>q(external), format=>q(html));
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX117 at 2018.12.28 01:22:04
   {my ($o) = @_;
    if (my $c = $o->outputclass)
     {if ($c =~ m(\AInternet_20_link\Z))
       {$o->deleteAttr_outputclass;
       }
     }
   });

  $x->by(sub                                                                    # NEX118 at 2018.12.28 01:25:04
   {my ($r) = @_;
    if ($r->isAllBlankText_row)
     {$r->cut;
     }
   });

  $x->by(sub                                                                    # NEX119 at 2018.12.28 01:31:34
   {my ($r, $h) = @_;
    if ($r->at_row_thead and !$r->isFirst)
     {my $b = $h->addNext_tbody;
      $b->putLastCut($r);
     }
   });

  $x->by(sub                                                                    # NEX120 at 2018.12.28 01:32:05
   {my ($u) = @_;
    if ($u->at_ul)
     {$u->ditaMergeLists;
     }
   });

  $x->by(sub                                                                    # NEX121 at 2018.12.28 01:33:54
   {my ($i, $p) = @_;
    if ($i->at_image_p)
     {$p->change_fig;
     }
   });

  $x->by(sub                                                                    # NEX122 at 2018.12.28 01:40:09
   {my ($r, $c) = @_;
    if ($r->at_xref_conbody)
     {$r->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # PLEX fig within fig
   {my ($f, $F) = @_;
    if ($f->isLast_fig_fig)
     {$F->putNextCut($f);
     }
   });

  $x->by(sub                                                                    # NEX126 Move leading text and fig in results into preceeding step
   {my ($r) = @_;
    if ($r->at_result_taskbody)
#    {if ($r->outputclassX =~ m(\AList_20_Continue)s)                           # Result
     {#if ($r->outputclassX =~ m(\AList_20_Continue)s)                          # Try for all result
       {if (my $steps = $r->prev_steps)                                         # Previous steps
         {if (my $step = $steps->last_step)                                     # Last step
           {my @m;                                                              # Grab
            for my $o(@$r)
             {my $tag = -t $o;
              if ($o->isText or $tag =~ m(\A(fig|image|p)\Z)s)                  # Leading text and fig|image
               {push @m, $o;                                                    # Grab leading items
                next;
               }
              last;
             }
            if (@m)                                                             # Move leading items into stepresult
             {my $stepResult = $m[0]->wrapTo($m[-1], q(stepresult));
              $step->putLastCut($stepResult);                                   # Position step result
             }
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # Move leading ol/ul into previous steps as steps
   {my ($r) = @_;
    if (my $steps = $r->ap_result_steps_taskbody)
     {my @c = @$r;                                                              # Provide array context in following for()
      for my $o(@c)
       {if ($o->at(qr(\A(ol|ul)\Z)i))                                           # Leading ol or ul
         {$o->ditaListToSteps;                                                  # Convert to steps
          $steps->putLastCut($o);                                               # Position steps
          $o->unwrap;                                                           # Unwrap moved steps
          next
         }
        last
       }
      $r->unwrap if $r->isAllBlankText;                                         # Unwrap empty result
     }
   });

  $x->by(sub                                                                    # NEX132 at 2018.12.31 00:42:42
   {my ($t, $c, $s) = @_;
    if ($t->isText_cmd_step)                                                    # Cmd
     {if ($t->text =~ m(following:\Z))                                          # Cmd ending in following:
       {my @s;
        for my $step($s->contentAfter)                                          # Following steps that start with to
         {if ($step->at_step)                                                   # Step
           {if (my $t = $step->go_cmd_CDATA)                                    # Text
             {if ($t->text =~ m(\ATo)s)                                         # Text starts with To
               {push @s, $step;                                                 # Step with cmd starting with to
                next;                                                           # Keep going
               }
             }
           }
          last;                                                                 # No more such cmd
         }
        if (@s)                                                                 # Convert matching steps to choices
         {my $choices = $s[0]->wrapTo($s[-1], q(choices));                      # Wrap in choices
          $choices->ditaStepsToChoices;                                         # Convert to choices
          $s->putLastCut($choices);                                              # Move choices into position
         }
       }
     }
   }) if 0;  ###TEST check failures without

  $x->by(sub                                                                    # p onlyChild li first under taskbody at 2019.01.04 01:08:10
   {my ($p, $l) = @_;
    if ($p->isOnlyChild_p_li_taskbody)
     {if ($l->isFirst)
       {$l->unwrap;
       }
     }
   });

  $x->by(sub                                                                    # NEX133 at 2019.01.02 23:14:45 fig under cmd to following info
   {my ($f, $c) = @_;
    if ($f->at_fig_cmd)
     {$c->addNext_info->putFirstCut($f);
     }
    elsif ($f->at_note_cmd)
     {$c->addNext_info->putFirstCut($f);
     }
   });

  $x->by(sub                                                                    # NEX134 at 2019.01.03 00:05:12 fig (CDATA image CDATA) to fig (p image p)
   {my ($t, $f) = @_;
    if ($t->isText_fig)
     {$t->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # NEX136 at 2019.01.03 19:09:40
   {my ($p, $l, $c) = @_;
    if ($p->isOnlyChild_p_li_conbody and $l->isFirst)
     {$l->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX137 at 2019.01.03 19:16:28
   {my ($b, $c) = @_;
    if ($b->at_b_conbody)
     {$b->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # NEX138 at 2019.01.03 19:21:53
   {my ($f) = @_;
    if ($f->at_fig)
     {my $c = $f->contentAsTags;
      if ($f->over(qr(\Aimage fig (draw:contour-polygon )?p\Z)))
       {my ($i, $F, $p) = @$f;
        $F->putLastCut($p);
        $f->putNextCut($F);
       }
     }
   });

  $x->by(sub                                                                    # NEX139 at 2019.01.03 19:55:40
   {my ($n, $o) = @_;
    if ($n->isOnlyChild_note_note)
     {if ($n->attrX_type =~ m(\Aremember\Z)is)
       {$n->unwrap;
        $n->putFirstAsText(q(Remember: ));
       }
     }
    elsif ($n->at_note)
     {if ($n->attrX_type =~ m(\Anotes\Z)is)
       {if (my $u = $n->next_ul)
         {$n->putFirstCut($u);
          $n->deleteAttr_type;
         }
       }
     }
    elsif ($n->at_xref)                                                         # Remove navtitle from xref for: /home/phil/x/aws/wordToDita/nex/reports/fails/c_miscvalues.dita
     {$n->deleteAttr_navtitle;
     }
    elsif ($n->at(q(text:date)))                                                # NEX140 at 2019.01.03 20:23:59
     {$n->change_codeph->set(outputclass=>"date");
     }
    elsif ($n->isOnlyChild_fig_b)                                               # Unwrap b around fig
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX143 at 2019.01.03 21:44:42
   {my ($b) = @_;
    if ($b->at_taskbody)
     {my @s = $b->c_steps;
      my @u = $b->c(q(steps-unordered));
      if (@u and @s)
       {for my $u(@u)
         {$u->change_steps;
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX142 at 2019.01.03 21:06:34
   {my ($b) = @_;
    if ($b->at_taskbody)
     {my @s = $b->c_steps;
      while(@s > 1)
       {my ($b, $a) = reverse @s;
        if ($a->next != $b)
         {my $i = $a->next->wrapTo($b->prev, q(info));
          if (my $step = $a->last_step)
           {$step->putLastCut($i);
           }
         }
        $a->putLastCut($b);
        $b->unwrap;
        pop @s;
       }
     }
   });

  $x->by(sub                                                                    # Still stuff that needs to become context
   {my ($s, $b) = @_;
    if ($s->at_steps_taskbody)
     {if (!$s->isFirst)
       {if (!$b->go_context)
         {$s->prev->wrapFromFirst_context;
         }
       }
     }
    elsif ($s->at_context_taskbody)                                             # Still stuff after context but before steps at 2019.01.04 01:26:23
     {if (my $steps = $b->firstIn(qw(steps steps-unordered)))
       {if ($s->next != $steps)
         {$s->moveEndBefore($steps);
         }
       }
     }
   });

  $x->by(sub                                                                    # Still stuff that needs to become result
   {my ($b) = @_;
    if ($b->at_taskbody)
     {if (!$b->go_result)
       {if (my $s = $b->lastIn(qw(steps steps-unordered)))
         {if (!$s->isLast)
           {$s->next->wrapToLast_result;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # Table inside table
   {my ($g, $b, $G, $T) = @_;
    if ($g->at_tgroup_tbody_tgroup_table)
     {$_->unwrap for $b, $G;
     }
   });

  $x->by(sub                                                                    # Unwrap empty ol, li or step
   {my ($l) = @_;
    if ($l->isAllBlankText(qr(\A(li|ol|step|ul)\Z)))
     {$l->unwrap;
     }
   });

  $x->by(sub                                                                    # Note under note at 2019.01.04 01:46:46
   {my ($n, $N) = @_;
    if ($n->isOnlyChild_note_note)
     {if (my $t = $n->attr_type)
       {if (!$N->attr_type)
         {$N->set(type=>$t);
         }
       }
      $n->unwrap;
     }
   });

  $x->by(sub                                                                    # p b p at 2019.01.04 21:56:07
   {my ($b) = @_;
    if (my @p = $b->apn_p_b_p)
     {my ($p, $q) = @p;
      $p->putLastCut($_) for $b, $q;
      $q->unwrap;
     }
   });

  $x->by(sub                                                                    # note wrapped in list after substep  at 2019.01.04 22:11:45
   {my ($n, $l, $o, $c, $s) = @_;
    if ($n->isOnlyChildToDepth_2_note_li_ol_cmd_substep)
     {$l->unwrap;
      $o->change_info;
      $c->putNextCut($o);
     }
   });

  $x->by(sub                                                                    # Cmd buried in li\ol with following info at 2019.01.04 22:54:37
   {my ($p, $l, $o, $c) = @_;
    if ($p->isOnlyChildToDepth_2_p_li_ol_cmd)
     {$p->change_cmd;
      $c->change_info;
      $c->putPrevCut($p);
      $_->unwrap for $l, $o;
     }
   });
  $x->by(sub                                                                    # Cmd buried in li with following info at 2019.01.04 22:54:37
   {my ($p, $l, $c) = @_;
    if ($p->isOnlyChildToDepth_1_p_li_cmd)
     {$p->change_cmd;
      $c->change_info;
      $c->putPrevCut($p);
      $_->unwrap for $l;
     }
   });

  $x->by(sub                                                                    # Step result only child of step to preceding step
   {my ($r, $s) = @_;
    if ($r->isOnlyChild_stepresult_step)
     {if (my $S = $s->prev_step)
       {$S->putLastCut($r);
        $s->unwrap;
       }
     }
   });

  $x->deleteAttrValueAtInTree_type_notes_note;                                  # note=notes

  $x->by(sub                                                                    # NEX123 at 2019.01.06 19:25:21 - does not appear to be necessary
   {if (my ($t, $title, $concept) = @_)
     {if ($t->isText_title_concept)
       {if ($t->text =~ m(\A(Working with Basic and Common Features)(.*)\Z))
         {my $s    = $2;
          $t->text = $1;
          my $c = $concept->go_conbody__addFirst_p;
          $c->putFirstAsText($s);
         }
       }
     }
   });

  $x->by(sub                                                                    # Fix tables - were 7614 at 2019.01.06 21:15:51 now: 1625 bad tables and all lacking headers
   {if (my ($t) = @_)
     {if ($t->at_table)
       {$t->fixTable;
       }
     }
   });

  $x->by(sub                                                                    # 2019.01.06 21:15:51
   {if (my ($r) = @_)
     {if ($r->at_result)                                                        # At result
       {if ($r->outputclassX =~ m(\AList_20_Continue))                          # Outputclass check
         {if (my $s = $r->prev_steps)                                           # Previous steps
           {$s->putLastCut($r);                                                 # Move
            $r->unwrap;                                                         # Unwrap
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX146 at 2019.01.07 21:05:39
   {my ($o, $s) = @_;
    if ($o->at_ol_steps)
     {$o->ditaListToSteps;
      $o->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX147 extended at 2019.01.07 21:05:36
   {my ($o) = @_;
    if (my $s = $o->ap(qr(CDATA|fig|image|note|table|ul|xref), q(step)))
     {$s->putLastCut($o->wrapWith_info);
     }
   });

  $x->by(sub                                                                    # NEX148 at 2019.01.07 22:18:17
   {my ($u) = @_;
    if (my $s = $u->ap_ul_step)
     {$s->putLastCut($u->change_choices);
      for my $l(@$u)
       {$l->change_choice;
       }
     }
   });

  $x->by(sub                                                                    # NEX149 at 2019.01.07 23:14:58
   {my ($f, $c, $s) = @_;
    if ($f->isLast_fig_cmd_step)
     {$c->moveEndBefore($f->wrapWith_info);
     }
   });

  $x->by(sub                                                                    # Step followed by p at 2019.01.08 01:58:20
   {my ($p) = @_;
    if (my $s = $p->ap
     (qr(\A(b|CDATA|fig|note|p|required-cleanup|table|ul|xref)\Z),              # Move as info into preceding step  at 2019.01.08 18:29:07
      qw(step steps)))
     {$s->moveEndAfter($p->wrapWith_info);
     }
   });

  $x->by(sub                                                                    # Stepresult before info to info at 2019.01.08 02:30:34
   {my ($s) = @_;
    if (my $i = $s->an_stepresult_info)
     {$s->change_info;
     }
   });

  $x->by(sub                                                                    # Empty notes
   {my ($n) = @_;
    if ($n->isAllBlankText_note)
     {$n->unwrap;
     }
   });

  $x->byReverse(sub                                                             # Note|Xref in cmd
   {my ($n, $c) = @_;
    if ($n->isLast(qr(\A(note|xref)\Z), q(cmd)))
     {$c->putNextCut($n->wrapWith_info);
     }
   });


  $x->by(sub                                                                    # Note Xref CDATA in cmd
   {my ($c) = @_;
    if ($c->at_cmd)
     {if ($c->over2(qr(note ( xref | CDATA )+\Z)))
       {my $n = $c->go_note;
           $n->moveEndLast;
        $c->putNextCut($n->wrapWith_info);
       }
     }
   });

  $x->by(sub                                                                    # NEX150 at 2019.01.08 20:29:09
   {my ($t, $p, $i) = @_;
    if ($t->isText(q(p), qr(\A(fig|image)\Z)))                                  # Text under p under info or fig
     {if ($t->text !~ m(\.\s*\Z)s)                                              #2
       {if ($i->over(qr(\Ap image p\Z)))                                        #1
         {my ($p, $i) = @$i;                                                    # Address nodes under info
          $p->moveEndAfter($i);                                                 #3
          $p->mergeLikeElements;                                                #4
         }
       }
     }
   });

  $x->by(sub                                                                    # Fix the only file that has problems when we unwrap all results, File: /home/phil/r/wordToDita/out/Modified_Stage_2_2018_Objective_3_CPOE for Medication_Laboratory_and _Radiology_Orders_for_NextGen_HQM/t_hqmsettings.dita
   {my ($o, $c, $s) = @_;
    if ($o->isOnlyChild_ol_cmd_step)
     {if (my $step = $s->prev_step)
       {if (my $t = $step->go_cmd_CDATA)
         {if ($t->text =~ m(\AThe following apply:\Z)s)
           {$step->putLastCut($s);
            $o->ditaListToChoices;
            $_->unwrap for $c, $s;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # li first under conbody at 2019.01.11 19:12:37
   {my ($l, $b) = @_;
    if ($l->isFirst_li_conbody)
     {$l->change_p;
     }
   });

  $x->byReverse(sub                                                             # Scan parse tree to merge back to back info NEX151 at 2019.01.09 19:33:05
   {$_->mergeLikeElements_info;                                                 # Merge back to back info
   });

  my $docHistory = 0;
  $x->by(sub                                                                    # Left over document revision histories
   {my ($t, $p, $q, $r) = @_;
    if ($t->isText)                                                             # Text identifying document revision history
     {my $Title = qq(Document Revision History);
      if ($p and $t->text =~ m(\A$Title\Z)s)
       {if ($p->at_stepresult_step_steps)                                       # Revision history location fix
         {if (my $result = $r->next_result)
           {$result->putFirstCut($p->change_p);
           }
         }
        return if $p->at_title_concept;                                         # Otherwise we get a lot of empty concepts

        my $b = $p->wrapToLast_conbody;
        my $c = $b->wrapWith_concept;
        $c->createGuidId;
        $c->putFirstAsText(qq(<title>$Title</title>)) unless $c->go_title;
        $c->cut;

        $project->cleanUpDocumentRevisionHistory($c);                           # Cleanup the topic

        my $source = $c->ditaPrettyPrintWithHeaders;                            # Source xml for file

        my $file = gbStandardFileName($source, outExtTopic);                    # Standardize topic file name
        my $path = fpf(out, $file);                                             # Full file name for topic

        my $l = $project->linter;                                               # Linter
        $l->file     = $path;                                                   # File for topic
        $l->source   = $source;                                                 # Source to lint
        $l->ditaType = -t $c;                                                   # Topic type
        $l->guid     = $c->id;                                                  # Guid for topic
        $l->title    = $Title;                                                  # Title for topic
        $l->lint;                                                               # Lint topic

        push @{$project->titleFiles}, [$c->tag, length($source), $Title, $path];# Title list
        $project->revisionHistory = $file;                                      # Note creation of document revision history
       }
     }
   });

  $x->by(sub                                                                    # Image under command
   {my ($i, $c) = @_;
    if ($i->isLast_image_cmd)
     {my $info = $i->wrapWith_info;
      $c->putNextCut($info);
     }
   });

  $x->by(sub                                                                    # Fig first under cmd at 2019.04.11 19:51:42
   {my ($f, $c) = @_;
    if ($f->isFirst_fig_cmd)
     {$c->putPrevCut($f->wrapWith_Note);
     }
    elsif ($f->isFirst_fig_fig)                                                 # Fig first in fig at 2019.04.11 19:53:00
     {$c->putPrevCut($f);
     }
    if (my $i = $f->id)                                                         # Spaces in ids at 2019.04.11 19:55:03
     {if ($i =~   m([ ,&;%/])s)
       {$f->id =~ s([ ,&;%/]+) (_)gs;
       }
     }
    if ($f->at(qr(\A(draw:a|text:change(-(start|end))?)\Z)is))                  # Draw:a at 2019.04.11 20:10:39
     {$f->unwrap;
     }
   });

  $x->by(sub                                                                    # ol in cmd at 2019.04.12 01:25:12
   {my ($o, $c, $s) = @_;
    if ($o->at_ol_cmd_step)
     {$c->putNextCut($o->wrapWith_info);
      $c->mergeLikeNext;
     }
   });

  if ($x->isAllBlankText_reference)                                             # empty reference at 2019.04.12 01:31:10
   {my $a = $x->putFirstAsTree(<<END);
<a>
  <title>Unknown</title>
  <refbody/>
</a>
END
    $a->unwrap;
   }

  $project->cleanUpCutOutTopicNex ($x);
  $project->cleanUpCutOutTopicPlex($x);

  $x->by(sub                                                                    # NEX283 - fig\p\result
   {my ($f, $p, $r) = @_;
    if ($f->isOnlyChild_fig_p_result)
     {$p->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX283 - note and fig at head of results to last step as info
   {my ($r) = @_;
    if ($r->at_result)
     {if (my $steps = $r->prev_steps)
       {if (my $step = $steps->last_step)
         {for(1..9)
           {if (my $f = $r->first(qr(\A(note|fig)\Z)is))
             {my $i = $step->putLastCut($f->wrapWith_stepresult);
              $i->mergeLikePrev;
              next;
             }
            last;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX283
   {my ($t, $r, $step, $steps) = @_;
    if ($t->isText_stepresult_step_steps)
     {my $text = $t->text;
      if ($text =~ m(Option\s*One)is                     or
          $text =~ m(Follow one of the options below:)is or
          $text =~ m(\ACriteria 1:)is)
       {$r->change_info;
        if (my $subSteps = $step->wrapSiblingsAfter_substeps)
         {for my $s($subSteps->c_step)
           {$s->change_substep;
           }
          $step->putLastCut($subSteps);
          if (my $result = $steps->next_result)
           {for(1..10)
             {if (my $p = $result->first_p)
               {if (my $t = $p->firstText)
                 {my $text = $t->text;
                  if ($text =~ m(Option)is                                    or
                      $text =~ m(\A\s*(AND with or without:?|AND with)\s*\Z)is or
                      $text =~ m(\ACriteria 2:)is)
                   {if (my $u = $p->next_ul)
                     {$u->ditaListToSubSteps;
                      $step->putLastCut($_) for $p->change_info, $u;
                      next;
                     }
                   }
                 }
               }
              last;
             }
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # NEX283 - ol\cmd\substep
   {my ($o, $c, $s) = @_;
    if ($o->isLast_ol_cmd)
     {my $i = $o->wrapWith_info;
      $c->putNextCut($i);
     }
   });

  $x->by(sub                                                                    # Remove empty steps
   {$_->unwrap if $_->isAllBlankText_step;
   });

  $x->by(sub                                                                    # PS2-513
   {my ($t, $b) = @_;
    if ($t->isText_conbody)
     {$t->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # PS2-530 - clean up titles
   {my ($t, $T) = @_;
    if ($t->isFirstText_title)
     {my $s = $t->text;
      $t->text =~ s(\A\s*) ()s;
      $t->text =~ s(\A(\d|\.|\s|[A-Z]\.|Section)*) ()is;                        # PS2-532
     }
   });

  $x->by(sub                                                                    # PS2-534 - remove first p under body of it is just numbers
   {my ($p, $b, $topic) = @_;
    if ($p->isFirst(q(p), qr(body\Z)))
     {if ($p->stringContent =~ m(\A\d*\Z))
       {$p->cut;
       }
     }
   });

  $x->by(sub                                                                    # PS2-534 - remove first p under body of it is just numbers
   {my ($p, $b, $topic) = @_;
    if ($p->isFirst(q(p), qr(body\Z)))
     {if (my $t = $b->prev_title)
       {if ($t->isAllBlankText)
         {$t->putFirstCut($p);
          $p->unwrap;
         }
       }
     }
   });

  &clientSpecificCleanup($project, $x);                                         # Client specific cleanup


#xxxx

 } # cleanUpCutOutTopic

sub clientSpecificCleanup($$)                                                   #r Client specific clean up
 {my ($project, $x, $file) = @_;                                                # Project, parse tree
 }

sub clientSpecificFilterSections($$)                                            #r filter sections just before cutting out topics - allows us to remove unwanted sections
 {my ($project, $x) = @_;                                                       # Project, parse tree
 }

sub clientSpecificCleanupBookMap($$)                                            #r Clean up a bookmap just before it gets writen
 {my ($project, $x) = @_;                                                       # Project, parse tree
 }

sub Project::cleanUpDocumentRevisionHistory($)                                  # Cleanup the document revision history topic
 {my ($project, $x, $file) = @_;                                                # Project, parse tree, file about to be written to

  $x->by(sub
   {my ($o, $p, $q) = @_;
    if    ($o->at_cmd)
     {$o->change_p;
     }
    elsif ($o->at_info)
     {$o->unwrap;
     }
    elsif ($o->at_xref_conbody_concept)
     {$o->change_p;
      $o->deleteAttr_href;
     }
   });

  $x->by(sub
   {my ($o) = @_;
    if ($o->isText_conbody)
     {$o->wrapWith_p;
     }
   });

  $x->by(sub                                                                    # refbody under conbody at 2019.04.12 01:25:12
   {my ($r, $c) = @_;
    if ($r->at_refbody_conbody)
     {if (my $t = $r->prev_title)
       {$t->cut;
       }
      $r->unwrap;
     }
   });
 }

sub Project::cleanUpCutOutTopicNex($$)                                          # Clean up a topic
 {my ($project, $x, $file) = @_;                                                # Project, parse tree

  $x->by(sub
   {my ($o) = @_;
    if ($o->at_xref)
     {if ($o->hrefX =~ m(../C:)is)                                              # Strange file
       {$o->href =~ s(../) ()gs;
       }
     }
   });

  $x->by(sub                                                                    # NEX111 at 2018.12.06 02:09:52
   {my ($o) = @_;
    $o->unwrap(q(text:alphabetical-index-mark));
   });

  $x->by(sub                                                                    # NEX112 at 2018.12.06 16:52:13
   {my ($o) = @_;
    my $t = -t $o;
    if ($t =~ m(\A
(text:index-body
|text:index-title-template
|text:alphabetical-index-entry-template
|text:alphabetical-index
|text:alphabetical-index-source
|text:alphabetical-index
|text:index-entry-text
|text:index-title-template
|text:index-entry-page-number
|draw:contour-polygon
|draw:custom-shape
|draw:enhanced-geometry
|draw:equation
)\Z)ixs)
     {$o->cut;
     }
   });

  $x->by(sub                                                                    # Wrap sub tables with note
   {my ($t) = @_;
    if ($t->at_table_entry)
     {$t->wrapWith_note;
     }
   });

  $x->by(sub                                                                    # Record a dictation
   {my ($b) = @_;
    if ($b->at_taskbody)
     {if ($b->over(qr(\Afig p ul p p\Z)))
       {my ($f, $p, $u) = @$b;
        $f->wrapWith_context;
        my $steps = $p->wrapToLast(q(steps));
        $_->change_cmd->wrapWith_step for $steps->c_p;
        my $info = $u->wrapWith_info;
        $p->putNextCut($info);
       }
     }
   });


  $x->by(sub                                                                    # NEX272
   {my ($i, $f, $c) = @_;
    if ($i->at_image_fig_cmd)
     {if (my $d = $f->go_desc)
       {$i->putFirstCut($d->change_alt);
       }
      $f->unwrap;
     }
   });

  $x->by(sub                                                                    # NEX273
   {my ($c) = @_;
    if ($c->at_cmd_step_steps)
     {if ($c->isAllBlankText)
       {$c->putFirstAsTree(<<END);
<required-cleanup>This task likely needs a rewrite, task content is not in steps.</required-cleanup>
END
       }
     }
    if ($c->isAllBlankText_steps)                                               # Remove empty steps
     {$c->unwrap;
     }
   });


 } # cleanUpCutOutTopicNex

sub Project::cleanUpCutOutTopicPlex($$$)                                        # Clean up a topic for a specific organization
 {my ($project, $x, $file) = @_;                                                # Project, parse tree, file about to be written to

  $x->by(sub                                                                    # Bold r has floated away at 2018.12.11 20:55:27
   {my ($t, $b, $c) = @_;
    if ($t->isText_b_conbody||$t->isText_b_taskbody and $b->isFirst)
     {if ($t->text =~ m(\A(r|tic)\Z)is)
       {if (my $T = $c->prev_title)
         {my $s = $T->stringContent;
          my $S = sub
           {if ($s eq q(Configu e the Plexxi Control IP Address on One Switch))
             {return q(Configure the Plexxi Control IP Address on One Switch);
             }
            elsif ($s eq q(Elect ostatic Discha ge Precautions))
             {return q(Electrostatic Discharge Precautions);
             }
            elsif ($s eq q(No es))
             {return q(Notices);
             }
           }->();

          if ($S)
           {while(my $b = $c->first_b) {$b->cut}
            $T->replaceContentWithText($S);
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # Unwrap small interior table at 2018.12.11 21:02:48
   {my ($t, $p, $e, $r, $b, $g, $T, $E) = @_;
    if ($t->isText_p_entry_row_tbody_tgroup_table_entry)
     {if ($r->isOnlyChild)
       {if ($t->text =~ m(\A(QSFP Active Optical Cable, 100M|PX-CBL-OP-QSFP-4LC-30M|PX-CBL-OP-QSFP-4SC-10M|PX-CBL-LR4-40G-QSFP-10KM|QSFP. Transceiver for 40 GbE .40GBASE-IR4. Intermediate Range .2KM. over Single mode Fiber-LC Optical Connector|PX-CBL-SR4-40G-QSFP-100M)\Z))
         {$_->unwrap for $r->c_entry, $r, $b, $g, $T;
         }
       }
     }
   });

  $x->by(sub                                                                    # Cmd under substep under li: change to p
   {my ($c, $s, $l) = @_;
    if ($c->isOnlyChild_cmd_substep_li and $c->isOnlyChild)
     {$s->unwrap;
      $c->change_p;
     }
   });

  $x->by(sub                                                                    # First p in task body becomes context if it ends with :
   {my ($p, $t) = @_;
    if ($p->isFirst_p_taskbody)
     {$p->wrapWith_context;
     }
   });

  $x->by(sub                                                                    # p/ul after context becomes part of context
   {my ($c, $t) = @_;
    if ($c->at_context_taskbody)
     {for my $b($c->contentAfter)
       {my $B = $b->tag;
        if ($B =~ m(\A(note|p|table|ul)\Z)s)
         {$c->putLastCut($b);
         }
        elsif ($b->isText)
         {my $t = $b->wrapWith_p;
          $c->putLastCut($t);
         }
        else
         {last;
         }
       }
     }
   });

  $x->by(sub                                                                    # stepresult-stepresult
   {my ($s) = @_;
    if (my $S = $s->an_stepresult_stepresult_step)
     {$s->putLastCut($S);
      $S->change_p;
     }
   });

  $x->by(sub                                                                    # fig in cmd
   {my ($f, $c) = @_;
    if ($f->at_fig_cmd)
     {my $i = $c->addNext(q(info));
      $i->putLastCut($f)
     }
   });

  $x->by(sub                                                                    # PLEX5 at 2018.12.18 18:59:24
   {my ($c, $t) = @_;
    if (my $s = $c->an(qw(context steps-unordered taskbody)))
     {if ($c->stringContent =~ m(You need the following to install the switch in a telco rack:)s)
       {my $u = $s->[0]->wrapTo($s->[2], q(ul));
        $u->ditaStepsToList_ul;
        $c->putLastCut($u);
        $s->change_steps;
       }
     }
   });

  $x->by(sub                                                                    # PLEX19 at 2018.12.19 22:26:35
   {my ($e, $r, $b, $g, $t) = @_;
    if ($e->at_entry_row)
     {if ($e->over2(qr(\A( p )+\Z)))
       {for my $p(@$e)
         {return unless $p->outputclassX =~ m(\Acode)s;
         }
        $e->change_codeblock;
        if (1)                                                                  # Format content of codeblock
         {my $s = $e->stringContent;

          $s =~ s(<p[^>]*>|</p>) (\n)gs;                                        # Remove p tags
          $s =~ s(\n+) (\n)gs;                                                  # Reduce white space a bit

          $e->replaceContentWithText($s);
         }
        for ($r, $b, $g, $t)
         {$_->unwrap if $_->isOnlyChild;
         }

        if ($t->parent and $t->over2(qr(\A( required-cleanup )?( codeblock )\Z)))
         {$t->unwrap;
         }
        $e->putPrevRequiredCleanUp(q(PLEX19));
       }
     }
   });

  $x->by(sub                                                                    # PLEX21 at 2018.12.20 23:55:51
   {my ($p) = @_;
    if ($p->at_p)
     {if ($p->outputclassX =~ m(\Acode)s)
       {$p->change_codeblock;
       }
     }
   });

  $x->by(sub                                                                    # PLEX20 at 2018.12.20 17:46:00
   {my ($p) = @_;
    if ($p->at_p)
     {my @c = $p->c_b;
      if (@c >= 3)
       {$_->unwrap for @c;
        $p->putFirstRequiredCleanUp(qq(PLEX20 - Paragraph contained a lot of bold text));
       }
     }
   });

  $x->by(sub                                                                    # PLEX23  at 2018.12.21 22:23:37
   {my ($i, $l) = @_;
    if ($i->at_info_li)
     {$i->unwrap;
     }
   });

  $x->by(sub                                                                    # PLEX31 at 2018.12.27 21:16:02
   {my ($t, $n) = @_;
    if ($t->isText_note)
     {if ($t->text =~ m(\A\s*Fastpath\s*information\s*\Z)s)
       {$n->wrapWith_p;
        $n->unwrap;
       }
     }
   });

  $x->by(sub                                                                    # PLEX45 at 2019.01.02 21:21:02
   {my ($d, $i) = @_;
    if ($d->at(q(desc), qr(\A(fig|image)\Z)))
     {$d->cut;
     }
   });

# xxxx

 } # cleanUpCutOutTopicPlex

sub Project::cleanUpCutOutTask($$$)                                             # Clean up a topic for a specific organization
 {my ($project, $x, $file) = @_;                                                # Project, parse tree, file

  $x->by(sub                                                                    # Note only child under step
   {my ($n, $s) = @_;
    if ($n->isOnlyChild_note_step)
     {$s->putFirstAsTree(q(<cmd>Please do the following:</cmd>));
      $n->wrapWith_info;
     }
   });

  $x->by(sub                                                                    # Note only child under step
   {my ($f, $c) = @_;
    if ($f->at_fig_cmd)
     {$c->putNextCut(my $i = $f->wrapWith_info);
      $i->putFirstAsTree(<<END);
<required-cleanup>Figure moved out of preceding cmd</required-cleanup>
END
     }
   });

  &clientSpecificCleanUpTask(@_);
 } # cleanUpCutOutTask

sub clientSpecificCleanUpTask($$$)                                              #r Clean up a topic for a specific organization
 {my ($project, $x, $file) = @_;                                                # Project, parse tree, file
 } # clientSpecificCleanUpTask

sub Data::Edit::Xml::convertListToSteps($;$)                                    # Change ol/ul to steps
 {my ($o, $s) = @_;                                                             # List node in parse tree, "step" or "substep"
  return unless $o->at(qw(ol)) or $o->at(qw(ul));
  $s //= q(step);                                                               # Default is to steps
  for my $l($o->contents)
   {$l->change(qw(cmd))->wrapWith($s);
    for my $L($l->contents)
     {$L->unwrap if $L->at(qw(p cmd));
     }
   }
  $o->change($s.q(s));
  $o
 } # convertListToSteps

sub spelling($)                                                                 # Clean up spelling in a string particularly in regard to ampersands which, so far, I have not yet found a way to fix the XML parser to avoid this problem
 {my ($s) = @_;                                                                 # String
  $s =~ s/ & / and /gsi;                                                        # Covert ampersand to &amp; in various circumstances

  $s =~ s(<text:line-break/>) (\n)gs;                                           # Line breaks are more for human readability than anything else
  $s =~ s((<codeblock.*?>)([^\n]))  ($1\n$2)gs;                                 # E040 - codeblock must be followed by new line
  $s =~ s(([^\n])(</codeblock>))    ($1\n\n$2)gs;

  $s =~ s(accomodation) (accommodation)g;

  $s
 } # spelling
