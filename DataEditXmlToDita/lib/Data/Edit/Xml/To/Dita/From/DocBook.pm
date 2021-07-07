#!/usr/bin/perl -I/home/phil/perl/cpan/DataEditXml/lib  -I/home/phil/perl/cpan/DataTableText/lib -I/home/phil/perl/cpan/DataEditXmlToDita/lib -I/home/phil/perl/cpan/DataEditXmlXref/lib -I/home/phil/perl/cpan/DataEditXmlLint/lib/ -I/home/phil/perl/cpan/GitHubCrud/lib/ -I/home/phil/perl/cpan/DataEditXml/lib/ -I/home/phil/perl/cpan/DitaGBStandard/lib/ -I/home/phil/perl/cpan/FlipFlop/lib/
#-------------------------------------------------------------------------------
# Convert DocBook to Dita
# Philip R Brenan at gmail dot com, Ryffine Inc., 2019
#-------------------------------------------------------------------------------
package Data::Edit::Xml::To::Dita::From::DocBook;
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use utf8;
use feature qw(say current_sub);

sub acceptConversion($$$)                                                       # Decide whether to accept the conversion or not
 {my ($project, $x, $tags) = @_;                                                # Project, parse tree, tag counts

  return qw(cleanUpBookMap cleanUpCutOutTopics convertDocument)                 # Accept with modified versions of these routines if there is an article tag present
    if $$tags{article};
  ()                                                                            # Return empty list to reject
 }

sub syntaxBasic {0}                                                             # Use basic syntatx diagrams if truee
sub ellipsis    {q(&#x2026)}                                                    # Ellipsis

sub convertDocument($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.

  eval {$x->expandIncludes};                                                    # Trap any errors while expanding includes

  if ($@)                                                                       # Report include file failure
   {confess "Failed to expand an include file while processing source file:\n".
      $project->source. "\n$@";
   }

  if ($x->at_book)                                                              # Book to bookmap
   {$x->change_bookmap;
   }
  elsif ($x->at_article)                                                        # Article becomes bookmap wioth cut out topics
   {$x->change_bookmap;
   }
  elsif ($x->at_articleinfo)                                                    # Article info
   {$x->change_bookmaptitle;
   }
  elsif ($x->at_refentry)                                                       # refentry to reference
   {$x->change_reference;
    $x->wrapContentWith_refbody;
   }
  else                                                                          # Wrap material with bookmap - which might be collapsed later on
   {my $b = $x->wrapWith_conbody;
    my $c = $b->wrapWith_concept;
    my $B = $c->wrapWith_bookmap;

    if (my $t = $c->attr_title)
     {$c->addFirst_title->putFirstAsText($t);
      $c->deleteAttr_title;
     }
    elsif (my $T = $b->first__firstText)
     {$c->addFirst_title->putFirstAsText($T->text);
     }
    else
     {$c->addFirst_title;
     }

    $x = $B;
   }

  $x->by(sub
   {my ($o) = @_;
    $o->renameAttr(qw(xml:id id));
    $o->change_section_refsynopsisdiv;
   });

  $x->ditaSyntaxDiagramFromDocBookCmdSynopsis;                                  # Convert to syntaxdiagram
  $x->ditaSyntaxDiagramToBasicRepresentation;                                   # Optionally convert syntax diagram to basic representation
  $x->ditaObviousChanges;

  $x->by(sub                                                                    # dlentry(term, li)
   {my ($o, $p) = @_;
    if ($o->isLast_li_dlentry)
     {$o->change_dd;
     }
    elsif ($o->isFirst_term_dlentry)
     {$o->change_dt;
     }
    elsif ($o->at(qr(\Arefsect\d\Z)))                                           # Refsect to sect
     {$o->change_section;
     }
    elsif ($o->at(qr(\Asect\d\Z)))                                              # Any other sect to concept
     {$o->change_section;
      $o->ditaConvertSectionToConcept
     }
    elsif ($o->at_preface)                                                      # preface
     {$o->unwrap;
     }
    elsif ($o->at_email)                                                        # email
     {$o->change_xref;

      my @s = &develop ? () :  (scope=>q(external));
      $o->set(href=>q(mailto:).$o->stringContent, @s, format=>q(html));
     }
    elsif ($o->at_partintro)                                                    # PS2-182
     {$o->change_section;
     }
   });

  $x->by(sub                                                                    # Appendix/Chapter to concept
   {my ($o, $p, $q) = @_;
    if ($o->isOnlyChild_chapter_conbody or
        $o->isOnlyChild_appendix_conbody)
     {if   (my $t = $o->first_title)
       {if (my $T = $q->first_title)
         {$T->replaceWith($t->cut);
          $o->unwrap;
         }
       }
     }
    elsif ($o->isOnlyChildToDepth_2_imagedata_imageobject_mediaobject or        # imagedata
           $o->isOnlyChildToDepth_2_imagedata_imageobject_inlinemediaobject)
     {$_->unwrap for $p, $q;
      $o->change_image;
      $o->deleteAttrs_align_width;
     }
    elsif ($o->at_xref)                                                         # xref
     {$o->changeAttr(qw(xl:href href));
      $o->changeAttr(qw(xlink:href href));
      $o->deleteAttrs(qw(xmlns:xlink));
     }
    elsif ($o->at_ol)                                                           # ol
     {$o->deleteAttr_mark;
     }
    elsif ($o->at_informaltable)                                                # table
     {$o->change_table;
      $o->deleteAttrs_colsep_rowsep_frame;
     }
   });

  $x->by(sub                                                                    # Appendix/Chapter to concept
   {my ($o, $p) = @_;
    if ($o != $x and $o->at_chapter or $o->at_appendix)
     {$o->change_concept;
      $o->id = q(c).-k $x;
      my $b = $o->wrapContentWith_conbody;
      if (my $t = $b->first_title)
       {$b->putPrevCut($t);
       }
     }
   });

  $x->by(sub                                                                    # Surface ol if possible so we can convert to task
   {my ($o, $p) = @_;
    if ($o->at_ol_p_conbody)
     {$o->breakOutChild;
     }
   });

  $x->by(sub                                                                    # PS2-169 Topic under conbody
   {my ($o, $p, $q, $r, $s) = @_;
    if ($o->at_refnamediv)                                                      # PS2-179
     {my $p = $o->wrapContentWith_p;
      $o->putFirstAsTree(qq(<title>Name</title>));
      $o->change_section;
     }
    elsif ($o->at_refname)                                                      # PS2-179
     {$o->change_ph;
     }
    elsif ($o->at_refpurpose)                                                   # PS2-179
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # PS2-169 Topic under conbody
   {my ($o, $p, $q, $r, $s) = @_;
    if ($o->at_topic_conbody)
     {$o->unwrap;
     }
    elsif ($o->at_title_info)                                                   # PS2-1760 Empty title
     {if ($o->isAllBlankText)
       {$o->unwrap;
       }
     }
    elsif ($o->at_info)                                                         # PS2-171 Empty info
     {if ($o->isAllBlankText)
       {$o->unwrap;
       }
     }
    elsif ($o->at_refentry)                                                     # PS2-172 Unwrap refentry
     {$o->change_reference;                                                     # PS2-358 redo
     }
    elsif ($o->at_refsynopsisdiv)                                               # PS2-173 Unwrap refsynopsisdiv
     {$o->unwrap;
     }
    elsif ($o->at_section and $o != $x)                                         # PS2-174
     {my $n = 0;
      while(my $l = $o->last_section)
       {$o->putNextCut($l);
        last if ++$n > 100;
       }
     }
    elsif ($o->at_manvolnum)                                                    # PS2-175
     {$o->cut;
     }
    elsif ($o->at_refmiscinfo)                                                  # PS2-176
     {$o->cut;
     }
    elsif ($o->at_refentrytitle_refmeta_refbody_reference)                      # PS2-177  PS2-358 redo
     {my $t = $o->change_title;
      $r->putFirstCut($t);
     }
    elsif ($o->at_refmeta)                                                      # PS2-178
     {$o->unwrap;
     }
    elsif ($o->at_p and $o->isAllBlankText)                                     # PS2-180
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # PS2-186
   {my ($o, $p) = @_;
    if ($o->at_fig and $o->outputclassX =~ m(\Ascreencapture\Z)s)               # PS2-191
     {$o->deleteAttrs_outputclass;
     }
   });

  $x->by(sub
   {my ($o) = @_;
    if ($o->at_informalfigure)                                                  # PS2-190
     {$o->unwrap;
     }
   });

  $x->by(sub                                                                    # PS2-249
   {my ($t, $T, $s) = @_;
    if ($t->isText_title_section)
     {if ($t->text =~ m(\AName))
       {if (my $t2 = $s->prev_section__first_title__firstText)
         {if ($t2->text =~ m(\AName))
           {$s->mergeLikePrev;
            $T->cut;
           }
         }
       }
     }
   });

  $x->by(sub                                                                    # PS2-168
   {if (my ($o, $s) = @_)
     {if ($o->at_ol_section)
       {$s->ditaConvertSectionToTask;
       }
     }
    if (my ($o, $p, $s) = @_)
     {if ($o->at_ol_p_section)
       {$o->breakOutChild;
        $s->ditaConvertSectionToTask;
       }
     }
   });

  $x->by(sub                                                                    # PS2-247
   {my ($r) = @_;
    if ($r->at_refbody)
     {my @s = $r->c_section;
      my @c;                                                                    # Classified sections
      for my $s(@s)                                                             # Classify sections
       {if ($s->outputclassX =~ m(\Anew-topic\Z)s)
         {push @c, [$s, 1];
         }
        else
         {push @c, [$s, 0];
         }
       }

      while(@c)                                                                 # Remove leading sections not requiring new topic
       {last if $c[0][1];
        shift @c;
       }

      my @r;                                                                    # Sections in one new reference
      my $saveSectionsAsReference = sub                                         # Wrap sections in reference
       {my ($r) = @r;

        my $body = $r->wrapTo($r[-1], q(refbody));
        my $ref  = $body->wrapWith_reference;
        $ref->putFirstCut($r);                                                  # Move first section in as title
        $r->unwrap;                                                             # PS2-248
       };

      for my $s(@c)                                                             # Parse out each block of sections
       {if ($$s[1])
         {&$saveSectionsAsReference if @r;
          @r = ();
         }
        push @r, $$s[0];                                                        # Save section
       }

      &$saveSectionsAsReference if @r;                                          # Wrap any remaining sections in reference
     }
   });

  $x->by(sub                                                                    # kwd under p needs synph
   {my ($k, $l) = @_;
    if ($k->at(q(kwd), qr(\A(b|dt|p)\Z)))
     {$k->wrapWith_synph;
     }
    if ($k->isOnlyChild_kwd_codeph)
     {$l->change_synph;
     }
   });

  $x->by(sub                                                                    # var under dt becomes varname
   {my ($v, $w) = @_;
    if ($v->at(q(var), qr(\A(codeblock|dt|p)\Z)is))
     {$v->change_varname;
     }
    if ($v->isOnlyChild_var_optional)
     {$w->change_synph;
      $v->set(importance=>q(optional));
     }
   });

  $x->by(sub                                                                    # Syntax diagram under refbody
   {my ($s, $t) = @_;
    if ($s->at_syntaxdiagram_refbody)
     {$s->wrapWith_section;
     }
   });

  if (1)                                                                        # PS2-359
   {my $d = 0;
    $x->through(sub
     {if ($_->at(qr(\A(concept|reference|task)\Z)))
       {++$d;
        $_->outputclass = qq(h$d);
       }
     }, sub
     {if ($_->at(qr(\A(concept|reference|task)\Z)))
       {--$d;
       }
     });
   }

   $x->by(sub                                                                   # PS2-360
    {my ($o, $p, $q) = @_;
     if ($o->isText_title_reference)
      {if ($o->text =~ m(\Apure)is)
        {$q->outputclass = q(cli-reference);
        }
      }
    });

   $x->by(sub                                                                   # Ellipsis
    {if ($_->isText)
      {my $ellipsis = ellipsis;
       $_->text =~ s(\.\.\.) ($ellipsis)gs;
      }
    });

# xxxx
  $x
 } # convertDocument

sub confirmTask($)                                                              #r Heuristics for whether a topic is really a task
 {my ($x) = @_;                                                                 # Parse tree
  1
 }

sub convertToTask($)                                                            #r Heuristics for whether a topic must be a task
 {my ($x) = @_;                                                                 # Parse tree

  0
 }

sub cleanUpBookMap($$)                                                          #r Clean up a book map once all its topics have been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse
  $x->set(q(xml:lang)=>q(en-US));                                               # PS2-295


  $x->by(sub                                                                    # PS2-394
   {my ($o, $p) = @_;
    if ($o->at_chapter_part_bookmap)
     {$o->change_topicref;
     }
    elsif ($o->at_part_bookmap)
     {$o->change_chapter;
     }
   });
 }

sub ps258($)                                                                    # PS2-258
 {my ($x) = @_;
  $x->by(sub
   {my ($f, $c) = @_;
    if ($f->at_fig_cmd)
     {my $r = $f->wrapToLast_stepresult;
      $c->putNextCut($r);
     }
   });
 }

sub ps266($)
 {my ($x) = @_;
  $x->by(sub                                                                    # PS2-266
   {my ($n) = @_;
    if ($n->isAllBlankText_note)
     {$n->cut;
     }
   });
 }

sub cleanUpCutOutTopic($$)                                                      #r Clean up a topic once it has been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse

  $x->by(sub                                                                    # Look for topics to convert to references or tasks
   {my ($b, $c) = @_;
    if ($b->at_conbody_concept)                                                 # Must be a task
     {if (convertToTask($c))
       {$c->ditaConvertConceptToTask
       }
      elsif (my @s = $b->c_section)                                             # Otherwise reference if it has sections in it
       {$c->ditaConvertConceptToReference;
       }
      elsif (confirmTask($b))                                                   # Heuristics for whether the concept might be better as a task
       {$c->ditaConvertConceptToTask;
       }
     }
   });

  $x->by(sub
   {my ($r) = @_;
    if ($r->at_reference)                                                       # PS2-181
     {if (my $t = $r->first_title)
       {if (!$t->next_refbody)
         {my $b = $r->wrapContentWith_refbody;
          $r->putFirstCut($t);
         }
       }
     }
   });

  $x->by(sub
   {my ($p) = @_;
    if ($p->at_p)                                                               # PS2-200
     {if   (my $s = $p->prev_section)
       {if (my $S = $p->next_section)
         {if (my $t = $S->first_title)
           {$t->putNextCut($p);
           }
         }
       }
     }
   });

  $x->by(sub
   {my ($t, $b, $p) = @_;
    if ($t->isText_b_p)                                                         # PS2-201
     {if ($t->text =~ m(Important note)s)
       {$p->change_note;
        $p->set(type=>q(important));
        $b->cut;
       }
     }
   });

  $x->by(sub
   {my ($o, $p, $q) = @_;
    if ($o->at_codeph)                                                          # PS2-204
     {if ($o->attrX_choice =~ m(\Areq\Z)s)
       {$o->deleteAttrs_choice;
       }
     }
    elsif ($o->at_b_b)                                                          # PS2-205
     {$o->unwrap;
     }
    elsif ($o->at_xref_b)                                                       # PS2-206
     {$p->unwrap;
     }

    if ($o->outputclassX =~ m(\A\s*select:\s*title\s*,\s*nopage\s*\Z)is)        # PS2-207
     {$o->deleteAttrs_outputclass;
     }

    if ($o->isText_b_p)                                                         # PS2-208
     {if ($o->text =~ m(Important)is)
       {$q->change_note;
        $q->set(type=>q(important));
        if (my $n = $p->nextText)
         {$n->text =~ s(\A\s*:\s*) ()gs;
         }
        $p->cut;
       }
     }

    if ($o->isFirst_p_dl)                                                       # PS2-209
     {$p->putPrevCut($o);
     }
   });

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->at_b_cmd)                                                           # PS2-252
     {$p->putFirstRequiredCleanUp(q(This task likely needs to be broken into multiple tasks. Rewrite as-needed.));
     }
   });

  $x->by(sub                                                                    # PS2-253 - at the moment only ol last in cmd
   {my ($o, $p) = @_;
    if ($o->isLast_ol_cmd)
     {$p->putNextCut($o->wrapWith_info);
     }
   });

  $x->by(sub                                                                    # PS2-254
   {my ($o) = @_;
    if ($o->at_result)
     {if ($o->go_section)
       {$o->unwrap;
       }
     }
   });

  $x->by(sub                                                                    # PS2-255
   {my ($t, $b, $p) = @_;
    if ($t->isText_b_p)
     {if ($t->text =~ m(\A\s*Note\s*:?\s*)is)
       {$b->cut;
        $p->change_note;
        if (my $t = $p->firstText)
         {$t->text =~ s(\A\s*:\s*) ()s;
         }
       }
     }
   });

  $x->by(sub                                                                    # PS2-257
   {my ($n) = @_;
    if ($n->at_note)
     {if (my $steps = $n->prev_steps)
       {if (my $step = $steps->last_step)
         {$step->putFirst($n->cut);
         }
       }
     }
   });

  ps258($x);

  $x->by(sub                                                                    # PS2-259 screen last in cmd becomes following info
   {my ($s, $c) = @_;
    if ($s->at_screen)
     {$s->change_codeblock;                                                     # PS-260
     }
    if ($s->isLast_codeblock_cmd)
     {my $i = $s->wrapWith_info;
      $c->putNextCut($i);
     }
   });

  $x->by(sub                                                                    # PS2-261
   {my ($f, $s) = @_;
    if ($f->at_fig_stepresult)
     {$s->putFirstRequiredCleanUp(q(This entire step likely needs a rewrite (may be step result). Rewrite as-needed.));
     }
   });

  $x->by(sub                                                                    # PS2-262
   {my ($p, $i, $s) = @_;
    if ($p->isOnlyChild_p_info_step)
     {if ($i->isLast)
       {if (my $r = $i->prev_stepresult)
         {$s->putNextCut($i->change_step);
          $p->change_cmd;
         }
       }
     }
   });

  $x->by(sub                                                                    # PS2-263
   {my ($n, $s) = @_;
    if ($n->isFirst_note_steps)
     {my $c = $s->addPrev_context;
      $c->putLastCut($n);
     }
   });

  ps266($x);                                                                    # PS2-266

  $x->by(sub                                                                    # PS2-267
   {my ($d, $p) = @_;
    if ($d->isOnlyChild_dl_p)
     {$p->unwrap;
     }
   });

  $x->by(sub                                                                    # PS2-268
   {my ($p) = @_;
    if ($p->at_p)
     {if (my $l = $p->attr_xreflabel)
       {$p->putFirstRequiredCleanUp(q(This paragraph likely has references to it that the conversion couldn't resolve. Rewrite and fix as-needed.));
        $l =~ s(\s+) (-)gs;
        $p->set(id=>lc($l), xreflabel=>undef);
       }
     }
   });

  $x->by(sub                                                                    # PS2-269
   {my ($d, $p) = @_;
    if ($d->isLast_dl_p)
     {$p->putNextCut($d);
     }
   });

  $x->by(sub                                                                    # PS2-271
   {my ($r) = @_;
    if ($r->at(q(required-cleanup)))
     {if (my $R = $r->prev(q(required-cleanup)))
       {$r->cut;
       }
     }
   });

  $x->by(sub                                                                    # PS2-272
   {my ($o, $i, $s) = @_;
    if ($o->at_ol_info_step)
     {if (my $c = $i->prev_cmd)
       {my $t = $o->stringContent;
        if ($t !~ m(<ol)s)
         {$o->ditaListToSubSteps;
          $i->unwrap;
         }
       }
     }
   });

  $x->by(sub                                                                    # PS2-275 at 2019.05.07 17:16:07
   {my ($t, $d) = @_;
    if ($t->at_term_dlentry)
     {$t->change_dt;
     }
   });

  $x->by(sub                                                                    # PS2-277
   {my ($n, $s) = @_;
    if ($n->at_note_step)
     {if (my $c = $n->prev_cmd)
       {$c->putPrevCut($n);
       }
     }
   });

  $x->by(sub
   {my ($f, $c, $s) = @_;
    if ($f->isLast_fig_cmd or                                                   # PS2-278
        $f->isLast_dl_cmd)                                                      # PS2-287
     {my $i = $f->wrapWith_info;
      $c->putNextCut($i);
     }
   });

  $x->by(sub                                                                    # PS2-279
   {my ($p, $s) = @_;
    if ($p->at_p_steps)
     {$p->wrapWith_stepsection;
     }
   });

  $x->by(sub                                                                    # PS2-280
   {my ($n, $c, $s) = @_;
    if ($n->at_note_cmd)
     {if (!$n->isLast)
       {my $i = $n->next->wrapToLast_info;
        $c->putNextCut($i);
       }
      $c->putPrevCut($n);
     }
    if ($n->isAllBlankText_p)                                                   # PS-281
     {$n->cut;
     }
    if ($n->isAllBlankText_context)                                             # PS-282
     {$n->cut;
     }
    if ($n->isAllBlankText_dt)                                                  # PS-284
     {$n->cut;
     }
    if ($n->isAllBlankText_dd)                                                  # PS-285
     {$n->cut;
     }
    if ($n->isAllBlankText_dlentry)                                             # PS-286
     {$n->cut;
     }
    if ($n->isAllBlankText_result)                                              # PS-288
     {$n->cut;
     }
   });

  $x->by(sub                                                                    # PS2-289
   {my ($b, $c, $s) = @_;
    if ($b->at_codeblock_cmd)
     {my $i = $b->wrapToLast_info;
      $c->putNextCut($i);
     }
   });

  $x->by(sub                                                                    # PS2-290
   {my ($i, $s) = @_;
    if ($i->at_info_step)
     {if (my $r = $i->prev_stepresult)
       {$r->putLastCut($i);
        $i->unwrap;
       }
     }
   });

  $x->by(sub                                                                    # PS2-291
   {my ($p, $s) = @_;
    if ($p->at_p_step)
     {$p->wrapWith_info->mergeLikePrev;
     }
   });

  $x->by(sub                                                                    # PS2-292
   {my ($n, $s) = @_;
    if ($n->at_note_step)
     {if (my $substeps = $n->prev_substeps)
       {if (my $fss = $substeps->go_substep)
         {$fss->putFirstCut($n);
         }
       }
     }
   });

  $x->by(sub                                                                    # section under p - unwrap p  at 2019.05.07 19:12:45
   {my ($s, $p) = @_;
    if ($s->at_section_p)
     {$p->unwrap;
     }
   });

  $x->by(sub                                                                    # PS2-276
   {my ($o, $c, $s) = @_;
    if ($o->isLast_ol_cmd)
     {$c->putNextCut($o->wrapWith_info);
     }
   });

  $x->by(sub                                                                    # PS2-276 flag ol under info
   {my ($o, $i) = @_;
    if ($o->at_ol_info)
     {$o->putPrevRequiredCleanUp(q(This task might need to be broken into multiple tasks. Please take a look and re-write as needed.));
     }
   });

  $x->by(sub                                                                    # dl to ul at 2019.05.07 19:44:02
   {my ($d) = @_;
    if ($d->at_dl)
     {$d->ditaConvertDlToUl
     }
   });

  $x->by(sub                                                                    # Xrefs at 2019.05.08 04:49:53
   {my ($r) = @_;
    if ($r->at_xref)
     {if (my $h = $r->href)
       {if ($h =~ m(\A(https?://|mailto:|www.))i)
         {$r->set(scope=>q(external), format=>q(html));
         }
       }
     }
   });

  $x->by(sub                                                                    # PS-325
   {my ($t, $b, $c) = @_;
    if ($t->isFirst_title_conbody_concept)
     {if (my $T = $c->first_title)
       {if ($T->isAllBlankText)
         {$T->replaceWith($t->cut);
         }
       }
     }
   });

  ps258($x);                                                                    # Rerun

  ps266($x);

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($v, $w) = @_;
    if ($v->at_var_codeblock)
     {$v->change_varname;
     }
    if ($v->isLast_var_codeblock_synph)
     {$w->putNextCut($v);
     }
    if ($v->at_var_codeph)
     {$w->change_synph;
     }
   });

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($k, $l) = @_;
    if ($k->at_kwd_title)
     {$k->unwrap;
     }
   });

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($g, $h) = @_;
    if ($g->at_groupseq_p)
     {$g->wrapWith_synph;
     }
    if ($g->at_groupchoice)
     {$g->deleteAttr_choice;
     }
   });

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($n, $r) = @_;
    if ($n->at_note_refbody)
     {if (my $s = $n->prev_section)
       {$s->putLastCut($n);
       }
     }
   });

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($g, $s) = @_;
    if ($g->isOnlyChild_groupseq_synph)
     {$g->unwrap;
     }
   });

  $x->by(sub                                                                    # var under codeblock to varname
   {my ($k, $s) = @_;
    if ($k->isOnlyChild_kwd_synph)
     {$k->unwrap;
     }
   });

  $x->by(sub                                                                    # refentrytitle under reference
   {my ($t) = @_;
    $t->change_title_refentrytitle_reference;
   });

  $x->by(sub                                                                    # refentrytitle under reference
   {my ($s, $r) = @_;
    if ($s->at_section_reference)
     {my $b = $s->wrapWith_refbody;
      $b->mergeLikePrev;
     }
   });

  $x->by(sub                                                                    # optional to square brackets - assumes basic syntax digram
   {my ($o) = @_;
    if ($o->at_optional)
     {$o->putPrevAsText(q( [ ));
      $o->putNextAsText(q( ] ));
      $o->unwrap;
     }
   }) if syntaxBasic;

  $x->by(sub                                                                    # userinput last under cmdname to next - assumes basic syntax digram
   {my ($o, $p) = @_;
    if ($o->isLast_userinput_cmdname)
     {$p->putNextCut($o);
     }
   }) if syntaxBasic;

  $x->by(sub                                                                    # userinput last under cmdname to next - assumes basic syntax digram
   {my ($t, $p) = @_;
    if ($t->isLastText_cmdname)
     {if ($t->text =~ m(\x{2026})s)
       {if (my $u = $t->prev_userinput)
         {$p->putNextCut($_) for $t, $u;
         }
       }
     }
   }) if syntaxBasic;

  $x->by(sub                                                                    # userinput last under cmdname to next - assumes basic syntax digram
   {my ($s, $b, $r) = @_;
    $s->cutIfEmpty_section_refbody;
    if ($s->isOnlyChild_section_refbody)
     {if (!$r->go_title)
       {if (my $t = $s->go_title)
         {$r->putFirstCut($t);
         }
       }
     }
   });

  $x->by(sub                                                                    # userinput last under cmdname to next - assumes basic syntax digram
   {my ($p, $s) = @_;
    if ($p->at_p_section)                                                       # PS2-363
     {if ($p->idX =~ m(-synopsis\Z))
       {if (!$s->go_title)
         {$s->putFirstAsTree(<<END);
<title>Synopsis</title>
END
         }
       }
     }
    $p->deleteAttr(qw(outputclass man-type));                                   # PS2-364
   });

  $x->by(sub
   {my ($t, $T, $s) = @_;
    if ($t->isOnlyChildText_title_section)                                      # PS2-365
     {if ($T->isOnlyChild)
       {if ($t->text =~ m(Examples)is)
         {$s->cut;
         }
       }
     }
   });

  $x->by(sub
   {my ($t, $T, $r) = @_;
    if ($t->isOnlyChildText_title_reference)                                    # PS2-366
     {if ($t->text =~ m(\Apure)is)
       {$r->outputclass = q(cli-reference);
       }
     }
   });

  $x->by(sub
   {my ($t, $T, $s) = @_;
    if ($t->isOnlyChildText_title_section)
     {if ($t->text =~ m(see\s*also)is)
       {$s->by(sub
         {my ($r) = @_;
          if ($r->at_xref)                                                      # PS2-367
           {if (my $text = $r->nextText)
             {if ($text->text =~ m(\A\s*,\s*\Z)s)
               {$text->cut;
               }
             }
            $r->wrapUp_li_ul->mergeLikePrev;
           }
         });
       }
     }
   });

  $x->by(sub
   {my ($b) = @_;
    $b->change_uicontrol_b;                                                     # PS2-372
#   if ($b->at_b)                                                               # PS2-372
#    {my $c = $b->stringContent;
#     if ($c =~ m(\ASMB\Z)s)
#      {$b->change_uicontrol;
#      }
#    }
    if ($b->at_table)                                                           # PS2-373
     {$b->set(frame=>"all");
     }
    if ($b->outputclassX =~ m(\Ah\d)is)                                         # PS2-369 - remove heading levels in outputclass
     {$b->deleteAttrs_outputclass;
     }
    if ($b->isAllBlankText_xref)                                                # PS2-368 - place xref href as  text so that Xref can fix it
     {if (my $h = $b->href)
       {if ($h =~ m(\Apure)s)
         {$b->putFirstAsText($h);
         }
       }
     }
   });

  $x->by(sub                                                                    # PS2-372 unwrap codeph under uicontrol
   {my ($c, $u) = @_;
    $c->unwrap_codeph_uicontrol;
   });

  $x->by(sub                                                                    # PS2-374
   {my ($t, $codeblock) = @_;
    if ($t->isFirstText_codeblock)
     {$t->text =~ s(\A\s*) ()s;
     }
   });

  $x->by(sub
   {my ($b, $r) = @_;
    if ($b->at_refbody_reference)
     {if ($r->outputclassX =~ m(\Acli-reference\Z)is)
       {my $author; my $seeAlso;
        for my $s($b->c_section)
         {if (my $t = $s->go_title)
           {my $title = $t->stringContent;
            if ($title =~ m(See\s*also)is)
             {++$seeAlso;
             }
            elsif ($title =~ m(author)is)
             {++$author;
             }
           }
         }
        if (!$seeAlso)                                                          # PS2-370
         {my $a = $b->putLastAsTree(<<'END');
<section>
   <title>See Also</title>
   <draft-comment>Add references to commands as-needed</draft-comment>
</section>
END
         }
        if (!$author)                                                           # PS2-371
         {my $a = $b->putLastAsTree(<<'END');
<section>
  <title>Author</title>
  <p>Pure Storage Inc. <xref format="html" href="mailto:documentfeedback@purestorage.com"
      scope="external">documentfeedback@purestorage.com</xref>
  </p>
</section>
END
         }
       }
     }
   });

   $x->by(sub                                                                   # refbody under refbody
    {my ($r, $R) = @_;
     if ($r->at_refbody_refbody)
      {if (my $t = $R->first_title)
        {$t->cut;
         $R->unwrap;
        }
      }
    });

   $x->by(sub                                                                   # refbody trailing section
    {my ($s) = @_;
     if ($s->at_section_reference)
      {if (my $r = $s->prev_refbody)
        {$r->putLastCut($s);
        }
      }
    });

   $x->by(sub                                                                   # PS2-518
    {my ($T, $t, $e, $l, $s, $r) = @_;
     if ($T->isText_dt_dlentry_dl_section_refbody_reference)
      {if (my $title = $s->go_title__first)
        {if ($title->text =~ m(options)is)
          {$t->wrapContentWith_codeph;
          }
        }
      }
    });

   $x->by(sub                                                                   # PS2-518
    {my ($r, $u) = @_;
     if ($r->isOnlyChild_xref_uicontrol)
      {$u->unwrap;
      }
    });

   $x->by(sub
    {my ($o) = @_;
     $o->deleteAttrs_version;                                                   # Remove version attribute
    });

# xxxx
 } # cleanUpCutOutTopic

1
