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

  return qw(cleanUpCutOutTopics convertDocument spelling) if $$tags{html};      # Accept with modified versions of these routines if there is a html tag present
  ()                                                                            # Return empty list to reject
 }

sub spelling($)                                                                 #r Fix spelling
 {my ($s) = @_;                                                                 # Source string
  $s =~ s(&nbsp;) (&#160;)gs;
  $s
 }

sub convertDocument($$)                                                         #r Convert one document.
 {my ($project, $x) = @_;                                                       # Project == document to convert, parse tree.

  $x->ditaObviousChanges;                                                       # Safe because preceded by convertDocument and followed by cleanUpCutoutTopic

  $x->change_concept_html;
  $x->go_body__change_conbody;

  $x->by(sub
   {my ($o, $p, $q) = @_;
    if ($o->at_head or $o->at_meta)
     {$o->unwrap;
     }
    elsif ($o->at_xref)
     {$o->renameAttr_rel_xtrf;
      $o->renameAttr_name_xtrc;
      $o->cut if $o->attrX_type eq "text/css";
     }
    elsif ($o->at_table)
     {$o->htmlTableToDita;
      $o->deleteAttrs_bgcolor_border_bordercolor_cellpadding_cellspacing_width;
     }
    elsif ($o->at_table)
     {$o->htmlTableToDita;
      $o->deleteAttrs_bgcolor_border_bordercolor_cellpadding_cellspacing_width;
     }
    elsif ($o->at_blockquote)
     {$o->change_cite;
     }
    elsif ($o->at_p_ul or $o->at_p_ol or $o->at_ul_ol)
     {$o->wrapWith_li;
     }
    elsif ($o->at_ul)
     {$o->renameAttr_type_xtrf;
     }
    elsif ($o->at_address||$o->at(q(o:p)) and $o->isAllBlankText)
     {$o->cut;
     }
    elsif ($o->at_dir)
     {$o->change_div;
     }
    elsif ($o->at_p and $o->classX =~ m(\ASubhead-(\d)\Z)s)
     {$o->change(q(h).$1);
     }
    elsif ($o->at_br)
     {$o->unwrap;
     }
    elsif ($o->at_img)
     {$o->change_image;
      $o->renameAttr_src_href;
     }
    elsif ($o->at_font)
     {$o->change_div;
      $o->set(outputclass=>q(font));
     }
    if ($o->attr_style)
     {$o->renameAttr_style_xtrf;
     }
    $o->deleteAttrs_align_border_bgcolor_bordercolordark_bordercolorlight_calss_cell_color_face_height_lang_noresize_padding_scrolling_size_start_target_valign;
   });

  $x->divideHtmlDocumentIntoSections;                                           # Position sections just before html header tags so that subsequently the document can be divided into L<sections|/divideDocumentIntoSections>.

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->at_section_section)
     {$o->ditaConvertSectionToConcept;
     }
    elsif ($o->isOnlyChild_ul_cite)
     {$p->unwrap;
     }
    elsif ($o->at_entry)
     {$o->deleteAttrs_bgcolor_width;
     }
    elsif ($o->at_xref_title)
     {$o->unwrap;
     }
    elsif ($o->at_tgroup_table)
     {if ($o->attrX_cols eq q(0) and my $c = $p->attr_cols)
       {$o->set(cols=>$c);
        $o->wrapContentWith_tbody;
        $p->deleteAttr_cols;
        $p->fixTable;
       }
     }
    elsif ($o->at_div_title)
     {$o->unwrap;
     }
    elsif ($o->at_li or $o->at_ol)
     {$o->deleteAttrs_type;
     }
   });

  $x->by(sub
   {my ($t, $s, $c) = @_;
    if ($t->at_title_section_concept)
     {if (my $T = $c->first_title)
       {if ($t->stringContent eq $T->stringContent)
         {$s->change_conbody;
          $t->cut;
         }
       }
     }
    elsif ($t->at_colgroup)
     {$t->cut;
     }
    elsif ($t->at_table)
     {$t->deleteAttrs_cols;
     }
    elsif ($t->isAllBlankText_ul)
     {$t->cut;
     }
    elsif ($t->at_entry)
     {$t->deleteAttrs_rowspan_colspan;
     }
    elsif ($t->at_cite)
     {if ($t->hasSingleChild_p)
       {$t->first_unwrap;
       }
      elsif ($t->contentAsTags2 =~ m(\A( (ol|p|ul) )+\Z))
       {$t->change_div;
        $t->set(outputclass=>q(cite));
       }
     }
    elsif ($t->at_date)
     {$t->change_codeph;
     }
    elsif ($t->at_strong)
     {$t->change_b;
     }
    elsif ($t->isAllBlankText_base)
     {$t->unwrap;
     }
    elsif ($t->isAllBlankText_ol)
     {$t->unwrap;
     }
   });

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->isOnlyChild_p_cite)
     {$o->unwrap;
     }
    elsif ($o->at_title_concept)
     {$o->mergeLikePrev;
     }
   });

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->at_div)
     {if ($o->contentAsTags2 =~ m(\A( section )+\Z)s)
       {$o->unwrap;
       }
     }
    elsif ($o->at_xref_title)
     {if (my $i = $o->id)
       {$p->id = $i;
        $o->unwrap;
       }
     }
   });

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->at_div_conbody)
     {if ($o->contentAsTags2 =~ m(\A( section )+\Z)s)
       {$o->unwrap;
       }
     }
    elsif ($o->at_xref_title)
     {if (my $i = $o->id)
       {$p->id = $i;
        $o->unwrap;
       }
     }
   });

  $x                                                                            # Return parse tree
 } # convertDocument

sub cleanUpCutOutTopic($$)                                                      #r Clean up a topic once it has been cut out and its output file has been assigned
 {my ($project, $x) = @_;                                                       # Project, parse

  $x->by(sub
   {my ($o, $p) = @_;
    if ($o->isOnlyChildToDepth_2_title_section_li      or
        $o->isOnlyChildToDepth_2_title_section_conbody or
        $o->isOnlyChildToDepth_2_title_section_entry)
     {$o->change_p; $p->unwrap;
     }
   });

  $x->byReverse(sub                                                             # Trailing div to section
   {my ($o, $p) = @_;
    if ($o->at_div_conbody)
     {if ($o->isLast or $o->next_section)
       {$o->change_section;
       }
     }
    elsif ($o->at_div_cite)
     {$o->unwrap; $p->change_codeph;
     }
   });

  $x                                                                            # Return parse tree
 } # cleanUpCutOutTopic

if (!caller)
 {say STDERR "BBBB HTML ";
 }

1
