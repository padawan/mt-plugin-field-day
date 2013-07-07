##########################################################################
# Copyright (C) 2008-2010 Six Apart Ltd.
# This program is free software: you can redistribute it and/or modify it
# under the terms of version 2 of the GNU General Public License as published
# by the Free Software Foundation, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
# version 2 for more details. You should have received a copy of the GNU
# General Public License version 2 along with this program. If not, see
# <http://www.gnu.org/licenses/>.

package FieldDay::ObjectType::Category;

use strict;

use Data::Dumper;

use base qw( FieldDay::ObjectType );

sub insert_before {
    return qq{<mtapp:settinggroup id="category-ping">};
}

sub object_form_id {
    return 'category_form';
}

sub stashed_id {
    my $class = shift;
    my ($ctx, $args) = @_;
    my $e;
    my $cat = ($ctx->stash('category') || $ctx->stash('archive_category'))
        || (($e = $ctx->stash('entry')) && $e->category);
    return $cat ? $cat->id : 0;
}

sub edit_template_source {
    my $class = shift;
    my ($cb, $app, $template) = @_;
    $$template =~ s/<form method="post"/<form method="post" name="category_form" id="category_form"/;
    $class->SUPER::edit_template_source(@_);
}

sub insert_before_html_head {
    return q{<mt:include name="include/header.tmpl">};
}

# specify terms to use when a tag loads objects
sub load_terms {
    my $class = shift;
    my ($ctx, $args) = @_;
    return {
        $ctx->stash('blog') ? ('blog_id' => $ctx->stash('blog')->id) : (),
    };
}

# called when a tag needs to loop through objects of this type
sub block_loop {
    my $class = shift;
    my ($iter, $ctx, $args, $cond) = @_;
    my $builder = $ctx->stash('builder');
    my $tokens  = $ctx->stash('tokens');
    my $out = '';
    local $ctx->{inside_mt_categories} = 1;
    while (my $cat = $iter->()) {
        local $ctx->{__stash}{category} = $cat;
        local $ctx->{__stash}{entries};
        local $ctx->{__stash}{category_count};
        local $ctx->{__stash}{blog_id} = $cat->blog_id;
        local $ctx->{__stash}{blog} = MT::Blog->load($cat->blog_id);
        my $text = $builder->build($ctx, $tokens, $cond);
        return $ctx->error( $builder->errstr ) unless defined $text;
        $out .= $text;
    }
    return $out;
}

sub sort_by {
    return 'created_on';
}

sub sort_order {
    return 'descend';
}

1;
