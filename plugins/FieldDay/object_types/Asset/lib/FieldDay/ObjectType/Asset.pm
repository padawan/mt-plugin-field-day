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

package FieldDay::ObjectType::Asset;

use strict;

use Data::Dumper;

use base qw( FieldDay::ObjectType );

sub insert_before {
    return qq{<button};
}

sub object_form_id {
    return 'asset_form';
}

sub stashed_id {
    my $class = shift;
    my ($ctx, $args) = @_;
    my $asset = $ctx->stash('asset');
    return $asset ? $asset->id : undef;
}

sub insert_before_html_head {
    return q{<mt:include name="include/header.tmpl">};
}

# specify terms to use when a tag loads objects
sub load_terms {
    my $class = shift;
    my ($ctx, $args) = @_;
    return {
        'class' => '*',
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
    my @assets;
    while (my $asset = $iter->()) {
        push(@assets, $asset);
    }
    local $ctx->{__stash}->{assets} = \@assets;
#   return Dumper(\@assets);
    return $ctx->tag('assets', $args, $cond);
}

sub sort_by {
    return 'created_on';
}

sub sort_order {
    return 'descend';
}

1;
