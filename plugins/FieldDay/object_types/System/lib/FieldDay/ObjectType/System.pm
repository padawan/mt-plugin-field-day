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

package FieldDay::ObjectType::System;

use strict;

use Data::Dumper;

use base qw( FieldDay::ObjectType );

sub insert_before {
    return qq{<mt:setvarblock name="action_buttons">};
}

sub object_form_id {
    return 'cfg_form';
}

sub insert_before_html_head {
    return q{<mt:include name="include/header.tmpl">};
}

sub stashed_id {
    return -1;
}

sub callbacks {
    return {
        'MT::Config::post_save' => \&post_save_config,
    };
}

sub post_save_config {
    my ($cb, $obj) = @_;
    my $app        = MT->instance;
    my $mode       = eval { $app->param('__mode') };
    return unless $mode and $mode eq 'save_cfg_system_general';
    $obj->id(undef);
    return FieldDay::ObjectType->cms_post_save( $cb, $app, $obj );
}

1;
