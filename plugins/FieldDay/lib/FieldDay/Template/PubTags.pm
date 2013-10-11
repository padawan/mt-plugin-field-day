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

package FieldDay::Template::PubTags;

use strict;

use Data::Dumper;
use FieldDay::YAML qw( field_type object_type );
use FieldDay::Util qw( load_fields require_type mtlog obj_stash_key );

sub get_fd_data {
    my ($plugin, $ctx, $args, $cond) = @_;
    my ($key, $object_id) = obj_stash_key($ctx, $args);
    return $ctx->stash($key) if $ctx->stash($key);
    my $ot = FieldDay::YAML->object_type($args->{'object_type'});
    my %blog_id = ($ot->{'has_blog_id'} && ($args->{'blog_id'} || $ctx->stash('blog')))
        ? ('blog_id' => ($args->{'blog_id'} || $ctx->stash('blog')->id)) : ();
    my %setting_terms = (
        %blog_id,
        'object_type' => $args->{'object_type'},
    );
    my %value_terms = ( %setting_terms, 'object_id' => $object_id );
    $ctx->stash('fd:setting_terms', \%setting_terms);
    $ctx->stash('fd:value_terms', \%value_terms);
    my ($fields_by_name, $grouped_fields, $group_need_ns, $values, $group_orders, $groups_by_id)
        = load_fields($plugin, $ctx, $args, $cond);
    my $fd_data = {
        'fields_by_name' => $fields_by_name,
        'grouped_fields' => $grouped_fields,
        'group_need_ns' => ( $group_need_ns || 0 ),
        'values' => $values,
        'group_orders' => $group_orders,
        'groups_by_id' => $groups_by_id,
    };
    $ctx->stash($key, $fd_data);
    return $fd_data;
}

sub hdlr_FieldGroup {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my %instances = $args->{'instances'} ? (map { $_ => 1 } split(/,/, $args->{'instances'})) : ();
    my $stash_key = obj_stash_key($ctx, $args);
    my $group_id = $ctx->stash($stash_key . ':group_id');
    my $group = $args->{'group'};
    return '' unless ($group_id || $group);
    if ($group) {
        my %groups_by_name = map { $_->name => $_ } values %{$fd_data->{'groups_by_id'}};
        if (defined $groups_by_name{$group}) {
          $group_id = $groups_by_name{$group}->id;
          local $ctx->{'__stash'}{"$stash_key:group_id"} = $group_id;
        } else {
            my $blog_id = $ctx->stash('blog_id');
            my $tmpl = $ctx->{__stash}{template};
            my $msg = 'Publish error in template "%s": '
                        . 'Error publishing %s tag: Unknown group %s';
            $msg
                = sprintf( $msg, $tmpl->name, $ctx->this_tag, $group );
            warn $msg;
            MT->log({
                ($blog_id ? ( blog_id => $blog_id ) : () ),
                message => $msg,
                category => "publish",
                level => MT::Log::ERROR(),
            });
            return $ctx->error($msg);
        }
    }
    my $group_need_ns = $fd_data->{'group_need_ns'}->{$group_id};
    my @indices = (0 .. ($group_need_ns || 1) - 1);
    if ($args->{'sort_by'}) {
        # we don't need to actually sort the values, just rejigger the indices.
        if ($args->{'numeric'}) {
            if ($args->{'sort_order'} eq 'descend') {
                @indices = sort {
                    $fd_data->{'values'}->{$args->{'sort_by'}}->[$b]->value
                    <=> $fd_data->{'values'}->{$args->{'sort_by'}}->[$a]->value
                } @indices;
            } else {
                @indices = sort {
                    $fd_data->{'values'}->{$args->{'sort_by'}}->[$a]->value
                    <=> $fd_data->{'values'}->{$args->{'sort_by'}}->[$b]->value
                } @indices;
            }
        } else {
            if ($args->{'sort_order'} eq 'descend') {
                @indices = sort {
                    lc($fd_data->{'values'}->{$args->{'sort_by'}}->[$b]->value)
                    cmp lc($fd_data->{'values'}->{$args->{'sort_by'}}->[$a]->value)
                } @indices;
            } else {
                @indices = sort {
                    lc($fd_data->{'values'}->{$args->{'sort_by'}}->[$a]->value)
                    cmp lc($fd_data->{'values'}->{$args->{'sort_by'}}->[$b]->value)
                } @indices;
            }
        }
    }
    my $builder = $ctx->stash('builder');
    my $tokens  = $ctx->stash('tokens');
    my $out;
    for my $i (@indices) {
        next if (%instances && !$instances{$i+1});
        local $ctx->{'__stash'}{"$stash_key:instance"} = $i;
        my $text = $builder->build( $ctx, $tokens )
            or return $ctx->error( $builder->errstr );
        $out .= $text;
    }
    return $out;
}

sub hdlr_Field {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my %instances = $args->{'instances'} ? (map { $_ => 1 } split(/,/, $args->{'instances'})) : ();
    my $stash_key = obj_stash_key($ctx, $args);
    my $field = $ctx->stash($stash_key . ':field') || $args->{'field'};
    return '' unless ($field);
    local $ctx->{'__stash'}{"$stash_key:field"} = $field;
    if (!$fd_data->{'fields_by_name'}->{$field}) {
        return $ctx->error("Field $field not defined");
    }
    my $group_id = $fd_data->{'fields_by_name'}->{$field}->data->{'group'} || 0;
    my $builder = $ctx->stash('builder');
    my $tokens  = $ctx->stash('tokens');
    my $out;
    my $group_need_ns = $fd_data->{'group_need_ns'}->{$group_id} || 0;
    for (my $i = 0; $i < $group_need_ns; $i++) {
        next if (%instances && !$instances{$i+1});
        local $ctx->{'__stash'}{"$stash_key:instance"} = $i;
        my $text = $builder->build( $ctx, $tokens )
            or return $ctx->error( $builder->errstr );
        $out .= $text;
    }
    return $out;
}

sub get_group_id {
    my ($fd_data, $ctx, $args) = @_;
    my $stash_key = obj_stash_key($ctx, $args);
    my $group_id = $ctx->stash($stash_key . ':group_id');
    my $group = $args->{'group'};
    return 0 unless ($group_id || $group);
    if ($group) {
        my %groups_by_name = map { $_->name => $_ } values %{$fd_data->{'groups_by_id'}};
        $group_id = $groups_by_name{$group}->id;
    }
    return $group_id;
}

sub get_group_ids {
    my ($fd_data, $ctx, $args) = @_;
    my $stash_key = obj_stash_key($ctx, $args);
    my $group_id = $ctx->stash($stash_key . ':group_id');
    my @group_ids;
    push(@group_ids, $group_id) if $group_id;
    my $groups = $args->{'group'} || $args->{'groups'};
    return 0 unless ($group_id || $groups);
    if ($groups) {
        for my $group (split(/,/, $groups)) {
            my %groups_by_name = map { $_->name => $_ } values %{$fd_data->{'groups_by_id'}};
            next unless defined $groups_by_name{$group};
            push(@group_ids, $groups_by_name{$group}->id);
        }
    }
    return @group_ids;
}

sub hdlr_IfFieldGroup {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my %instances = $args->{'instances'} ? (map { $_ => 1 } split(/,/, $args->{'instances'})) : ();
    my @group_ids = get_group_ids($fd_data, $ctx, $args);
    # return true if any instance of any field in this group has a value
    for my $group_id (@group_ids) {
        my $group_need_ns = $fd_data->{'group_need_ns'}->{$group_id} || 0;
        for (my $i = 0; $i < $group_need_ns; $i++) {
            next if (%instances && !$instances{$i+1});
            for my $field (@{$fd_data->{'grouped_fields'}->{$group_id}}) {
                my $values = $fd_data->{'values'}->{$field->name};
                next unless ($values && @$values && $values->[$i]);
                return 1 if $values->[$i]->value;
            }
        }
    }
    return 0;
}

sub hdlr_IfField {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my $stash_key = obj_stash_key($ctx, $args);
    my %instances;
    # if there's a stashed instance, we only want to check that one
    if ($args->{'instance'}) {
        $instances{$args->{'instance'} - 1} = 1;
    } elsif (defined $ctx->stash("$stash_key:instance")) {
        $instances{$ctx->stash("$stash_key:instance")} = 1;
    } else {
        %instances = $args->{'instances'} ? (map { $_ => 1 } split(/,/, $args->{'instances'})) : ();
    }
    my $fields = $ctx->stash($stash_key . ':field') || $args->{'field'} || $args->{'fields'};
    return 0 unless ($fields);
    for my $field (split(/,/, $fields)) {
        if (!$fd_data->{'fields_by_name'}->{$field}) {
            next;
            #return $ctx->error("Field $field not defined");
        }
        my $group_id = $fd_data->{'fields_by_name'}->{$field}->data->{'group'} || 0;
        # return true if any instance of this field has a value
        my $values = $fd_data->{'values'}->{$field};
        next unless ($values && @$values);
        my $group_need_ns = $fd_data->{'group_need_ns'}->{$group_id} || 0;
        for my $i (%instances ? (keys %instances) : (0 .. $group_need_ns)) {
            next unless $values->[$i];
            return 1 if $values->[$i]->value;
        }
    }
    return 0;
}

sub hdlr_FieldValue {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my $stash_key = obj_stash_key($ctx, $args);
    my $field = $args->{'field'} || $ctx->stash("$stash_key:field");
    return '' unless $field;
    my $instance = 0;
    if ($args->{'instance'}) {
        $instance = $args->{'instance'} - 1;
    } elsif (defined $ctx->stash("$stash_key:instance")) {
        $instance = $ctx->stash("$stash_key:instance");
    }
    my $values = $fd_data->{'values'}->{$field};
    if (!$fd_data->{'fields_by_name'}->{$field}) {
        return $ctx->error("Field $field not defined");
    }
    my $field_class = require_type(MT->instance, 'field', $fd_data->{'fields_by_name'}->{$field}->data->{'type'} || 'Text');
    if (!($values && @$values && $values->[$instance])) {
        if ($args->{'enter'}) {
            return $field_class->pre_publish($ctx, $args, undef, $fd_data->{'fields_by_name'}->{$field});
        } else {
            return '';
        }
    }
    return $field_class->pre_publish($ctx, $args, $values->[$instance]->value, $fd_data->{'fields_by_name'}->{$field});
}

sub hdlr_FieldLabel {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args, $cond);
    my $stash_key = obj_stash_key($ctx, $args);
    my $field = $args->{'field'} || $ctx->stash("$stash_key:field");
    return '' unless $field;
    my $field_obj = $fd_data->{'fields_by_name'}->{$field};
    return '' unless $field_obj;
    return $field_obj->data->{'label'};
}

sub hdlr_FieldI {
    my $class = shift;
    my ($plugin, $ctx, $args) = @_;
    my $ot = lc($ctx->stash('tag'));
    $ot =~ s/fieldi$//;
    if ($args->{'id'}) {
        my $object_id = $ctx->tag($ot . 'id');
        require FieldDay::Value;
        my $value = FieldDay::Value->load(
            {
                object_id => $args->{'id'},
                object_type => $ot,
                key => $args->{'field'},
                value => $object_id,
            }
        );
        return $value ? $value->instance : 0;
    }
    my $stash_key = obj_stash_key($ctx, $args);
    return $ctx->stash("$stash_key:instance") + 1;
}

sub hdlr_FieldGroupI {
    return hdlr_FieldI(@_);
}

sub hdlr_FieldCount {
    my $class = shift;
    my ($plugin, $ctx, $args) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args);
    return $ctx->error('No field passed') unless $args->{'field'};
    my $values = $fd_data->{'values'}->{$args->{'field'}};
    return $values ? @$values : 0;
}

sub hdlr_FieldGroupCount {
    my $class = shift;
    my ($plugin, $ctx, $args) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args);
    my $group_id = get_group_id($fd_data, $ctx, $args);
    return $ctx->error('Group not found') unless $group_id;
    my $n = $fd_data->{'group_need_ns'}->{$group_id};
    return $n || 0;
}

sub hdlr_FieldGroupLabel {
    my $class = shift;
    my ($plugin, $ctx, $args) = @_;
    my $fd_data = get_fd_data($plugin, $ctx, $args);
    my $group_id = get_group_id($fd_data, $ctx, $args);
    return $ctx->error('Group not found') unless $group_id;
    my $group = $fd_data->{'groups_by_id'}->{$group_id};
    return $group->data->{'label'};
}

sub hdlr_ByValue {
    my $class = shift;
    my ($plugin, $ctx, $args, $cond) = @_;
    my $tag = $ctx->stash('tag');
    $tag =~ /^(.+)ByValue/i;
    my $ot = FieldDay::YAML->object_type_by_plural($1);
    my $object_type = $ot->{'object_type'};
    my $ot_class = require_type(MT->instance, 'object', $object_type);
    require FieldDay::Value;
    my $terms = $ot_class->load_terms($ctx, $args);
    my $blog_ids = $args->{'blog_id'} || $args->{'include_blogs'};
    if ($blog_ids) {
        $terms->{'blog_id'} = [ split(/,/, $blog_ids) ];
        delete $args->{'blog_id'};
        delete $args->{'include_blogs'};
    }
    my $id_col = ($ot->{'object_datasource'} || $ot->{'object_mt_type'} || $object_type) . '_id';
    my @keys = grep { /^(eq|ne)/ } keys %$args;
    my %use_args;
    for my $key (@keys, qw( gt lt ge le like not_like )) {
        next unless ($args->{$key});
        $args->{$key} =~ s/Date([^>]*)>/Date$1 format="%Y%m%d%H%M%S">/ig;
        my $tmpl = MT::Template->new('type' => 'scalarref', 'source' => \$args->{$key});
        $tmpl->context($ctx);
        $use_args{$key} = $tmpl->output;
    }
    my $load_args = {};
    my @terms;
    my %join_args;
    my @eq;
    my @ne;
    if ($use_args{le} && $use_args{ge}) {
        push(@terms, '-and', { value => { between => [ $use_args{ge}, $use_args{le} ] } });
        delete $use_args{le};
        delete $use_args{ge};
    } elsif ($use_args{lt} && $use_args{gt}) {
        push(@terms, '-and', { value => [ $use_args{gt}, $use_args{lt} ] });
        $join_args{'range'} = { value => 1 };
        delete $use_args{lt};
        delete $use_args{gt};
    }
    my %ops = (
        'ge' => '>=',
        'gt' => '>',
        'le' => '<=',
        'lt' => '<',
    );
    for my $key (keys %use_args) {
        if ($key =~ /^eq/) {
            push(@eq, $use_args{$key});
        } elsif ($key =~ /^ne/) {
            push(@ne, $use_args{$key});
        } elsif ($key =~ /^(ge|gt|le|lt|like|not_like)$/) {
            my $op = $ops{$key} || $key;
            if ($args->{numeric} && ($key =~ /^(ge|gt|le|lt)$/)) {
                push(@terms, '-and', { value => \"$op $use_args{$key}" }); #"
            } else {
                push(@terms, '-and', { value => { $op => $use_args{$key} } });
            }
        }
    }
    if (@eq) {
        push(@terms, '-and', { value => [ @eq ] });
    }
    if (@ne) {
        push(@terms, '-and', { value => { not => [ @ne ] } });
    }
    my %instance_args;
    if ($args->{'instance'}) {
        my @instances = split(',', $args->{'instance'});
        $instance_args{'instance'} = \@instances;
    }
    require FieldDay::Value;
    $load_args->{'join'} = FieldDay::Value->join_on(
        undef,
        [{
            object_id        => \"= $id_col", #"
            key => $args->{'field'},
            %instance_args,
        },
        @terms
        ],
        \%join_args
    );
    my $iter = $ot->{'object_class'}->load_iter($terms, $load_args);
    return $ot_class->block_loop($iter, $ctx, $args, $cond);
}

1;
