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

package MT::App::Search::FieldDay;

use strict;

use base qw( MT::App::Search );
use MT::ObjectDriver::SQL qw( :constants );
use FieldDay::Value;
use FieldDay::YAML qw( object_type );
use FieldDay::Util qw( use_type require_type );
use Data::Dumper;

sub id { 'new_search' }

sub core_methods {
    my $app = shift;
    return {
        'linked' => \&process_link,
        'linking' => \&process_link,
        %{ $app->SUPER::core_methods() },
    };
}

sub core_parameters {
    my $app = shift;
    my $params = $app->SUPER::core_parameters();
    my %filter_types = %{$params->{types}->{entry}->{filter_types}};

    # Add filters for filtering by field
    my $q = $app->param;
    my $blog_id = $q->param('blog_id') || $app->first_blog_id();
    my %terms = (
        type => 'field',
        object_type => 'entry',
        blog_id => $blog_id,
    );
    my %args = (
        'sort' => 'order',
        'direction' => 'ascend',
    );
    for my $field (MT->model('fdsetting')->load_with_default(\%terms, \%args)) {
        $filter_types{$field->name} = \&_filter_by_field;
    }

    $params->{types}->{entry}->{filter_types} = {
        linking_ids => \&_join_linking_ids,
        linked_ids => \&_join_linked_ids,
        category_basename => \&_join_category_basename,
        %filter_types,
    };
    $params;
}

# need to defer this until after we munge the params
sub generate_cache_keys {
}

sub query_parse {
    my $app = shift;
    my ( %columns ) = @_;
    my $parsed = $app->SUPER::query_parse(%columns);
    my %ids;
    if ($app->param('object_ids')) {
        %ids = map { $_ => 1 } split(/,/, $app->param('object_ids'));
    }
    my $type = $app->{searchparam}{Type};
    my $ot = FieldDay::YAML->object_type(use_type($type));
    my $args = {};
    if ($app->param('search') && ($app->param('search') ne '%')) {
        my $terms = $app->def_terms;
        my $like = $parsed->{'terms'}->[0]->[0]->{[keys %{$parsed->{'terms'}->[0]->[0]}]->[0]}->{like};
        my $id_col = id_col($ot);
        $args->{join} = FieldDay::Value->join_on(
            undef,
            {
                'object_id' => \"= $id_col", #"
                'value' => { like => $like },
                'object_type' => $ot->{'object_type'},
                $app->param('fields') ? ('key' => [ split(/,/, $app->param('fields')) ]) : (),
                ($ot->{'has_blog_id'} && exists $app->{searchparam}{IncludeBlogs})
                    ? ('blog_id' => [ keys %{ $app->{searchparam}{IncludeBlogs} } ])
                    : (),
            }
        );
        eval("require $ot->{'object_class'};");
        my $iter = $ot->{'object_class'}->load_iter($terms, $args);
        my @ids;
        while (my $obj = $iter->()) {
            $ids{$obj->id} = 1;
        }
        if (%ids) {
            push(@{$parsed->{terms}->[0]}, '-or', { id => [ keys %ids ] });
        }
    }
    $parsed;
}

sub execute {
    my $app = shift;
    return $app->SUPER::execute(@_) unless $app->param('sort_field');
    my ( $terms, $args ) = @_;

    my $class = $app->model( $app->{searchparam}{Type} )
        or return $app->errtrans('Unsupported type: [_1]', encode_html($app->{searchparam}{Type}));

    my $count = $app->count( $class, $terms, $args );
    return $app->errtrans("Invalid query: [_1]", $app->errstr) unless defined $count;

    my $offset = $args->{offset} || 0;
    my $limit = $args->{limit} || 0;
    delete $args->{offset};
    delete $args->{limit};
    my @results = $class->load( $terms, $args )
        or $app->error($class->errstr);
    my @ids = map { $_->id } @results;

    # Information for each sort column and how it should be sorted
    #   column1:direction:numeric,column2:direction:numeric,...
    my @sort_cols;
    my $sort_by = $app->param('sort_field');
    $sort_by =~ s/[^\w\-\.\,\:]+//g;
    if ($sort_by) {
        my @sort_bys = split ',', $sort_by;
        foreach my $key (@sort_bys) {
            my ($column, $direction, $numeric) = split ':', $key;
            $direction ||= $app->{searchparam}{SearchResultDisplay};
            $direction = ( ( $direction =~ /^desc(?:end)?$/i ) ? 'descend' : 'ascend' );
            $numeric ||= $app->param('sort_numeric');
            $numeric = ( $numeric ? 1 : 0 );
            my $column_type = ( ($class->has_column($column)) ? ($class->datasource) : 'fdvalue' );
            push @sort_cols, {
                column      => $column,
                column_type => $column_type,
                direction   => $direction,
                numeric     => $numeric,
            };
        }
    }

    require FieldDay::YAML;
    require FieldDay::Value;
    my $ot = FieldDay::YAML->object_type_by_class($class);

    # Create array of sort keys
    my @sort_keys;
    foreach my $sort_column (@sort_cols) {
        my @values;
        my %values;
        if ( $sort_column->{column_type} eq 'fdvalue' ) {
            # Create sort keys for FieldDay field
            @values = FieldDay::Value->load({
                key => $sort_column->{column},
                object_type => $ot->{object_type},
                object_id => \@ids,
            });
            %values = map { $_->object_id => lc($_->value || $_->value_text || '') } @values;

            # Get type of fd field, to see if sort field is a Linked* type of field
            my $q = $app->param;
            my $blog_id = $q->param('blog_id') || $app->first_blog_id();
            my %field_terms = (
                type        => 'field',
                object_type => 'entry',
                blog_id     => $blog_id,
                name        => $sort_column->{column},
            );
            my $field = (MT->model('fdsetting')->load_with_default(\%field_terms, undef))[0];
            my $field_type = $field->data->{'type'} if $field;
            my $linked_class = require_type(MT->instance, 'field', $field->data->{'type'});

            # Replace linked object id value with default field value from linked object
            #   (title for entries, label for categories, name for blogs, etc.)
            if ( $field_type =~ /^Linked/ ) {
                foreach my $obj_id (keys %values) {
                    my $obj = MT->model($linked_class->object_type)->load($values{$obj_id});
                    $values{$obj_id} = ( $obj ? lc($linked_class->object_label($obj)) : '' );
                }
            }
        } else {
            # Create sort keys for MT::Entry field
            my $class_column = $sort_column->{column};
            %values = map { $_->id => lc($_->$class_column || '') } @results;
        }
        push @sort_keys, \%values;
    }

    # Do the sort
    @results = sort { 
        my $result;
        for my $i (0 .. $#sort_keys) {
            if ( $sort_cols[$i]->{direction} eq 'descend' ) {
                if ( $sort_cols[$i]->{numeric} ) {
                    $result = ($sort_keys[$i]->{$b->id} || 0) <=> ($sort_keys[$i]->{$a->id} || 0);
                } else {
                    $result = ($sort_keys[$i]->{$b->id} || '') cmp ($sort_keys[$i]->{$a->id} || '');
                }
            } else {
                if ( $sort_cols[$i]->{numeric} ) {
                    $result = ($sort_keys[$i]->{$a->id} || 0) <=> ($sort_keys[$i]->{$b->id} || 0);
                } else {
                    $result = ($sort_keys[$i]->{$a->id} || '') cmp ($sort_keys[$i]->{$b->id} || '');
                }
            }
            return $result if $result;
        }
        return 0;
    } @results;

    my $max;
    if ($limit) {
        $max = $limit + $offset - 1;
    }
    if (!$max || ($max > $#results)) {
        $max = $#results;
    }

    @results = @results[$offset .. $max];
    my $iter = sub { shift @results; };
    ( $count, $iter );
}

sub process_link {
    my $app = shift;

    if (!$app->param('search')) {
        $app->{search_string} = '%';
        $app->param('search', '%');
    }
    my @arguments = $app->search_terms();
    return $app->error($app->errstr) if $app->errstr;
    my $count = 0;
    my $iter;
    if ( @arguments ) {
        ( $count, $iter ) = $app->execute( @arguments );
        return $app->error($app->errstr) unless $iter;

        $app->run_callbacks( 'search_post_execute', $app, \$count, \$iter );
    }
    my @ids;
    while (my $obj = $iter->()) {
        push(@ids, $obj->id);
    }
    $app->param('IncludeBlogs', $app->param(ucfirst($app->mode) . 'Blogs'));
    my $blog_list = $app->create_blog_list;
    $app->{searchparam}{IncludeBlogs} = $blog_list->{IncludeBlogs};
    for my $key (qw( searchTerms search category category_basename author fields )) {
        $app->param($key, '');
    }
    for my $key ($app->param) {
        if ($key =~ /^link(ed|ing)_/) {
            my $val = $app->param($key);
            $key =~ s/^link(ed|ing)_//;
            $app->param($key, $val);
        }
    }
    if (@ids) {
        $app->param($app->mode . '_ids', join(',', @ids));
        $app->{search_string} = $app->param('search') || '%';
    } else {
        # if no linked/linking objects were found, we want the next search to find nothing
        $app->{search_string} = 'mnfn87n4uinbv8hkmsdboiuhne4v8jnvrimn0s8rjvopmfv89jsrgj';
    }
    $app->param('search', $app->{search_string});
    $app->SUPER::generate_cache_keys();
    return $app->process();
}

sub def_terms {
    my $app = shift;
    my $params = $app->registry( $app->mode, 'types', $app->{searchparam}{Type} );
    my %def_terms = exists( $params->{terms} )
          ? %{ $params->{terms} }
          : ();
    #FIXME: why is this in here?
    delete $def_terms{'plugin'};

    if ( exists $app->{searchparam}{IncludeBlogs} ) {
        $def_terms{blog_id} = [ keys %{ $app->{searchparam}{IncludeBlogs} } ];
    }
    my @terms;
    if (%def_terms) {
        # If we have a term for the model's class column, add it separately, so
        # array search() doesn't add the default class column term.
        my $type = $app->{searchparam}{Type};
        my $model_class = MT->model($type);
        if (my $class_col = $model_class->properties->{class_column}) {
            if ($def_terms{$class_col}) {
                push @terms, { $class_col => delete $def_terms{$class_col} };
            }
        }
        push @terms, \%def_terms;
    }
    return \@terms;
}

sub id_col {
    my ($ot) = @_;
    return ($ot->{'object_datasource'} || $ot->{'object_mt_type'} || $ot->{'object_type'}) . '_id'
}

sub _join_category_basename {
    my ( $app, $term ) = @_;

    # search for exact match
    my $terms = [[ { basename => $term->{term} } ]];
    return unless $terms && @$terms;
    push @$terms, '-and', {
        id => \'= placement_category_id',
        blog_id => \'= entry_blog_id',
    };
    require MT::Placement;
    require MT::Category;
    return MT::Placement->join_on( undef,
        { entry_id => \'= entry_id', blog_id => \'= entry_blog_id' },
        { join => MT::Category->join_on( undef, $terms, {} ),
          unique => 1 }
    );
}

sub _join_linked_ids {
    my ($app, $term) = @_;
    return unless $term->{term};
    return unless $app->param('LinkField');
    my @ids = split(/,/, $term->{term});
    my $type = $app->param('LinkedType') || $app->{searchparam}{Type};
    my $ot = FieldDay::YAML->object_type(use_type($type));
    my $id_col = id_col($ot);
    require FieldDay::Value;
    return FieldDay::Value->join_on(
        undef,
        {
            'value'        => \"= $id_col", #"
            'key' => $app->param('LinkField'),
            'object_type' => $ot->{'object_mt_type'} || $ot->{'object_type'},
            'object_id' => \@ids,
        },
        {
            'unique' => 1,
            'alias'  => 'linked_ids',
        }
    );
}

sub _join_linking_ids {
    my ($app, $term) = @_;
    return unless $term->{term};
    return unless $app->param('LinkField');
    my @ids = split(/,/, $term->{term});
    my $type = $app->param('LinkingType') || $app->{searchparam}{Type};
    my $ot = FieldDay::YAML->object_type(use_type($type));
    my $id_col = id_col($ot);
    require FieldDay::Value;
    return FieldDay::Value->join_on(
        undef,
        {
            'object_id'   => \"= $id_col", #"
            'value'       => \@ids,
            'key'         => $app->param('LinkField'),
            'object_type' => $ot->{'object_mt_type'} || $ot->{'object_type'},
        },
        {
            'unique' => 1,
            'alias'  => 'linking_ids',
        }
    );
}

sub _filter_by_field {
    my ($app, $term) = @_;
    return unless $term->{term};

    my $q = $app->param;
    my $blog_id = $q->param('blog_id') || $app->first_blog_id();
    my $type = $app->{searchparam}{Type};
    my $ot = FieldDay::YAML->object_type(use_type($type));
    my $id_col = id_col($ot);

    # Get type of fd field using fd field name
    my %field_terms = (
        type        => 'field',
        object_type => 'entry',
        blog_id     => $blog_id,
        name        => $term->{field},
    );
    my $field = (MT->model('fdsetting')->load_with_default(\%field_terms, undef))[0] or return;
    my $field_type = $field->data->{'type'};

    # Parse field value into terms
    my $query = $term->{term};
    if ( 'PHRASE' eq $term->{query} ) {
        $query =~ s/'/"/g;
    }

    # No searching by fdvalue_id
    # my $can_search_by_id = $query =~ /^[0-9]*$/ ? 1 : 0;

    require Lucene::QueryParser;
    my $lucene_struct = Lucene::QueryParser::parse_query($query);
    if ( 'PROHIBITED' eq $term->{type} ) {
        $_->{type} = 'PROHIBITED' foreach @$lucene_struct;
    }

    my $match;
    if ( $field_type =~ /^Text(?:Area)?$/ ) {
        # Partial match for text and textarea fields
        $match = 'like';
    } else {
        # Exact match for all other types of fields
        $match = 1;
    }

    my ($terms)
        = $app->_query_parse_core( $lucene_struct, {
            # No searching by fdvalue_id
            # ( $can_search_by_id ? ( id => 1 ) : () ),
            value      => $match,
            value_text => $match,
            },
        {} );
    return unless $terms && @$terms;

    push @$terms, '-and', { 'key' => $term->{field} };
    push @$terms, '-and', { 'object_id' => \"= $id_col", }; #"
    push @$terms, '-and', { 'object_type' => $ot->{'object_mt_type'} || $ot->{'object_type'} };

    require FieldDay::Value;
    return FieldDay::Value->join_on(
        undef, 
        $terms,
        {
            'unique' => 1,
            'alias'  => $term->{field},
        }
    );
}

1;
