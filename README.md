# Field Day, a plugin for Movable Type

* Author: MT5 port by Ubiquitic, original code from Six Apart
* Copyright: 2012 Ubiquitic, 2008-2010 Six Apart Ltd.
* License: GPL
* Site: <https://github.com/padawan/mt-plugin-field-day/>

## Compatibility

Requirement: MT 5.2.x

This is the MT5 port of FieldDay by Ubiquitic. *It will NOT work on MT4*. If you are looking for the MT4 version, check <https://github.com/movabletype/mt-plugin-field-day> instead.
Please report issues here to help improve this plugin.


## Overview

FieldDay is a plugin for Movable Type that lets you add more fields to the MT
interface.


## Features

How is Field Day different from MT's built-in "Commercial Pack" implementation
of custom fields?

* You can define fields for templates, assets, comments, and blogs, as well as
  system-wide fields.
* Linked object field types let you connect any supported object type to any
  other.
* By organizing fields into groups and allowing multiple instances of each
  group, you can allow users to associate an unlimited amount of data with a
  given object.


## Documentation

* Field Day Basics: <http://github.com/movabletype/mt-plugin-field-day/wiki/Basics>
* Field Day Developer Notes: <http://github.com/movabletype/mt-plugin-field-day/wiki/Developer-Notes>


## Installation

1. Move the `FieldDay` plugin directory to the MT `plugins` directory.
2. Move the `FieldDay` mt-static directory to the `mt-static/plugins`
   directory.

Should look like this when installed:

    $MT_HOME/
        plugins/
            FieldDay/
                [plugin files here]
        mt-static/
            plugins/
                FieldDay/
                    [plugin static files here]


## Release notes

Version: 1.5.4 (current)
Changes in this version:
* Better exception handling when an undefined group in used in a IfFieldGroup tag (authors: @jayallen, @CNG, see pull request https://github.com/padawan/mt-plugin-field-day/pull/16)