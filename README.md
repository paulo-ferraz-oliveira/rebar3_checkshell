rebar3_checkshell
=================

`rebar3_checkshell` is a `rebar3` plugin that performs
[ShellCheck](https://github.com/koalaman/shellcheck) static analysis on your scripts, while not
having to install `shellcheck` locally.

Purpose
-------

The need for this plugin grew out of wanting to have a team of developers perform static analysis
on their `bash`/`sh` scripts without having to install and keep updating versions of `shellcheck`.
As an added bonus, a CI/CD system like Jenkins can also benefit from this: no need to install
`shellcheck` or keep it updated.

Usage
-----

Add a `project_plugins` element to your `rebar.config`:

```erlang
{project_plugins, [rebar3_checkshell]}.
```

Configuration
-------------

In your `rebar.config` add section `checkshell` and within it a pattern for the files to analyze.

```erlang
TODO
```

Command line options
--------------------

The only accepted option is TODO

Check it out
------------

TODO: add example output

Ignoring issues
---------------

(as per [ShellCheck](https://github.com/koalaman/shellcheck#ignoring-issues)'s GitHub `README.md`)
> Issues can be ignored via environmental variable, command line, individually or globally within
> a file:
> https://github.com/koalaman/shellcheck/wiki/Ignore

Checks
------

You'll find a complete list of checks under
[ShellCheck - Checks](https://github.com/koalaman/shellcheck/wiki/Checks). This is the canonical
Wiki; it is not maintained by this plugin's maintainers.

Contributing
------------

First of all, thank you for contributing with your time and patience.

If you want to request a new feature make sure to
[open an issue](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/issues) so we can
discuss it first.

Bug reports and questions are also welcome, but do check you're using the latest version of the
plugin - if you found a bug - and/or search the issue database - if you have a question, since it
might have already been answered before.

Contributions will be subject to GNU General Public License 3.0.
You will retain the copyright.

Copyright
---------

`rebar3 checkshell` is licensed under GNU General Public License 3.0. A copy of this license is
included in file [LICENSE](LICENSE).

Copyright 2020, [Paulo F. Oliveira](https://github.com/paulo-ferraz-oliveira) and contributors.
