# rebar3_checkshell [![Erlang CI][ci-img]][ci]

[ci]: https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/actions
[ci-img]: https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/actions/workflows/erlang.yml/badge.svg

`rebar3_checkshell` is a `rebar3` plugin that performs
[ShellCheck](https://github.com/koalaman/shellcheck) static analysis on your scripts, while not
having to install `shellcheck` locally.

## The plugin

### Purpose

The need for this plugin grew out of wanting to have a team of developers perform static analysis
on their `bash`/`sh` scripts without having to install and keep updating versions of `shellcheck`.
As an added bonus, a CI/CD system like GitHub can also benefit from this: no need to install
`shellcheck` or keep it updated.

### Usage

Add a `project_plugins` element to your `rebar.config`:

```erlang
{project_plugins, [rebar3_checkshell]}.
```

### Configuration

In your `rebar.config`, add section `checkshell` and within it the options described below.
There are no compulsory options (all defaults are those assumed by `shellcheck`).

```erlang
{checkshell, [
    % Include warnings from sourced files
    check_sourced,

    % Use color (auto, always, never)
    {color, auto | always | never},

    % Consider only given types of warnings
    {include, ["CODE1", "CODE2"]},

    % Exclude types of warnings
    {exclude, ["CODE1", "CODE2"]},

    % Output format (checkstyle, diff, gcc, json, json1, quiet, tty)
    {format, checkstyle | diff | gcc | json | json1 | quiet | tty},

    % List checks disabled by default
    list_optional,

    % Don't look for .shellcheckrc files
    norc,

    % List of optional checks to enable (or 'all')
    {enable, ["check1", "check2"] | all},

    % Specify path when looking for sourced files ("SCRIPTDIR" for script's dir)
    {source_paths, "SOURCEPATHS"},

    % Specify dialect (sh, bash, dash, ksh)
    {shell, sh | bash | dash | ksh},

    % Minimum severity of errors to consider (error, warning, info, style)
    {severity, error | warning | info | style},

    % The number of wiki links to show, when applicable
    {wiki_link_count, NUM},

    % Allow 'source' outside of FILES
    external_sources
]}.
```

### Command line options

The only accepted (and compulsory) command option is `--files`, used as

```console
rebar3 checkshell --files=scripts/*.sh
```

### Check it out

```console
===> Compiling rebar3_checkshell
In script.sh line 2:
VAR=none
^-- SC2034: VAR appears unused. Verify use (or export if used externally).

For more information:
  https://www.shellcheck.net/wiki/SC2034 -- A appears unused. Verify use (or ...
===> checkshell: ShellCheck exited with error
```

### Ignoring issues

(as per [ShellCheck](https://github.com/koalaman/shellcheck#ignoring-issues)'s GitHub `README.md`)
> Issues can be ignored via environmental variable, command line, individually or globally within
> a file:
> <https://github.com/koalaman/shellcheck/wiki/Ignore>

## ShellCheck

### Checks

You'll find a complete list of checks under
[ShellCheck - Checks](https://github.com/koalaman/shellcheck/wiki/Checks). This is the canonical
Wiki; it is not maintained by this plugin's maintainers.

### Restrictions

Since we're bundling the binary elements for ShellCheck analysis (and they are obtained from
pre-compiled sources, online, for a given "latest" version), it is possible that the analysis
won't executed out-of-the-box and exit with an uncontrolled error. If fixing that issue is really
important to you, [open an issue](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/issues)
so we can find a solution.

**Note**: to verify that the downloaded files haven't been tampered with, go to `priv`, study
the content of `update.sh` and then run it when you're ready. You'll see that we download the
files to specific folder but don't touch them otherwise. Also, doing this on a non-tampered version
should yield no changes to the file under Git source control.

## The project

### Changelog

A complete changelog can be found under [CHANGELOG.md](CHANGELOG.md).

### Code of Conduct

This project's code of conduct is made explicit in [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

### Contributing

First of all, thank you for contributing with your time and patience.

If you want to request a new feature make sure to
[open an issue](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/issues) so we can
discuss it first.

Bug reports and questions are also welcome, but do check you're using the latest version of the
plugin - if you found a bug - and/or search the issue database - if you have a question, since it
might have already been answered before.

Contributions will be subject to GNU General Public License 3.0.
You will retain the copyright.

For more information check out [CONTRIBUTING.md](CONTRIBUTING.md).

## License

License information can be found inside [LICENSE.md](LICENSE.md).

## Security

This project's security policy is made explicit in [SECURITY.md](SECURITY.md).
