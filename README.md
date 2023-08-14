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
    % Source files to consider for analysis
    {files, ["FILE1", "FILE2"]},

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

    % Don't look for .shellcheckrc files
    norc,

    % List of optional checks to enable (or 'all')
    {enable, ["check1", "check2"] | all},

    % Specify path when looking for sourced files ("SCRIPTDIR" for script's dir)
    {source_path, "SOURCEPATH"},

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

The only accepted command option is `--files`, used as

```console
rebar3 checkshell --files=scripts/*.sh
```

Since `files` can also come from `rebar.config`, the plugin makes an effort to merge both lists
and apply the analysis on top of that.

### Check it out

<!-- markdownlint-disable MD013 -->
```console
===> Compiling rebar3_checkshell
===> checkshell: analysis starting. This may take a while...
In sh/fish.sh line 1:
#!/usr/bin/env fish
^-- SC1008 (error): This shebang was unrecognized. ShellCheck only supports sh/bash/dash/ksh. Add a 'shell' directive to specify.

For more information:
  https://www.shellcheck.net/wiki/SC1008 -- This shebang was unrecognized. Sh...
===> checkshell: ShellCheck exited with error
```
<!-- markdownlint-enable -->

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

### Downloading and caching executables

Executables are downloaded and cached inside `~/.cache/rebar3/checkshell`, per version and
platform. As long as the version doesn't change or you don't delete the sources, they are only
downloaded once and subsequently reused.

**Note**: to verify that the downloaded files haven't been tampered with, we implement a basic
MD5-based checksum (using `crypto`) against the files we're about to execute. If the checksums
don't coincide, the execution is aborted. You can override this by setting option
`{checkshell, [{checksum, false}]}.` in `rebar.config`.

#### Choosing a ShellCheck version

The plugin ships with its own rules for downloading, caching and checksumming a specific ShellCheck
**version**. You can override this by setting option `{checkshell, [{vsn, "v0.9.0"}]}.` in
`rebar.config`, in which case the plugin will warn you that the check is turned off.

**Note**: using a ShellCheck version different from the one the plugin targets by default may
introduce unexpected error (also, new versions might bring e.g. new options that won't be available
for consumption out-of-the-box). In this case, feel free to pull request, or open a GitHub issue
to discuss moving forward.

## The project

### Changelog

A complete changelog can be found under [CHANGELOG.md](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/blob/main/CHANGELOG.md).

### Code of Conduct

This project's code of conduct is made explicit in [CODE_OF_CONDUCT.md](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/blob/main/CODE_OF_CONDUCT.md).

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

For more information check out [CONTRIBUTING.md](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/blob/main/CONTRIBUTING.md).

### License

License information can be found inside [LICENSE](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/blob/main/LICENSE).

### Security

This project's security policy is made explicit in [SECURITY.md](https://github.com/paulo-ferraz-oliveira/rebar3_checkshell/blob/main/SECURITY.md).
