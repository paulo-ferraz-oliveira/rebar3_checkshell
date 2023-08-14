% @private
-module(rebar3_checkshell_inst).

-include("rebar3_checkshell.hrl").

-export([put_executables/1]).
-export([shellcheck_path/1]).

-define(SHELLCHECK_VERSION, "v0.9.0").
-elvis([{elvis_style, operator_spaces, disable}]).
-define(DARWIN_CHECKSUM,
    <<253, 31, 54, 249, 47, 253, 173, 108, 34, 206, 20, 25, 167, 196, 78, 129>>
).
-define(LINUX_CHECKSUM,
    <<35, 100, 38, 131, 239, 213, 185, 125, 148, 117, 168, 245, 248, 158, 25, 107>>
).
-define(WIN32_CHECKSUM,
    <<167, 119, 58, 62, 90, 217, 211, 132, 219, 212, 190, 41, 13, 219, 120, 241>>
).

-export_type([nonempty_ubytes/0]).

-spec put_executables(State) -> Result when
    State :: rebar_state:t(),
    Result :: ok | {error, nonempty_ubytes()}.
put_executables(State) ->
    ArchCacheDirExists = filelib:is_dir(arch_cache_dir()),
    CacheDirResult = mkdir_arch_cache(ArchCacheDirExists),

    VsnCacheDirExists = filelib:is_dir(vsn_cache_dir(State)),
    VsnDirResult = mkdir_vsn_cache(VsnCacheDirExists, CacheDirResult, State),

    CompressedTargetExists = filelib:is_file(compressed_target(State)),
    DownloadAndWriteResult = download_and_write(CompressedTargetExists, VsnDirResult, State),

    ExpandedExists = filelib:is_file(shellcheck_path(State)),
    ExpandResult = expand(ExpandedExists, DownloadAndWriteResult, State),
    CheckSummed = rebar3_checkshell_prv:opt(State, checksum, should_checksum(State)),
    checksum(CheckSummed, ExpandResult, State).

-spec compressed_target(State) -> Result when
    State :: rebar_state:t(),
    Result :: string().
compressed_target(State) ->
    filename:join(vsn_cache_dir(State), installer_name(State)).

% supertype
-dialyzer({nowarn_function, arch_folder_name/0}).
-spec arch_folder_name() -> Result when
    Result :: nonempty_string().
arch_folder_name() ->
    arch_folder_name_for(rebar3_checkshell_arch:t()).

% supertype
-dialyzer({nowarn_function, arch_folder_name_for/1}).
-spec arch_folder_name_for(Arch) -> Result when
    Arch :: rebar3_checkshell_arch:t(),
    Result :: nonempty_string().
arch_folder_name_for(darwin) ->
    "darwin.x86_64";
arch_folder_name_for(linux) ->
    "linux.x86_64";
arch_folder_name_for(win32) ->
    "windows.x86".

-spec checkshell_arch_folder_name() -> Result when
    Result :: string().
checkshell_arch_folder_name() ->
    filename:join("checkshell", arch_folder_name()).

-spec global_cache_dir() -> Result when
    Result :: string().
global_cache_dir() ->
    rebar_dir:global_cache_dir(dict:new()).

-spec arch_cache_dir() -> Result when
    Result :: string().
arch_cache_dir() ->
    filename:join(global_cache_dir(), checkshell_arch_folder_name()).

-spec vsn_cache_dir(State) -> Result when
    State :: rebar_state:t(),
    Result :: string().
vsn_cache_dir(State) ->
    {_IsDefaultVsn, Vsn} = vsn(State),
    filename:join(arch_cache_dir(), Vsn).

-spec vsn(State) -> Result when
    State :: rebar_state:t(),
    Result :: {IsDefaultVsn :: boolean(), Vsn :: string()}.
vsn(State) ->
    DefaultVsn = ?SHELLCHECK_VERSION,
    Vsn = rebar3_checkshell_prv:opt(State, vsn, DefaultVsn),
    {Vsn =:= DefaultVsn, Vsn}.

-spec shellcheck_path(State) -> Result when
    State :: rebar_state:t(),
    Result :: string().
shellcheck_path(State) ->
    filename:join(vsn_cache_dir(State), executable()).

% supertype
-dialyzer({nowarn_function, executable/0}).
-spec executable() -> Result when
    Result :: nonempty_string().
executable() ->
    executable_for(rebar3_checkshell_arch:t()).

% supertype
-dialyzer({nowarn_function, executable_for/1}).
-spec executable_for(Arch) -> Result when
    Arch :: rebar3_checkshell_arch:t(),
    Result :: nonempty_ubytes().
executable_for(darwin) ->
    "shellcheck";
executable_for(linux) ->
    "shellcheck";
executable_for(win32) ->
    "shellcheck.exe".

-spec mkdir_arch_cache(Exists) -> Result when
    Exists :: boolean(),
    Result :: ok | {error, file:posix()}.
mkdir_arch_cache(true = _Exists) ->
    _ = rebar3_checkshell_utils:log(debug, "arch. cache dir exists", []),
    ok;
mkdir_arch_cache(false = _Exists) ->
    ArchFolderName = arch_folder_name(),
    ArchCacheDir = arch_cache_dir(),
    _ = rebar3_checkshell_utils:log(
        info, "creating cache/arch. dir (arch: ~p) at ~p", [
            ArchFolderName, ArchCacheDir
        ]
    ),
    filelib:ensure_path(ArchCacheDir).

-spec mkdir_vsn_cache(Exists, CacheDirResult, State) -> Result when
    Exists :: boolean(),
    CacheDirResult :: ok | {error, file:posix()},
    State :: rebar_state:t(),
    Result :: ok | {error, file:posix()}.
mkdir_vsn_cache(true = _Exists, _CacheDirResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "vsn cache dir exists", []),
    ok;
mkdir_vsn_cache(false = _Exists, {error, _FilePosix} = CacheDirResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "(vsn cache) prior error ~p", [
        CacheDirResult
    ]),
    CacheDirResult;
mkdir_vsn_cache(false = _Exists, ok = _CacheDirResult, State) ->
    VsnCacheDir = vsn_cache_dir(State),
    _ = rebar3_checkshell_utils:log(info, "creating cache/version dir at ~p", [
        VsnCacheDir
    ]),
    filelib:ensure_path(VsnCacheDir).

-spec download_url(State) -> Result when
    State :: rebar_state:t(),
    Result :: nonempty_ubytes().
download_url(State) ->
    {_IsDefaultVsn, Vsn} = vsn(State),
    "https://github.com/koalaman/shellcheck/releases/download/" ++ Vsn ++ "/" ++
        installer_name(State).

-spec download_and_write(CompressedTargetExists, VsnDirResult, State) -> Result when
    CompressedTargetExists :: boolean(),
    VsnDirResult :: ok | {error, file:posix()},
    State :: rebar_state:t(),
    Result :: ok | {error, file:posix()}.
download_and_write(true = _CompressedTargetExists, _VsnDirResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "compressed target exists", []),
    ok;
download_and_write(false = _CompressedTargetExists, {error, _FilePosix} = VsnDirResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "(download and write) prior error ~p", [
        VsnDirResult
    ]),
    VsnDirResult;
download_and_write(false = _CompressedTargetExists, ok = _VsnDirResult, State) ->
    URL = download_url(State),
    VsnCacheDir = vsn_cache_dir(State),
    HttpHeaders = [],
    HttpOptions = [{ssl, tls_certificate_check:options(URL)}],
    Options = [{body_format, binary}],
    _ = rebar3_checkshell_utils:log(info, "downloading ~p to ~p", [URL, VsnCacheDir]),
    {ok, {{_HttpVersion, 200, _Status}, _HttpHeaders, HttpBodyResult}} = httpc:request(
        get, {URL, HttpHeaders}, HttpOptions, Options
    ),

    CompressedTarget = compressed_target(State),
    ok = file:write_file(CompressedTarget, HttpBodyResult).

-spec should_checksum(State) -> Result when
    State :: rebar_state:t(),
    Result :: boolean().
should_checksum(State) ->
    {IsDefaultVsn, _Vsn} = vsn(State),
    IsDefaultVsn.

-spec checksum(CheckSummed, ExpandResult, State) -> Result when
    CheckSummed :: boolean(),
    ExpandResult :: ok | {error, file:posix()},
    State :: rebar_state:t(),
    Result :: ok | {error, nonempty_ubytes()}.
checksum(false = _CheckSummed, _ExpandResult, _State) ->
    _ = rebar3_checkshell_utils:log(warn, "checksum bypass is ON", []),
    ok;
checksum(true = _CheckSummed, {error, FilePosix} = ExpandResult, _State) ->
    _ = rebar3_checkshell_utils:log(
        debug,
        "(checksum) prior error ~p",
        [ExpandResult]
    ),
    {error, "(check with DEBUG=1) " ++ atom_to_list(FilePosix)};
checksum(true = _CheckSummed, ok = _ExpandResult, State) ->
    {ok, ShellCheck} = file:read_file(shellcheck_path(State)),
    do_checksum(rebar3_checkshell_arch:t(), crypto:hash(md5, ShellCheck)).

-spec do_checksum(Arch, Checksum) -> Result when
    Arch :: rebar3_checkshell_arch:t(),
    Checksum :: binary(),
    Result :: ok | {error, nonempty_ubytes()}.
do_checksum(Arch, Checksum) when
    (Arch =:= darwin andalso Checksum =:= ?DARWIN_CHECKSUM) orelse
        (Arch =:= linux andalso Checksum =:= ?LINUX_CHECKSUM) orelse
        (Arch =:= win32 andalso Checksum =:= ?WIN32_CHECKSUM)
->
    _ = rebar3_checkshell_utils:log(debug, "checksum is Ok for arch. ~p", [Arch]),
    ok;
do_checksum(_Arch, _Expected) ->
    {error, "invalid executable checksum"}.

-spec expand(ExpandedExists, DownloadAndWriteResult, State) -> Result when
    ExpandedExists :: boolean(),
    DownloadAndWriteResult :: ok | {error, file:posix()},
    State :: rebar_state:t(),
    Result :: ok | {error, file:posix()}.
expand(ExpandedExists, DownloadAndWriteResult, State) ->
    expand_for(ExpandedExists, DownloadAndWriteResult, State).

-spec expand_for(ExpandedExists, DownloadAndWriteResult, State) -> Result when
    ExpandedExists :: boolean(),
    DownloadAndWriteResult :: ok | {error, file:posix()},
    State :: rebar_state:t(),
    Result :: ok | {error, file:posix()}.
expand_for(true = _ExpandedExists, _DownloadAndWriteResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "expanded target exists", []),
    ok;
expand_for(false = _ExpandedExists, {error, _Result} = DownloadAndWriteResult, _State) ->
    _ = rebar3_checkshell_utils:log(debug, "(expand for) prior error ~p", [
        DownloadAndWriteResult
    ]),
    DownloadAndWriteResult;
expand_for(false = _ExpandedExists, ok = _DownloadAndWriteResult, State) ->
    FileType = file_type(),
    CompressedTarget = compressed_target(State),
    VsnCacheDir = vsn_cache_dir(State),
    _ = rebar3_checkshell_utils:log(info, "extracting executable to ~p", [VsnCacheDir]),
    do_expand_for(FileType, CompressedTarget, VsnCacheDir).

-spec do_expand_for(FileType, CompressedTarget, TargetDir) -> Result when
    FileType :: tar_xz | zip,
    CompressedTarget :: string(),
    TargetDir :: string(),
    Result :: ok.
do_expand_for(tar_xz, CompressedTarget, TargetDir) ->
    Cmd = "tar",
    Args = ["xf", CompressedTarget, "--strip-components", "1", "--directory", TargetDir],
    {0, _Data} = rebar3_checkshell_utils:cmd(Cmd, Args),
    ok;
do_expand_for(zip, CompressedTarget, TargetDir) ->
    Options = [{cwd, TargetDir}],
    {ok, _Data} = zip:unzip(CompressedTarget, Options),
    ok.

-spec file_type() -> Result when
    Result :: tar_xz | zip.
file_type() ->
    file_type_for(rebar3_checkshell_arch:t()).

-spec file_type_for(Arch) -> Result when
    Arch :: rebar3_checkshell_arch:t(),
    Result :: tar_xz | zip.
file_type_for(darwin) ->
    tar_xz;
file_type_for(linux) ->
    tar_xz;
file_type_for(win32) ->
    zip.

-spec installer_name(State) -> Result when
    State :: rebar_state:t(),
    Result :: nonempty_ubytes().
installer_name(State) ->
    installer_name_for(rebar3_checkshell_arch:t(), State).

-spec installer_name_for(Arch, State) -> Result when
    State :: rebar_state:t(),
    Arch :: rebar3_checkshell_arch:t(),
    Result :: nonempty_ubytes().
installer_name_for(darwin, State) ->
    {_IsDefaultVsn, Vsn} = vsn(State),
    "shellcheck-" ++ Vsn ++ ".darwin.x86_64.tar.xz";
installer_name_for(linux, State) ->
    {_IsDefaultVsn, Vsn} = vsn(State),
    "shellcheck-" ++ Vsn ++ ".linux.x86_64.tar.xz";
installer_name_for(win32, State) ->
    {_IsDefaultVsn, Vsn} = vsn(State),
    "shellcheck-" ++ Vsn ++ ".zip".
