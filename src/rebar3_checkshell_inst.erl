-module(rebar3_checkshell_inst).

-include("rebar3_checkshell.hrl").

-export([put_executables/0]).
-export([shellcheck_path/0]).

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

-spec put_executables() -> Result when
    Result :: ok | {error, nonempty_ubytes()}.
put_executables() ->
    ArchCacheDirExists = filelib:is_dir(arch_cache_dir()),
    CacheDirResult = mkdir_arch_cache(ArchCacheDirExists),

    VsnCacheDirExists = filelib:is_dir(vsn_cache_dir()),
    VsnDirResult = mkdir_vsn_cache(VsnCacheDirExists, CacheDirResult),

    CompressedTargetExists = filelib:is_file(compressed_target()),
    DownloadAndWriteResult = download_and_write(CompressedTargetExists, VsnDirResult),

    ExpandedExists = filelib:is_file(shellcheck_path()),
    ExpandResult = expand(ExpandedExists, DownloadAndWriteResult),

    CheckSummed = application:get_env(checkshell, checksum, true),
    checksum(CheckSummed, ExpandResult).

-spec compressed_target() -> Result when
    Result :: string().
compressed_target() ->
    filename:join(vsn_cache_dir(), installer_name()).

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

-spec vsn_cache_dir() -> Result when
    Result :: string().
vsn_cache_dir() ->
    filename:join(arch_cache_dir(), ?SHELLCHECK_VERSION).

-spec shellcheck_path() -> Result when
    Result :: string().
shellcheck_path() ->
    filename:join(vsn_cache_dir(), executable()).

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
    _ = rebar_log:log(debug, "checkshell: arch. cache dir exists", []),
    ok;
mkdir_arch_cache(false = _Exists) ->
    ArchFolderName = arch_folder_name(),
    ArchCacheDir = arch_cache_dir(),
    _ = rebar_log:log(info, "checkshell: creating cache/arch. dir (arch: ~p) at ~p", [
        ArchFolderName, ArchCacheDir
    ]),
    filelib:ensure_path(ArchCacheDir).

-spec mkdir_vsn_cache(Exists, CacheDirResult) -> Result when
    Exists :: boolean(),
    CacheDirResult :: ok | {error, file:posix()},
    Result :: ok | {error, file:posix()}.
mkdir_vsn_cache(true = _Exists, _CacheDirResult) ->
    _ = rebar_log:log(debug, "checkshell: vsn cache dir exists", []),
    ok;
mkdir_vsn_cache(false = _Exists, {error, _FilePosix} = CacheDirResult) ->
    _ = rebar_log:log(debug, "checkshell: (vsn cache) prior error ~p", [CacheDirResult]),
    CacheDirResult;
mkdir_vsn_cache(false = _Exists, ok = _CacheDirResult) ->
    VsnCacheDir = vsn_cache_dir(),
    _ = rebar_log:log(info, "checkshell: creating cache/version dir at ~p", [VsnCacheDir]),
    filelib:ensure_path(VsnCacheDir).

-spec download_url() -> Result when
    Result :: nonempty_ubytes().
download_url() ->
    "https://github.com/koalaman/shellcheck/releases/download/" ++ ?SHELLCHECK_VERSION ++ "/" ++
        installer_name().

-spec download_and_write(CompressedTargetExists, VsnDirResult) -> Result when
    CompressedTargetExists :: boolean(),
    VsnDirResult :: ok | {error, file:posix()},
    Result :: ok | {error, file:posix()}.
download_and_write(true = _CompressedTargetExists, _VsnDirResult) ->
    _ = rebar_log:log(debug, "checkshell: compressed target exists", []),
    ok;
download_and_write(false = _CompressedTargetExists, {error, _FilePosix} = VsnDirResult) ->
    _ = rebar_log:log(debug, "checkshell: (download and write) prior error ~p", [VsnDirResult]),
    VsnDirResult;
download_and_write(false = _CompressedTargetExists, ok = _VsnDirResult) ->
    URL = download_url(),
    VsnCacheDir = vsn_cache_dir(),
    HttpHeaders = [],
    HttpOptions = [{ssl, tls_certificate_check:options(URL)}],
    Options = [{body_format, binary}],
    _ = rebar_log:log(info, "checkshell: downloading ~p to ~p", [URL, VsnCacheDir]),
    {ok, {{_HttpVersion, 200, _Status}, _HttpHeaders, HttpBodyResult}} = httpc:request(
        get, {URL, HttpHeaders}, HttpOptions, Options
    ),

    CompressedTarget = compressed_target(),
    ok = file:write_file(CompressedTarget, HttpBodyResult).

-spec checksum(CheckSummed, ExpandResult) -> Result when
    CheckSummed :: boolean(),
    ExpandResult :: ok | {error, file:posix()},
    Result :: ok | {error, nonempty_ubytes()}.
checksum(false = _CheckSummed, _ExpandResult) ->
    _ = rebar_log:log(warn, "checkshell: checksum bypass is ON", []),
    ok;
checksum(true = _CheckSummed, {error, FilePosix} = ExpandResult) ->
    _ = rebar_log:log(debug, "checkshell: (expand for) prior error ~p", [ExpandResult]),
    {error, "(check with DEBUG=1) " ++ atom_to_list(FilePosix)};
checksum(true = _CheckSummed, ok = _ExpandResult) ->
    {ok, ShellCheck} = file:read_file(shellcheck_path()),
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
    _ = rebar_log:log(debug, "checkshell: checksum is Ok for arch. ~p", [Arch]),
    ok;
do_checksum(_Arch, _Expected) ->
    {error, "invalid executable checksum"}.

-spec expand(ExpandedExists, DownloadAndWriteResult) -> Result when
    ExpandedExists :: boolean(),
    DownloadAndWriteResult :: ok | {error, file:posix()},
    Result :: ok | {error, file:posix()}.
expand(ExpandedExists, DownloadAndWriteResult) ->
    expand_for(ExpandedExists, DownloadAndWriteResult).

-spec expand_for(ExpandedExists, DownloadAndWriteResult) -> Result when
    ExpandedExists :: boolean(),
    DownloadAndWriteResult :: ok | {error, file:posix()},
    Result :: ok | {error, file:posix()}.
expand_for(true = _ExpandedExists, _DownloadAndWriteResult) ->
    _ = rebar_log:log(debug, "checkshell: expanded target exists", []),
    ok;
expand_for(false = _ExpandedExists, {error, _Result} = DownloadAndWriteResult) ->
    _ = rebar_log:log(debug, "checkshell: (expand for) prior error ~p", [DownloadAndWriteResult]),
    DownloadAndWriteResult;
expand_for(false = _ExpandedExists, ok = _DownloadAndWriteResult) ->
    FileType = file_type(),
    CompressedTarget = compressed_target(),
    VsnCacheDir = vsn_cache_dir(),
    _ = rebar_log:log(info, "checkshell: extracting executable to ~p", [VsnCacheDir]),
    do_expand_for(FileType, CompressedTarget, VsnCacheDir).

-spec do_expand_for(FileType, CompressedTarget, TargetDir) -> Result when
    FileType :: tar_xz | zip,
    CompressedTarget :: string(),
    TargetDir :: string(),
    Result :: ok.
do_expand_for(tar_xz, CompressedTarget, TargetDir) ->
    Cmd = "tar",
    Args = ["zxf", CompressedTarget, "--strip-components", "1", "--directory", TargetDir],
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

-spec installer_name() -> Result when
    Result :: nonempty_ubytes().
installer_name() ->
    installer_name_for(rebar3_checkshell_arch:t()).

-spec installer_name_for(Arch) -> Result when
    Arch :: rebar3_checkshell_arch:t(),
    Result :: nonempty_ubytes().
installer_name_for(darwin) ->
    "shellcheck-" ++ ?SHELLCHECK_VERSION ++ ".darwin.x86_64.tar.xz";
installer_name_for(linux) ->
    "shellcheck-" ++ ?SHELLCHECK_VERSION ++ ".linux.x86_64.tar.xz";
installer_name_for(win32) ->
    "shellcheck-" ++ ?SHELLCHECK_VERSION ++ ".zip".
