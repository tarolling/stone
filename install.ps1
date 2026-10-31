# Installs a prebuilt stone binary on Windows from the GitHub releases.
#
#   irm https://raw.githubusercontent.com/tarolling/stone/main/install.ps1 | iex
#
# Piped into iex it takes its options from the environment (STONE_VERSION,
# STONE_INSTALL_DIR, STONE_DOWNLOAD_URL, STONE_NO_MODIFY_PATH); saved as a file
# it also takes them as parameters, e.g. `.\install.ps1 -Version v0.4.0`.
# Everything runs inside Install-Stone, called on the last line, so a partial
# download never runs half a script, and failures throw rather than exit, so
# they never close the shell that ran iex.
[CmdletBinding()]
param(
    [string]$Version = $(if ($env:STONE_VERSION) { $env:STONE_VERSION } else { 'latest' }),
    [string]$Dir = $(if ($env:STONE_INSTALL_DIR) { $env:STONE_INSTALL_DIR } else { '' }),
    [switch]$NoModifyPath = [bool]$env:STONE_NO_MODIFY_PATH
)

function Write-Step([string]$Message) {
    Write-Host "stone-install: $Message"
}

# prints the name of this machine that release archives use
function Get-Target {
    switch ($env:PROCESSOR_ARCHITECTURE) {
        'AMD64' { return 'x86_64-windows' }
        # Windows on Arm runs x86-64 programs, stone's own included
        'ARM64' {
            Write-Step "note: installing the x86-64 build, which Windows on Arm runs emulated"
            return 'x86_64-windows'
        }
        default {
            throw "stone-install: error: no prebuilt stone for $($env:PROCESSOR_ARCHITECTURE); build it from source with`n    cargo install --git https://github.com/tarolling/stone stone"
        }
    }
}

# downloads $Url, which may also be a file:// URL, to the file $OutFile
function Get-Download([string]$Url, [string]$OutFile) {
    $uri = [Uri]$Url
    if ($uri.IsFile) {
        Copy-Item -LiteralPath $uri.LocalPath -Destination $OutFile
    } else {
        # without the progress bar, Windows PowerShell downloads many times faster
        $ProgressPreference = 'SilentlyContinue'
        Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $OutFile
    }
}

# checks that the file $File matches the checksum file $Checksum
function Test-Checksum([string]$File, [string]$Checksum) {
    $expected = ((Get-Content -LiteralPath $Checksum -Raw).Trim() -split '\s+')[0]
    $actual = (Get-FileHash -LiteralPath $File -Algorithm SHA256).Hash
    # -ne ignores case, and Get-FileHash writes capitals where sha256sum writes lowercase
    if ($expected -ne $actual) {
        throw "stone-install: error: checksum mismatch for $(Split-Path -Leaf $File): expected $expected, got $($actual.ToLower())"
    }
}

function Install-Stone {
    $ErrorActionPreference = 'Stop'
    $dir = $Dir
    if (-not $dir) {
        $dir = Join-Path $env:LOCALAPPDATA 'Programs\stone\bin'
    }
    $target = Get-Target
    if ($env:STONE_DOWNLOAD_URL) {
        $base = $env:STONE_DOWNLOAD_URL
    } elseif ($Version -eq 'latest') {
        $base = 'https://github.com/tarolling/stone/releases/latest/download'
    } else {
        $base = "https://github.com/tarolling/stone/releases/download/$Version"
    }
    $archive = "stone-$target.zip"

    $tmp = Join-Path ([IO.Path]::GetTempPath()) ("stone-install-" + [Guid]::NewGuid())
    New-Item -ItemType Directory -Path $tmp | Out-Null
    try {
        Write-Step "downloading $base/$archive"
        try {
            Get-Download "$base/$archive" (Join-Path $tmp $archive)
            Get-Download "$base/$archive.sha256" (Join-Path $tmp "$archive.sha256")
        } catch {
            throw "stone-install: error: could not download $base/${archive}: $_"
        }
        Test-Checksum (Join-Path $tmp $archive) (Join-Path $tmp "$archive.sha256")
        Expand-Archive -LiteralPath (Join-Path $tmp $archive) -DestinationPath (Join-Path $tmp 'files')

        New-Item -ItemType Directory -Force -Path $dir | Out-Null
        $exe = Join-Path $dir 'stone.exe'
        # a running program cannot be overwritten, but it can be renamed out of the way
        if (Test-Path -LiteralPath $exe) {
            Remove-Item -LiteralPath "$exe.old" -Force -ErrorAction SilentlyContinue
            Move-Item -LiteralPath $exe -Destination "$exe.old" -Force
        }
        Copy-Item -LiteralPath (Join-Path $tmp 'files\stone.exe') -Destination $exe
        Remove-Item -LiteralPath "$exe.old" -Force -ErrorAction SilentlyContinue
    } finally {
        Remove-Item -LiteralPath $tmp -Recurse -Force -ErrorAction SilentlyContinue
    }
    Write-Step "installed $(& $exe --version) to $exe"
    Write-Step "upgrade it later with 'stone update', or remove it with 'stone uninstall'"

    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    $onPath = ($userPath -split ';') -contains $dir
    if (-not $onPath -and -not $NoModifyPath) {
        $joined = if ($userPath) { "$dir;$userPath" } else { $dir }
        [Environment]::SetEnvironmentVariable('Path', $joined, 'User')
        $env:Path = "$dir;$env:Path"
        Write-Step "added $dir to your PATH; open a new terminal to use stone there"
    } elseif (-not $onPath) {
        Write-Step "$dir is not on your PATH; add it to use stone by name"
    }
}

Install-Stone
