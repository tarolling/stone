# Tests install.ps1 against a local release built from target\debug\stone.exe.
#
# Usage, from the repository root after `cargo build`:
#
#   pwsh scripts/test-install.ps1
#
# It packages the binary the way release.yml does, installs it through
# install.ps1 with STONE_DOWNLOAD_URL pointing at a file:// directory, runs an
# example with the installed binary, uninstalls one, and checks that a bad
# checksum is rejected. It never changes the user's PATH.
$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent $PSScriptRoot
$binary = Join-Path $root 'target\debug\stone.exe'
if (-not (Test-Path -LiteralPath $binary)) {
    throw "error: $binary not found, run cargo build first"
}

$work = Join-Path ([IO.Path]::GetTempPath()) ("stone-test-install-" + [Guid]::NewGuid())
New-Item -ItemType Directory -Path "$work\package", "$work\release" | Out-Null
$env:STONE_NO_MODIFY_PATH = '1'

# runs install.ps1 in a shell of its own, as `irm | iex` would, and returns whether it succeeded
function Invoke-Install([string[]]$Arguments, [string]$ErrorFile) {
    $shell = (Get-Process -Id $PID).Path
    $command = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', "$root\install.ps1") + $Arguments
    # Windows PowerShell turns each line of a native command's redirected stderr into an error,
    # which Stop would make fatal; this applies only inside the function
    $ErrorActionPreference = 'Continue'
    # the child's output goes to the host, since anything left in the output stream would join
    # the returned value and make a failure look like success
    if ($ErrorFile) {
        & $shell @command 2> $ErrorFile | Out-Host
    } else {
        & $shell @command | Out-Host
    }
    return $LASTEXITCODE -eq 0
}

try {
    Copy-Item -LiteralPath $binary, "$root\LICENSE", "$root\README.md" -Destination "$work\package"
    $archive = 'stone-x86_64-windows.zip'
    Compress-Archive -Path "$work\package\*" -DestinationPath "$work\release\$archive"
    $hash = (Get-FileHash -LiteralPath "$work\release\$archive" -Algorithm SHA256).Hash.ToLower()
    Set-Content -LiteralPath "$work\release\$archive.sha256" -Value "$hash  $archive"
    $env:STONE_DOWNLOAD_URL = ([Uri]"$work\release").AbsoluteUri

    Write-Host 'test: installs and runs an example'
    $env:STONE_INSTALL_DIR = "$work\bin"
    if (-not (Invoke-Install @())) { throw 'error: install.ps1 failed' }
    $printed = (& "$work\bin\stone.exe" run "$root\examples\basics.stn") -join "`n"
    $expected = (Get-Content -LiteralPath "$root\examples\basics.out" -Raw).TrimEnd("`n")
    if ($printed -ne $expected) { throw "error: basics printed`n$printed" }

    Write-Host 'test: installing again replaces the binary'
    if (-not (Invoke-Install @())) { throw 'error: installing again failed' }
    & "$work\bin\stone.exe" --version
    Remove-Item Env:STONE_INSTALL_DIR

    Write-Host 'test: -Dir overrides the install directory'
    if (-not (Invoke-Install @('-Dir', "$work\other"))) { throw 'error: install.ps1 -Dir failed' }
    & "$work\other\stone.exe" --version

    Write-Host 'test: stone uninstall removes the installed binary'
    & "$work\other\stone.exe" uninstall --yes
    if ($LASTEXITCODE -ne 0) { throw 'error: stone uninstall failed' }
    if (Test-Path -LiteralPath "$work\other\stone.exe") { throw 'error: stone.exe is still there' }

    Write-Host 'test: rejects a checksum mismatch'
    Set-Content -LiteralPath "$work\release\$archive.sha256" -Value ('0' * 64 + '  x')
    $env:STONE_INSTALL_DIR = "$work\bad"
    if (Invoke-Install @() "$work\stderr.txt") { throw 'error: install.ps1 accepted a bad checksum' }
    if (-not (Select-String -LiteralPath "$work\stderr.txt" -Pattern 'checksum' -Quiet)) {
        throw "error: no checksum message in $(Get-Content -LiteralPath "$work\stderr.txt" -Raw)"
    }
    if (Test-Path -LiteralPath "$work\bad\stone.exe") { throw 'error: a bad download was installed' }
    Remove-Item Env:STONE_INSTALL_DIR

    Write-Host 'test: rejects an unknown option'
    if (Invoke-Install @('-Bogus') "$work\stderr.txt") { throw 'error: install.ps1 accepted -Bogus' }

    Write-Host 'all install tests passed'
} finally {
    # the uninstalled binary's helper may still be deleting it
    Start-Sleep -Seconds 3
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
# the last install was meant to fail, and the CI shell exits with its $LASTEXITCODE otherwise
exit 0
