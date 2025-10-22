Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Write-Info {
  param([string]$Message)
  Write-Host 'info:' -ForegroundColor Blue -NoNewline
  Write-Host " $Message"
}

function Write-Success {
  param([string]$Message)
  Write-Host 'ok:' -ForegroundColor Green -NoNewline
  Write-Host " $Message"
}

function Write-Warn {
  param([string]$Message)
  Write-Host 'warn:' -ForegroundColor Yellow -NoNewline
  Write-Host " $Message"
}

function Write-Error {
  param([string]$Message)
  Write-Host 'error:' -ForegroundColor Red -NoNewline
  Write-Host " $Message"
  throw (New-Object System.Exception($Message))
}

$binName = 'naija'
$binExecutable = "${binName}.exe"
$installDirName = '.naijascript'
$installRoot = Join-Path $env:USERPROFILE $installDirName
$binRoot = Join-Path $installRoot 'bin'
$binPath = Join-Path $binRoot $binExecutable
$configFile = Join-Path $installRoot 'config.toml'
$versionDir = Join-Path $installRoot 'versions'
$repo = 'xosnrdev/naijascript'

Write-Info 'Checking environment...'
if (Test-Path -Path $binPath) {
  Write-Warn "NaijaScript interpreter already exists at: $binPath"
  exit 0
}

Write-Info 'Detecting OS...'
if ($env:OS -ne 'Windows_NT') {
  Write-Error "Your operating system ('$($env:OS)') is not supported. This script runs on Windows."
}

$arch = $env:PROCESSOR_ARCHITECTURE
switch ($arch) {
  'AMD64' { $target = 'x86_64-pc-windows-msvc' }
  'ARM64' { $target = 'aarch64-pc-windows-msvc' }
  default { Write-Error "Your architecture ('$arch') is not supported. This script requires x86_64 or aarch64." }
}

Write-Info 'Fetching the latest version...'
try {
  $latest = Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest"
  $latestTag = $latest.tag_name
}
catch {
  Write-Error 'Failed to fetch latest version. Please check your network or try again later.'
}

$assetName = "${binName}-${latestTag}-${target}.zip"
$assetUrl = "https://github.com/$repo/releases/download/$latestTag/$assetName"
$shaUrl = "https://github.com/$repo/releases/download/$latestTag/${binName}-${latestTag}-${target}.sha256"

$tmpDir = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString())
New-Item -ItemType Directory $tmpDir | Out-Null
$baseLocation = Get-Location

try {
  Set-Location $tmpDir

  Write-Info "Downloading files for version $latestTag..."
  Invoke-WebRequest -Uri $assetUrl -OutFile $assetName
  Invoke-WebRequest -Uri $shaUrl -OutFile "${binName}-${latestTag}-${target}.sha256"

  Write-Info 'Verifying checksum...'
  $expected = (Get-Content "${binName}-${latestTag}-${target}.sha256" | Select-Object -First 1).Split(' ')[0].Trim()
  $actual = (Get-FileHash $assetName -Algorithm SHA256).Hash.ToLowerInvariant()
  if ($expected -ne $actual) {
    Write-Error 'Checksum verification failed. The file may be corrupt.'
  }

  Write-Info 'Installing binary...'
  Expand-Archive $assetName -DestinationPath . -Force

  New-Item -ItemType Directory -Path $binRoot -Force | Out-Null

  $tmpBin = Join-Path ([System.IO.Path]::GetDirectoryName([System.IO.Path]::GetTempFileName())) "${binExecutable}"
  Copy-Item -LiteralPath $binExecutable -Destination $tmpBin -Force

  Write-Info 'Testing installation...'
  try {
    & $tmpBin --version | Out-Null
  }
  catch {
    if (Test-Path -Path $tmpBin) {
      Remove-Item -Path $tmpBin -Force
    }
    Write-Error 'The installed binary failed to run. It may be incompatible with your system.'
  }

  Move-Item -LiteralPath $tmpBin -Destination $binPath -Force

  if ($latestTag.startsWith("v")) {
    $configVersion = $latestTag.Substring(1)
  }
  else {
    $configVersion = $latestTag
  }

  $versionBinPath = Join-Path $versionDir $configVersion
  New-Item -ItemType Directory -Path $versionBinPath -Force | Out-Null
  Move-Item -LiteralPath $binExecutable -Destination $versionBinPath -Force

  Write-Info "Setting default version to '$configVersion'..."
  $configDir = Split-Path -Path $configFile -Parent
  New-Item -ItemType Directory -Path $configDir -Force | Out-Null
  "default = `"$configVersion`"`n" | Set-Content -Path $configFile -Encoding UTF8

  Write-Info 'Configuring user environment...'

  $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')

  if (-not ($userPath.Split(';') -contains $binRoot)) {
    $newPath = if ($userPath) { "$binRoot;$userPath" } else { $binRoot }
    [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')

    if (-not ($env:Path.Split(';') -contains $binRoot)) {
      $env:Path = "$binRoot;$env:Path"
    }
  }

  Write-Host ''
  Write-Success 'Installation complete.'
}
finally {
  Set-Location -Path $baseLocation
  if (Test-Path -Path $tmpDir) {
    Write-Info 'Cleaning up temporary files...'
    Remove-Item -Path $tmpDir -Recurse -Force
  }
}
