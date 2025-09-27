$ErrorActionPreference = 'Stop'

# --- Platform/Arch Detection ---
$os = $env:OS
$arch = $env:PROCESSOR_ARCHITECTURE

switch ($os) {
  'Windows_NT' {
    switch ($arch) {
      'AMD64' { $target = 'x86_64-pc-windows-msvc' }
      # FIXME: Workout true ARM64 support target
      # For now, we use x86_64 for ARM64 Windows 
      'ARM64' { $target = 'x86_64-pc-windows-msvc' }
      default { Write-Error 'Dis machine architecture no get support. We expect x86_64 or aarch64.'; exit 1 }
    }
  }
  default { Write-Error 'Dis operating system no get support. Use Windows.'; exit 1 }
}

$bin = "naijaup"
$repo = "xosnrdev/naijaup"

# --- Fetch Latest Version ---
try {
  $latest = Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest"
  $tag = $latest.tag_name
}
catch {
  Write-Error "I no fit fetch latest version. Check your network and try again."; exit 1
}

$assetUrl = "https://github.com/$repo/releases/download/$tag/${bin}-${tag}-$target.zip"
$shaUrl = "https://github.com/$repo/releases/download/$tag/${bin}-${tag}-$target.sha256"

# --- Create a unique temp directory ---
$tmp = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString())
New-Item -ItemType Directory -Path $tmp | Out-Null
Set-Location $tmp

# --- Download Binary and Checksum ---
Write-Host "Downloading binary and checksum..."

Invoke-WebRequest -Uri $assetUrl -OutFile "${bin}-${tag}-$target.zip"
Invoke-WebRequest -Uri $shaUrl -OutFile "${bin}-${tag}-$target.sha256"


# --- Verify Checksum of Archive ---
Write-Host "Verifying checksum..."
$expected = (Get-Content "${bin}-${tag}-$target.sha256").Split(' ')[0]
$actual = (Get-FileHash "${bin}-${tag}-$target.zip" -Algorithm SHA256).Hash.ToLower()
if ($expected -ne $actual) {
  Write-Error "Checksum no match. Aborting installation."; exit 1
}

# --- Extract Archive ---
Write-Host "Extracting archive..."
Expand-Archive "${bin}-${tag}-$target.zip" -DestinationPath .

# --- Install ---
Write-Host "Installing binary..."
$installDir = "$env:USERPROFILE\.naijascript\bin"
if (!(Test-Path $installDir)) { New-Item -ItemType Directory -Path $installDir | Out-Null }
Move-Item naijaup.exe "$installDir\naijaup.exe" -Force

# --- Self-Test ---
Write-Host "Testing installation..."
try {
  & "$installDir\naijaup.exe" --version | Out-Null
  Write-Host "Installation successful."
}
catch {
  Write-Error "Self-check fail. Confirm PATH setup or run the installer again."; exit 1
}

# --- Ensure $installDir is in User PATH permanently ---
$path = [System.Environment]::GetEnvironmentVariable("Path", "User")
$paths = $path -split ';'
if ($paths -notcontains $installDir) {
  $newPath = if ($path -and $path.Trim() -ne "") { $path + ";" + $installDir } else { $installDir }
  [System.Environment]::SetEnvironmentVariable("Path", $newPath, "User")
  Write-Host "PATH no contain $installDir. I don add am to your user PATH."
  Write-Host "Restart your shell."
}
else {
  Write-Host "$installDir already dey your user PATH."
}

Write-Host "Installation don complete."
Write-Host "Run 'naijaup install latest' to pull the newest Interpreter."

# --- Cleanup ---
Write-Host "Cleaning up..."
Set-Location $env:USERPROFILE
Remove-Item -Recurse -Force $tmp
