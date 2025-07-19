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
      default { Write-Error 'Oga, your machine arch no dey supported. Abeg use x86_64 or aarch64.'; exit 1 }
    }
  }
  default { Write-Error 'Oga, your OS no dey supported. Abeg use Windows.'; exit 1 }
}
$bin = "naijaup"
$repo = "xosnrdev/naijascript"

# --- Fetch Latest Version ---
try {
  $latest = Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest"
  $tag = $latest.tag_name
}
catch {
  Write-Error "Wahala! I no fit find latest version for GitHub. Check your network."; exit 1
}

$assetUrl = "https://github.com/$repo/releases/download/$tag/${bin}-${tag}-$target.zip"
$shaUrl = "https://github.com/$repo/releases/download/$tag/${bin}-${tag}-$target.sha256"

# --- Create a unique temp directory ---
$tmp = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString())
New-Item -ItemType Directory -Path $tmp | Out-Null
Set-Location $tmp

# --- Download Binary and Checksum ---
Write-Host "I dey download $bin..."

Invoke-WebRequest -Uri $assetUrl -OutFile "${bin}-${tag}-$target.zip"
Invoke-WebRequest -Uri $shaUrl -OutFile "${bin}-${tag}-$target.sha256"


# --- Verify Checksum of Archive ---
Write-Host "I dey check say archive correct..."
$expected = (Get-Content "${bin}-${tag}-$target.sha256").Split(' ')[0]
$actual = (Get-FileHash "${bin}-${tag}-$target.zip" -Algorithm SHA256).Hash.ToLower()
if ($expected -ne $actual) {
  Write-Error "Omo! Checksum no match. No try run am o."; exit 1
}

# --- Extract Archive ---
Expand-Archive "${bin}-${tag}-$target.zip" -DestinationPath .

# --- Install ---
$installDir = "$env:USERPROFILE\.naijascript\bin"
if (!(Test-Path $installDir)) { New-Item -ItemType Directory -Path $installDir | Out-Null }
Move-Item naijaup.exe "$installDir\naijaup.exe" -Force

# --- Self-Test ---
Write-Host "I don put naijaup for $installDir. Make I check am..."
try {
  & "$installDir\naijaup.exe" --version | Out-Null
  Write-Host "Naijaup don land gidigba!"
}
catch {
  Write-Error "E get as e be. Naijaup no run. Check your PATH or try again."; exit 1
}

# --- Ensure $installDir is in User PATH permanently ---
$path = [System.Environment]::GetEnvironmentVariable("Path", "User")
$paths = $path -split ';'
if ($paths -notcontains $installDir) {
  $newPath = if ($path -and $path.Trim() -ne "") { $path + ";" + $installDir } else { $installDir }
  [System.Environment]::SetEnvironmentVariable("Path", $newPath, "User")
  Write-Host "Oga, I don add $installDir to your user PATH permanently. Restart your terminal to use 'naijaup' anywhere."
}
else {
  Write-Host "$installDir already dey your PATH."
}

Write-Host "If you wan install NaijaScript interpreter, run: naijaup install latest"
Write-Host "Oya, you fit enjoy NaijaScript now!"

# --- Cleanup ---
Set-Location $env:USERPROFILE
Remove-Item -Recurse -Force $tmp
