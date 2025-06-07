$ErrorActionPreference = 'Stop'

# --- Platform/Arch Detection ---
$os = $env:OS
$arch = $env:PROCESSOR_ARCHITECTURE

switch ($os) {
  'Windows_NT' { $platform = 'windows' }
  default { Write-Error 'Oga, your OS no dey supported. Abeg use Windows.'; exit 1 }
}

switch ($arch) {
  'AMD64' { $arch = 'x86_64' }
  'ARM64' { $arch = 'aarch64' }
  default { Write-Error 'Oga, your machine arch no dey supported. Abeg use x86_64 or aarch64.'; exit 1 }
}

$bin = "naijaup-$platform-$arch.exe"
$repo = "xosnrdev/naijascript"

# --- Fetch Latest Version ---
try {
  $latest = Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest"
  $tag = $latest.tag_name
} catch {
  Write-Error "Wahala! I no fit find latest version for GitHub. Check your network."; exit 1
}

$assetUrl = "https://github.com/$repo/releases/download/$tag/$bin-$tag.zip"
$shaUrl = "https://github.com/$repo/releases/download/$tag/$bin-$tag.sha256"
$tmp = New-TemporaryFile | Split-Path
Set-Location $tmp

# --- Download Binary and Checksum ---
Write-Host "I dey download $bin..."
Invoke-WebRequest -Uri $assetUrl -OutFile "$bin-$tag.zip"
Invoke-WebRequest -Uri $shaUrl -OutFile "$bin-$tag.sha256"

# --- Extract Archive ---
Expand-Archive "$bin-$tag.zip" -DestinationPath .

# --- Verify Checksum of Extracted Binary ---
Write-Host "I dey check say file correct..."
$expected = (Get-Content "$bin-$tag.sha256").Split(' ')[0]
$actual = (Get-FileHash naijaup.exe -Algorithm SHA256).Hash.ToLower()
if ($expected -ne $actual) {
  Write-Error "Omo! Checksum no match. No try run am o."; exit 1
}

# --- Install ---
$installDir = "$env:USERPROFILE\.naijascript\bin"
if (!(Test-Path $installDir)) { New-Item -ItemType Directory -Path $installDir | Out-Null }
Move-Item naijaup.exe "$installDir\naijaup.exe" -Force

# --- Self-Test ---
Write-Host "I don put naijaup for $installDir. Make I check am..."
try {
  & "$installDir\naijaup.exe" --version | Out-Null
  Write-Host "Naijaup don land gidigba!"
} catch {
  Write-Error "E get as e be. Naijaup no run. Check your PATH or try again."; exit 1
}

# --- Ensure $installDir is in User PATH permanently ---
$path = [System.Environment]::GetEnvironmentVariable("Path", "User")
$paths = $path -split ';'
if ($paths -notcontains $installDir) {
  $newPath = if ($path -and $path.Trim() -ne "") { $path + ";" + $installDir } else { $installDir }
  [System.Environment]::SetEnvironmentVariable("Path", $newPath, "User")
  Write-Host "Oga, I don add $installDir to your user PATH permanently. Restart your terminal to use 'naijaup' anywhere."
} else {
  Write-Host "$installDir already dey your PATH."
}

Write-Host "If you wan install NaijaScript interpreter, run: naijaup install latest"
Write-Host "Oya, you fit enjoy NaijaScript now!"

# --- Cleanup ---
Set-Location $HOME
Remove-Item -Recurse -Force $tmp
