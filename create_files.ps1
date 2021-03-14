[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)] [string] $Directory,
    [Parameter(Mandatory = $true)] [int] $StartIndex,
    [Parameter(Mandatory = $true)][int] $EndIndex
)

for ($i = $StartIndex; $i -le $EndIndex; $i++) {
    $file_path = Join-Path -Path (Resolve-Path $Directory) "prog_$i.f95"
    New-Item -Path $file_path -ItemType File
}