[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)] [string] $Directory,
    [Parameter(Mandatory = $true)] [int] $StartIndex,
    [Parameter(Mandatory = $true)][int] $EndIndex
)

$template_name = "prog.template"

for ($i = $StartIndex; $i -le $EndIndex; $i++) {
    $file_path = Join-Path -Path (Resolve-Path $Directory) "prog_$i.f95"
    (Get-Content "$PSScriptRoot\$template_name" ) | ForEach-Object {
        $_ -replace "{program_name}", "p$i"
    } | `
    Set-Content -Path $file_path
}