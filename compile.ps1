[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)] [string] $Directory
)

$files_to_compile = Get-ChildItem -Path $Directory `
| Where-Object { $_.Extension -eq ".f95"}

$output_dir = Join-Path $Directory "output"
if(-not(Test-Path -Path $output_dir)){
    New-Item -ItemType Directory $output_dir
}

foreach ($file_to_compile in $files_to_compile) {
    $output_file = Join-Path $output_dir "$($file_to_compile.BaseName).o"
    f95 $file_to_compile -o $output_file
}