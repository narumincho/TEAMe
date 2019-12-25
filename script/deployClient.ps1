Write-Output "Compile Client Code And Upload Firebase Server";

New-Item ./client/distribution -ItemType Directory -Force

Write-Output "Compile Elm ...";

Set-Location ./client/source/main/;
elm make ./source/Main.elm --output ../../distribution/main.js --optimize;

Write-Output "Compile Elm OK";

