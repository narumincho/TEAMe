Write-Output "Compile Client Code And Upload Firebase Server";

./node_modules/.bin/parcel.ps1 build ./source/call/call.ts --out-dir distribution --out-file main.js;

Copy-Item ./source/assets/ ./distribution/ -Recurse -Force

firebase.ps1 deploy --only hosting --project teame-c1a32;
