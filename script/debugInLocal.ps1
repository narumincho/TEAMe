./node_modules/.bin/parcel.ps1 ./client/source/index.html --out-dir debugDistribution --open;

Copy-Item ./client/source/assets -Recurse -Destination ./debugDistribution;

