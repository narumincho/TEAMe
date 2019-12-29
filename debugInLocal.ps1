./node_modules/.bin/parcel.ps1 ./source/index.html --out-dir debugDistribution --open;

Copy-Item ./source/assets -Recurse -Destination ./debugDistribution;

