cd ..

git rev-parse --count HEAD > tmp.txt
set /p rev_count=<tmp.txt

git rev-parse --short HEAD > tmp.txt
set /p rev_hash=<tmp.txt

git rev-list --tags --max-count=1 > tmp.txt
set /p hash_tmp=<tmp.txt

git describe --tags %hash_tmp% > tmp.txt
set /p last_tag=<tmp.txt

git rev-list %last_tag%..HEAD --count > tmp.txt
set /p rev_count_since_tag=<tmp.txt

echo dev%rev_count_since_tag%.%rev_hash% > git_version.txt
del tmp.txt

VersionScript.exe
