cd ..

git rev-parse --short HEAD > tmp.txt
set /p rev_hash=<tmp.txt

git rev-list --tags --max-count=1 > tmp.txt
set /p hash_tmp=<tmp.txt

git describe --tags %hash_tmp% > tmp.txt
set /p last_tag=<tmp.txt

git rev-list %last_tag%..HEAD --count > tmp.txt
set /p rev_count_since_tag=<tmp.txt

echo %last_tag%.dev%rev_count_since_tag%.%rev_hash% > git_version.txt

echo text35='%last_tag%.dev%rev_count_since_tag%.%rev_hash%' > ./dtu_we_controller/git_version.inc

del tmp.txt
