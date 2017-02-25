#!/bin/bash -e

bin=`pwd`/bin/auto-push

rm -Rf test.git test
mkdir -p test.git
GIT_DIR=test.git git init --bare
cat >test.git/hooks/pre-receive <<EOF
#!/bin/bash -e
$bin pre-receive
EOF
cat >test.git/hooks/post-receive <<EOF
#!/bin/bash -e
$bin post-receive
EOF
chmod ugo+rx test.git/hooks/{pre-receive,post-receive}

( cd test.git; $bin server )&

mkdir -p test
pushd test
git init

echo hello > hello
git add hello
git commit -m "initial commit"

echo hello world >| hello
git commit -a -m "another commit"
git remote add origin ../test.git
git push -u origin master

echo gogogo >| hello
git commit -a -m "yet another commit"
git push -u origin master

git remote update
git show origin/master
popd

kill %1
