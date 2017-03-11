#!/bin/bash -e

bin=`pwd`/bin/auto-push

killall auto-push || true
rm -Rf test.git test
mkdir -p test.git
GIT_DIR=test.git git init --bare

# Create initial commit
mkdir -p test
pushd test
git init
echo hello | tee hello
git add hello
git commit -m "initial commit"
git remote add origin ../test.git
git push -u origin master
popd

# Setup hooks
cat >test.git/hooks/pre-receive <<EOF
#!/bin/bash -e
$bin pre-receive
EOF
cat >test.git/hooks/post-receive <<EOF
#!/bin/bash -e
$bin post-receive
EOF
chmod ugo+rx test.git/hooks/{pre-receive,post-receive}

# Start server
( cd test.git; $bin server +RTS -N4 2>&1 | awk '$0="[server] " $0' )&
sleep 0.1

pushd test
git checkout -b merge/master

commit_n=0
do_commit() {
    echo "commit $commit_n" | tee $1
    git add $1
    git commit -m "commit $commit_n"
    let commit_n=commit_n+1
}

# Open merge request
echo
do_commit file1
git push origin merge/master
sleep 1

echo
git checkout -b branch1
do_commit file1
sleep 1

echo
git checkout -b branch2 master
do_commit file2
git push origin branch1:merge/master
git push origin branch2:merge/master
sleep 1

echo
git remote update
echo "origin/master"
git show origin/master
echo "origin/merge/master"
git show origin/merge/master

gitk origin/master origin/merge/master $(git branch -r | grep auto-push)
popd

kill %1
