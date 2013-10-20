gallifrey.home=@@GALLIFREYHOME@@

ivy.root=${gallifrey.home}/.ivyroot
ivy.lcache.dir=${ivy.root}/cache
ivy.lrepo.dir=${ivy.root}/repos

build.dir=${basedir}/build.output.folder
report.dir=${build.dir}/reports
pod.dir=${basedir}/POD-INF

build.version=0.0.1-SNAPSHOT
build.debug=true
build.type=@@APPTYPE@@

ivy.lib.dir=${basedir}/lib
lib.dir=${pod.dir}/lib

test.dir=${basedir}/src/test
src.dir=${basedir}/src/main

out.test.dir=${pod.dir}/test-classes
out.jar.dir=${pod.dir}/classes

#jslang=typescript
jslang=coffee









