gantBuildDir=/wdrive/dev/builds/${gantProjectName}
scalaLibDir=/wdrive/opt/lang/scala/lib
clojureDir=/wdrive/opt/lang/clojure/lib
ivyLibDir=${gantBuildDir}/lib

ivyRoot=/wdrive/.ivy
ivyLCacheDir=${ivyRoot}/cache
ivyLRepoDir=${ivyRoot}/repos

buildVersion=0.0.1-SNAPSHOT
buildDebug=true

distribDir=${gantBuildDir}/distrib
libDir=${gantBuildDir}/lib
buildDir=${gantBuildDir}/build
qaDir=${gantBuildDir}/test
packDir=${gantBuildDir}/package

testDir=${basedir}/src/test
srcDir=${basedir}/src/main

reportTestDir=${qaDir}/test-reports
buildTestDir=${qaDir}/test-classes





