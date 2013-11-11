(defproject comzotohcljc/gallifrey "0.0.1-SNAPSHOT"

  :description "A full stack server-side application framework."

  :url "http://zotoh.com/gallifrey"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}


  :plugins [
            [lein-scalac "0.1.0"]
      ]

  :mirrors {

    "ibiblio" {
      :url "http://mirrors.ibiblio.org/pub/mirrors/maven2"
    }

    "clojars" {
      :url "http://clojars.org/repo"
    }

    "sonatype-oss-public" {
      :url "https://oss.sonatype.org/content/groups/public"
    }

    "central" {
      :url "http://central.maven.org/maven2"
    }

    "maven-1" {
      :url "http://repo1.maven.org/maven2"
    }

    "mandubian-mvn" {
      :url "http://mandubian-mvn.googlecode.com/svn/trunk/mandubian-mvn/repository"
    }

  }

  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :jvm-opts ["-Xmx1g"]
  :target-path "target/"

  :prep-tasks [ "javac" "scalac" "compile"]

  :global-vars { *warn-on-reflection* true
                 *assert* true }

  :dependencies [

    [bouncycastle/bcprov-jdk15on "149"]
    [bouncycastle/bcmail-jdk15on "149"]
    [bouncycastle/bcpkix-jdk15on "149"]
    [org.jasypt/jasypt "1.9.0"]
    [org.mindrot/jbcrypt "0.3m"]

    [org.slf4j/slf4j-api "1.7.5"]
    [log4j/log4j "1.2.17"]

    [ch.qos.logback/logback-classic "1.0.13"]
    [ch.qos.logback/logback-core "1.0.13"]

    [net.sourceforge.jregex/jregex "1.2_01"]
    [net.sf.jopt-simple/jopt-simple "4.5"]
    [com.google.guava/guava "14.0.1"]
    [com.google.code.findbugs/jsr305 "2.0.1"]
    [joda-time/joda-time "2.2"]
    [org.zeroturnaround/zt-zip "1.6"]
    [org.apache.axis/axis "1.4"]
    [org.apache.axis/axis-jaxrpc "1.4"]

    [org.jdom/jdom2 "2.0.4"]

    [org.apache.commons/commons-lang3 "3.1"]
    [commons-net/commons-net "3.3"]
    [commons-io/commons-io "2.4"]

    [commons-logging/commons-logging "1.1.1"]
    [commons-email/commons-email "1.3.1"]
    [commons-codec/commons-codec "1.8"]
    [commons-fileupload/commons-fileupload "1.3"]
    [commons-dbutils/commons-dbutils "1.5"]
    [com.sun.mail/javamail "1.5.0"]

    [org.apache.ivy/ivy "2.3.0"]
    [org.apache.ant/ant "1.9.2"]
    [org.apache.ant/ant-launcher "1.9.2"]
    [org.apache.ant/ant-junit4 "1.9.2"]
    [org.apache.ant/ant-junit "1.9.2"]
    [org.apache.ant/ant-apache-log4j "1.9.2" :exclusions [ log4j/log4j ]]
    [ant-contrib/ant-contrib "1.0b3" :exclusions [ org.apache.ant/ant ]]
    [org.codehaus.gant/gant_groovy2.1 "1.9.9"]

    [com.jolbox/bonecp "0.7.1.RELEASE"]

    [org.apache.httpcomponents/httpcore-nio "4.2.4"]
    [org.apache.httpcomponents/httpcore "4.2.4"]
    [org.apache.httpcomponents/httpclient "4.2.5"]
    [io.netty/netty "3.7.0.Final"]
    [org.eclipse.jetty.orbit/javax.servlet "3.0.0.v201112011016"]

    [org.eclipse.jetty/jetty-xml "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-server "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-continuation "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-servlet "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-server "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-util "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-security "9.0.4.v20130625"]
    [org.eclipse.jetty/jetty-webapp "9.0.4.v20130625"]
    [org.eclipse.jetty.websocket/websocket-api "9.0.4.v20130625"]
    [org.eclipse.jetty.websocket/websocket-common "9.0.4.v20130625"]
    [org.eclipse.jetty.websocket/websocket-servlet "9.0.4.v20130625"]
    [org.eclipse.jetty.websocket/websocket-client "9.0.4.v20130625"]
    [org.eclipse.jetty.websocket/websocket-server "9.0.4.v20130625"]

    [org.codehaus.groovy/groovy-all "2.1.7"]
    [org.scala-lang/scala-library "2.10.2"]
    [org.scala-lang/scala-compiler "2.10.2"]
    [com.sun.tools/tools "1.7.0"]

    [com.github.spullara.mustache.java/compiler "0.8.9"]
    [org.fusesource.scalate/scalate-core_2.10 "1.6.1"]
    [org.freemarker/freemarker "2.3.20"]

    [com.yahoo.platform.yui/yuicompressor "2.4.7"]
    [javax/geronimo-jms_1.1_spec "1.1.1"]
    [com.h2database/h2 "1.3.172"]
    [org.postgresql/postgresql "9.2-1002.jdbc4"]
    [com.mysql/mysql-connector-java "5.1.23-bin"]
    [net.sf.ehcache/ehcache "2.7.0"]

    [org.clojure/math.numeric-tower "0.0.2"]
    [org.clojure/math.combinatorics "0.0.4"]
    [org.clojure/tools.logging "0.2.6"]
    [org.clojure/data.codec "0.1.0"]
    [org.clojure/java.jdbc "0.3.0-alpha4"]
    [org.clojure/java.jmx "0.2.0"]
    [org.clojure/data.json "0.2.2"]
    [org.clojure/data.xml "0.0.7"]
    [org.clojure/clojure "1.5.1"]

    [org.apache.shiro/shiro-core "1.2.2"]

    [org.scalatest/scalatest_2.10 "1.9.1"]
    [net.mikera/cljunit "0.3.0"]
    [junit/junit "4.11"]

                 ]

  :scala-source-path [ "src/main/scala" ]
  :java-source-paths ["src/main/java"]
  :source-paths [ "src/main/clojure"]
  :test-paths [ "src/test/clojure"]

  :main comzotohcljc.tardis.etc.core


)
