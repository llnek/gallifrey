# About

A framework designed for building event driven applications. The goal is to make application development fast, effective and without unnecessary complexities.  Use it to build a ESB (Enterprise Service Bus) , SOA (Service oriented Arch.) service end points or a Web Application.

# Supported Platforms
* Scala 2.10
* Java VM &gt;= 1.6
* Linux &amp; Windows
* works with Groovy &amp; Java out of the box.

# Examples
Lots of examples showcasing how to use the framework, from simple to complex.  All samples
are available in Scala.

# Features
* Code! and then compile and run via simple command-line menu.
* Package and deploy to remote host via SSH.
* Together with Camungo (An EC2 console), can easily deploy and manage your app on EC2.
* Event driven, asynchronous, workflow driven coding style.
* Code your business logic in units and the framework will handle events and schedule your work units.
* Uses proven open-source libraries such as Netty-NIO, Jetty/Web.
* Plugin architecture allows user defined Event Devices.
* Access and Manage your EC2 instances right from command-line menu.

# Supported Event Devices (Event-Sources)
* HTTP Server
* POP3 Email Receiver
* File Monitoring
* JMS Message Listener
* TCP Socket
* Servlet
* RESTful
* WebSocket
* Timer(s)
* RSS/Atom Reader

# Making a Workflow
<pre>
import com.zotoh.blason.kernel.Job
import com.zotoh.blasonwflow._
class MyFlow(job:Job) extends Pipeline(j) {

    // hand to the framework the 1st(initial) step.
    override def onStart() = {
        new PTask withWork( new Work {
            public void eval(Job j, Object arg) {
                println("hello, I am step 1")
            }
        }).chain new PTask(new Work {  // step2
             def eval(j:Job, arg:Any*) {
                println("hello, I am step 2")
            }
        })
    }
}
</pre>

# Command line menu (terminal friendly)
<pre>
create web <app-name>              ' e.g. create helloworld as a webapp.
create <app-name>                  ' e.g. create helloworld
bundle <app-name>                  ' e.g. bundle helloworld
ide eclipse <app-name>             ' Generate eclipse project files.
compile <app-name>                 ' Compile sources.
test <app-name>                    ' Run test cases.
debug                              ' Start & debug the application.
start [bg]                         ' Start the application.
generate serverkey                 ' Create self-signed server key (pkcs12).
generate password                  ' Generate a random password.
generate csr                       ' Create a Certificate Signing Request.
encrypt <password> <some-text>     ' e.g. encrypt SomeSecretData
testjce                            ' Check JCE  Policy Files.
demo samples                       ' Generate a set of samples.
version                            ' Show version info.

help - show standard commands
</pre>

# Lots more to read
Goto [http://blason.zotoh.com/](http://blason.zotoh.com)

# Contact
For questions and bug reports, please email [contactzotoh@gmail.com](mailto:contactzotoh@gmail.com)

# Latest binary
Download the latest bundle [1.0.0](http://blason.zotoh.com/packages/stable/1.0.0/blason-1.0.0.tar.gz)


