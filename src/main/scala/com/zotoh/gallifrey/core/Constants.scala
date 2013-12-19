/*??
*
* Copyright (c) 2013 Cherimoia, LLC. All rights reserved.
*
* This library is distributed in the hope that it will be useful
* but without any warranty; without even the implied warranty of
* merchantability or fitness for a particular purpose.
*
* The use and distribution terms for this software are covered by the
* Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
* which can be found in the file epl-v10.html at the root of this distribution.
*
* By using this software in any fashion, you are agreeing to be bound by
* the terms of this license.
* You must not remove this notice, or any other, from this software.
*
 ??*/


package com.zotoh.gallifrey.core

object Constants {

  val SYS_DEVID_PFX= "system.####"
  val SYS_DEVID_SFX= "####"

  val SYS_DEVID_REGEX= SYS_DEVID_PFX+"[0-9A-Za-z_\\-\\.]+"+SYS_DEVID_SFX
  val SHUTDOWN_DEVID= SYS_DEVID_PFX+"kill_9"+SYS_DEVID_SFX
  val SHUTDOWN_URI="/kill9"

  val POD_PROTOCOL = "pod:"
  val META_INF = "META-INF"
  val POD_INF = "POD-INF"
  val WEB_INF = "WEB-INF"
//  val PATCH="patch"
//  val LIB="lib"
//  val CFG="conf"
//  val CLASSES="classes"
  val DN_BLOCKS = "blocks"
  val DN_CORE="exec"
  val DN_CONF="conf"
  val DN_CLASSES="classes"
  val DN_LIB="lib"
  val DN_CFG="etc"
  val DN_BOXX="apps"    
  val DN_PODS = "pods"
  val DN_LOGS="logs"
  val DN_TMP="tmp"
  val DN_DBS="dbs"
  val DN_DIST="dist"
  val DN_TEMPLATES = "templates"
  val DN_VIEWS = "views"
  val DN_PAGES = "pages"
  val DN_PATCH="patch"
  val DN_MEDIA="media"
  val DN_SCRIPTS="scripts"
  val DN_STYLES="styles"
  val DN_PUBLIC="public"


  val MN_FILE= META_INF + "/" + "MANIFEST.MF"
  val POD_CLASSES = POD_INF + "/"+ DN_CLASSES
  val POD_PATCH = POD_INF + "/"+ DN_PATCH
  val POD_LIB = POD_INF + "/"+ DN_LIB

  val WEB_CLASSES = WEB_INF + "/"+ DN_CLASSES
  val WEB_LIB = WEB_INF + "/"+ DN_LIB
  val WEB_LOG = WEB_INF + "/logs"
  val WEB_XML = WEB_INF + "/web.xml"

  val MN_RNOTES= META_INF + "/" + "RELEASE-NOTES.txt"
  val MN_README= META_INF + "/" + "README.md"
  val MN_NOTES= META_INF + "/" + "NOTES.txt"
  val MN_LIC= META_INF + "/" + "LICENSE.txt"

  val CFG_ENV_CF = DN_CONF + "/" + "env.conf"
  val CFG_APP_CF = DN_CONF + "/" + "app.conf"


  val PF_GALLIFREY_APPDOMAIN="gallifrey.app.domain"
  val PF_GALLIFREY_APPID="gallifrey.appid"
  val PF_GALLIFREY_APPTASK="gallifrey.app.task"

  val PF_JMXMGM="jmx-management"
  val PF_HOMEDIR="gallifrey.home"
  val PF_PROPS="gallifrey.conf"
  val PF_ROUTE_INFO="route.info"
  val PF_CLISH="cli-shell"
  val PF_COMPS="components"
  val PF_REGS="registries"
  val PF_KERNEL="kernel"
  val PF_EXECV="execvisor"
  val PF_DEPLOYER="deployer"
  val PF_BLOCKS="blocks"
  val PF_APPS="apps"
  val PF_SVCS="services"
  val PF_LOCALE="locale"
  val PF_L10N="l10n"
  val PF_PIDFILE="pidfile"

  val K_ROOT_CZLR="root.loader"
  val K_EXEC_CZLR="exec.loader"

  val K_BASEDIR="base.dir"
  val K_PODSDIR="pods.dir"
  val K_CFGDIR="cfg.dir"
  val K_APPDIR="app.dir"
  val K_PLAYDIR="play.dir"
  val K_LOGDIR="log.dir"
  val K_TMPDIR="tmp.dir"
  val K_DBSDIR="dbs.dir"
  val K_BKSDIR="blocks.dir"

  val K_COUNTRY="country"
  val K_LOCALE="locale"
  val K_LANG="lang"

  val K_META="meta"

}
