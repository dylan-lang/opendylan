*************************
Maintaining opendylan.org
*************************

In this document I aspire to make it easy for someone besides myself (cgay) to manage
opendylan.org or move it to another host in my absense.

`opendylan.org <https://opendylan.org>`_ is a relatively simple site comprised of the
following parts:

1.  The main website, http://opendylan.org/, is a static site built using `Sphinx
    <https://sphinx-doc.org>`_.  Instructions for building the documentation manually are
    in the `opendylan <https://github.com/dylan-lang/opendylan>`_ repository in the file
    :file:`documentation/README.rst`.

2.  `package.opendylan.org <https://package.opendylan.org>`_ is static documentation
    automatically built with the `gendoc <https://github.com/dylan-lang/gendoc>`_
    repository.

3.  `play.opendylan.org <https://play.opendylan.org>`_ is a dynamic website for executing
    arbitrary Dylan code, implemented in the `dylan-playground
    <https://github.com/dylan-lang/dylan-playground>`_ repository.

All three parts are hosted on the same machine.

The machine also hosts https://yhoti.org which is just a static site using the same
nginx.  That is generally for cgay to worry about, but when changing opendylan.org one
should be aware that taking down nginx also takes down yhoti.org.

Overview
========

As of December 2025 the machine is run by `Hostinger <https://www.hostinger.com>`_, its
IPv4 address is 195.179.193.248, and it is physically located in the Netherlands.  The
admin UI is at https://hpanel.hostinger.com/vps and the machine is paid for through
2027-12-08.  cgay is currently the only one with admin UI access.  housel has full access
to the machine itself.

The host machine is currently running Debian 13 (Trixie) and the jobs are managed with
systemd.  Use ``systemctl list-units | grep dylan`` to see the relevant services and
timers.

Machine Setup
=============

The following packages were required when installing on the current machine::

   apt install autoconf binutils bzip2 certbot clang emacs furo git libgc-dev \
     libunwind-dev make nginx python3-sphinx python3-sphinx-copybutton rsync

Install certificates for nginx.  I used::

  certbot certonly -d 'opendylan.org,*.opendylan.org' --cert-name opendylan.org --manual

This required adding a TXT DNS record for _acme-challenge.opendylan.org, and storing a
file in :file:`/.well-known/acme-challenge/2H9B9VYUAg54iEwE1q7W_VAJAH7X9W5QBf7xTUEBX04`
for proof of ownership.  nginx had to be serving on port 80 for the challenges to
succeed. (Both challenge assets have since been deleted.)

The certs are in :file:`/etc/letsencrypt/archive/opendylan.org/`.  After installation
certbot said::

  NEXT STEPS:
    - This certificate will not be renewed automatically. Autorenewal of --manual
      certificates requires the use of an authentication hook script (--manual-auth-hook)
      but one was not provided. To renew this certificate, repeat this same certbot
      command before the certificate's expiry date.

I created a link ``libunwind.so.1 -> libunwind.so.8.0.1`` to allow Open Dylan to be able
to link binaries correctly. (Housel says this would be fixed by using non-GNU libunwind.)

Backups
=======

The only assets on the machine that can't be regenerated from Git source files are some
systemd config files (most of which are included below) and the play.opendylan.org
"shares" directory.  For a full up-to-date list of such files see
:file:`/home/dylan/backup-opendylan.org.sh` on the machine or in one of the backups.

:file:`/etc/systemd/system/opendylan-backup.service` and
:file:`/etc/systemd/system/opendylan-backup.timer` define the backup schedule.  They
simply run :file:`/home/dylan/backup-opendylan.org.sh` which writes a full backup
of the important files and directories to
:file:`/var/backups/opendylan.org/opendylan.org-backup-DATE.tar.bz2`.

cgay also has a job to rsync the backups to his machine periodically.

Open Dylan
==========

The playground and gendoc need Open Dylan, which is installed in a version-specific
directory in :file:`/opt` and then linked from :file:`/opt/opendylan`.  Scripts depend on
:file:`/opt/opendylan/bin` existing.

nginx
=====

``nginx`` serves the static sites in :file:`/var/www/opendylan.org` and
:file:`/var/www/package.opendylan.org`, and proxies play.opendylan.org to
http://localhost:8001.

See the config in :file:`/etc/nginx/sites-availabel/opendylan.org`.  Various other
subdomain names and URLs redirect to opendylan.org.

(It is now possible to get the Dylan HTTP server running with SSL, so it would be very
nice to stop using nginx completely.)

play.opendylan.org
==================

The playground is a Dylan web app that runs as the "dylan" user.  Because the Dylan HTTP
server didn't work with SSL at the time the playground was written, ``nginx`` is used to
proxy to plain HTTP on port 8001.

The live playground instance is deployed to :file:`/opt/dylan-playground/live` by logging
in as "dylan" and::

  cd ~/dylan/workspaces/dylan-playground
  environment=live make install

In general if you redeploy after making source code changes or after upgrading the
installed version of Open Dylan you'll need to test something in the playground to get it
past that first very-long build time.  Just ask it to run Hello World.

The systemd service to start the playground after reboot, or restart it after a crash is
"dylan-playground".  The current systemd service file
(:file:`/etc/systemd/system/dylan-playground.service`) looks like this::

  [Unit]
  Description=Dylan Playground
  After=network.target
  StartLimitIntervalSec=0

  [Service]
  Type=simple
  Restart=always
  RestartSec=2
  User=dylan
  ExecStart=/opt/dylan-playground/live/bin/dylan-playground --config /opt/dylan-playground/live/config.xml --working-directory /opt/dylan-playground/live

  [Install]
  WantedBy=multi-user.target

Once that's in place, use these commands::

  systemctl enable dylan-playground
  systemctl start  dylan-playground
  systemctl status dylan-playground

opendylan.org
=============

There's a systemd timer to get the latest docs and rebuild the main website periodically,
defined by the following files.

:file:`/etc/systemd/system/opendylan.org.service`::

  [Unit]
  Description="Generate main Dylan documentation website"

  [Service]
  User=dylan
  ExecStart=/home/dylan/dylan/workspaces/opendylan/documentation/update-opendylan.org.sh

:file:`/etc/systemd/system/opendylan.org.timer`::

  [Unit]
  Description="Run opendylan.org.service"

  [Timer]
  OnCalendar=Mon-Sun *-*-* 03,09,15,21:30:02
  Unit=opendylan.org.service

  [Install]
  WantedBy=multi-user.target

See :file:`opendylan/documentation/README.rst` and the script in the service definition
above for more details.

Useful commands::

   systemctl status opendylan.org.service
   systemctl status opendylan.org.timer

package.opendylan.org (gendoc)
==============================

Another systemd timer.  You get the idea.

:file:`/etc/systemd/system/opendylan-gendoc.service`::

  [Unit]
  Description="Generate Dylan package documentation"

  [Service]
  User=dylan
  ExecStart=/home/dylan/dylan/workspaces/gendoc/update-package.opendylan.org.sh

:file:`/etc/systemd/system/package.opendylan.org.timer`::

  [Unit]
  Description="Run opendylan-gendoc.service"

  [Timer]
  OnCalendar=Mon-Sun *-*-* 10,21:10:02
  Unit=opendylan-gendoc.service

  [Install]
  WantedBy=multi-user.target

See :file:`gendoc/README.rst` for more details on this.
