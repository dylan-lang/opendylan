Network - Posix Sockets
***********************

The existing network code is written with the BSD sockets API in mind. The
POSIX sockets API introduces some `improvements`_, particularly with handling
IPv4 vs IPv6 addresses.

We should move to support the POSIX APIs, remove the usages of the BSD APIs
such as ``inet_aton``, ``gethostbyname`` and friends, and switch to using
the POSIX equivalents.

.. _improvements: https://en.wikipedia.org/wiki/Berkeley_sockets#BSD_vs_POSIX
