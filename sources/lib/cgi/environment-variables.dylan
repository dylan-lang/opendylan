module:	        environment-variables
author:         Peter Hinely <phinely@hawaii.edu>
copyright:      (C) 1996-1998 Peter Hinely
version:        0.11, 12/10/96
synopsis:       HTTP cgi utilities

define constant $remote-host = getenv("REMOTE_HOST"); 
define constant $remote-address = getenv("REMOTE_ADDR"); 
define constant $server-name = getenv("SERVER_NAME"); 
define constant $server-port = getenv("SERVER_PORT"); 
define constant $server-protocol = getenv("SERVER_PROTOCOL"); 
define constant $gateway-interface = getenv("GATEWAY_INTERFACE"); 
define constant $query-string = getenv("QUERY_STRING"); 
define constant $content-type = getenv("CONTENT_TYPE"); 
define constant $content-length = getenv("CONTENT_LENGTH"); 
define constant $http-referer = getenv("HTTP_REFERER"); 
define constant $http-user-agent = getenv("HTTP_USER_AGENT"); 
define constant $http-accept = getenv("HTTP_ACCEPT"); 
