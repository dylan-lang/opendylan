module: unix-sockets

define c-struct <pollfd>
  slot pollfd-fd :: <C-int>;
  slot pollfd-events :: <C-short>;
  slot pollfd-revents :: <C-short>;
  pointer-type-name: <pollfd*>;
end c-struct;

define inline-only C-function poll
  parameter ufds :: <pollfd*>;
  parameter nfds :: <C-int>;
  parameter timeout :: <C-int>;
  result val :: <C-int>;
  c-name: "poll";
end C-function;

define constant $POLLIN    = #x0001;    /* There is data to read */
define constant $POLLPRI   = #x0002;    /* There is urgent data to read */
define constant $POLLOUT   = #x0004;    /* Writing now will not block */
define constant $POLLERR   = #x0008;    /* Error condition */
define constant $POLLHUP   = #x0010;    /* Hung up */
define constant $POLLNVAL  = #x0020;    /* Invalid request: fd not open */
