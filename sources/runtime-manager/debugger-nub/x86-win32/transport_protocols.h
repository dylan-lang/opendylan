/* ********************************************************************** */
/* ** transport_protocols.h                                            ** */
/* ** Descriptors for network transport (RPC) protocols                ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard                                              ** */
/* ** Copyright: (c) 1998 Functional Objects, Inc.                     ** */
/* **            All Rights Reserved.                                  ** */
/* ********************************************************************** */

#include <rpc.h>

#define MAX_ENDPOINT_NAME_SIZE 32
#define MAX_NETWORK_ADDRESS_SIZE 256

/* The number of support RPC transport protocols. At the moment, we
   support only two. */
#define DBG_TRANSPORT_NUM_PROTOCOLS 3

/* Indices for the supported protocols. */
#define DBG_TRANSPORT_NCACN_NB_NB 0
#define DBG_TRANSPORT_NCACN_IP_TCP 1
#define DBG_TRANSPORT_NCACN_NP 2

/* A default protocol */
#define DBG_TRANSPORT_DEFAULT 1

/* UUIDs for the remote connection server, and the remote debugger nub */
#define CONNECTION_SERVER_UUID "45f7bfc0-1b70-11d0-a8f4-00aa006b0423"
#define DEBUGGER_NUB_UUID "10bab960-1c4d-11d0-a8f4-00aa006b0423"

/* Server endpoint name for the ncacn_np protocol */
#define SERVER_PIPE_NAME "\\\\pipe\\\\nubserver"

/* Base nub endpoint name for the ncacn_np protocol */
#define NUB_PIPE_NAME "\\\\pipe\\\\rnub"

/* Name of the standalone remote debugger nub */
#define DEBUGGER_NUB_EXECUTABLE_NAME "rnub.exe"

#define NUB_MAX_CONCURRENT_REQUESTS 20
#define SERVER_MAX_CONCURRENT_REQUESTS 20

/* Error codes */
#define DBG_TRANSPORT_OK 1
#define DBG_TRANSPORT_ILLEGAL_BINDING 2
#define DBG_TRANSPORT_COULD_NOT_FIND_SERVER 3
#define DBG_TRANSPORT_SERVER_SPAWN_NUB_FAILURE 4
#define DBG_TRANSPORT_COULD_NOT_FIND_NUB 5
#define DBG_TRANSPORT_NUB_SPAWN_PROCESS_FAILURE 6

typedef int DBG_TRANSPORT_INDEX;
typedef int NUB_ID;

/* A descriptor for transport protocols. Correlates internal 
   encodings with user-readable descriptions of the protocol. */

typedef struct _DBG_TRANSPORT_PROTOCOL_DESCRIPTOR {
  char      *ProtSeqEncoding;
  char      *Description;
  char      *ServerEndpointName;
} DBG_TRANSPORT_PROTOCOL_DESCRIPTOR;

/* A global variable that holds the descriptors. */
extern DBG_TRANSPORT_PROTOCOL_DESCRIPTOR 
  dbg_transport_protocols[DBG_TRANSPORT_NUM_PROTOCOLS];

/* Descriptor for a connection to a server. */

typedef struct _SERVER_CONNECTION {
  RPC_IF_HANDLE        RpcHandle;
  DBG_TRANSPORT_INDEX  TransportProtocol;
  char                *StringBinding;
  char                 NetworkAddress[MAX_NETWORK_ADDRESS_SIZE];
  char                 EndpointName[MAX_ENDPOINT_NAME_SIZE];
} SERVER_CONNECTION,  *LPSERVER_CONNECTION;

/* Descriptor for a connection to a nub. */

typedef struct _NUB_CONNECTION {
  RPC_IF_HANDLE        RpcHandle;
  char                *StringBinding;
  char                 EndpointName[MAX_ENDPOINT_NAME_SIZE];
  LPSERVER_CONNECTION  ConnectionServer;
} NUB_CONNECTION, *LPNUB_CONNECTION;


/* Endpoint name generators, used internally */
void generate_debugger_nub_endpoint_name
    (DBG_TRANSPORT_INDEX protocol, NUB_ID nub_id, char *buffer);

int debugger_nub_string_to_integer(char *buf);
void debugger_nub_integer_to_string(char *prefix, int i, char *buf);
void construct_debugger_nub_command_line
  (char *protocol, char *endpoint, char *buffer);
/* Interfaces to report internal errors and diagnostics. */

void debugger_nub_rpc_error (char *function, char *reason);
void debugger_nub_rpc_diagnostic (char *function, char *reason);

/* Interfaces to the Transport Protocols */

void transport_get_enumerations
  (DBG_TRANSPORT_INDEX *first, DBG_TRANSPORT_INDEX *last);

int transport_get_description_length
  (DBG_TRANSPORT_INDEX index);

void transport_get_description
  (DBG_TRANSPORT_INDEX index, int buf_size, char *buf);
