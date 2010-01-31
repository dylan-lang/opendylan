/*
 synopsis: ssl support for sockets
 author: Hannes Mehnert, hannes@mehnert.org
 copyright: BSD, GNU LGPL
*/
#include <openssl/ssl.h>
#include <openssl/pem.h>
#include <openssl/x509.h>

long my_SSL_set_mode (SSL* s, long op) {
  return SSL_set_mode(s, op);
}

X509* my_PEM_read_X509 (char* filename, X509** x, pem_password_cb* cb, void* u) {
  FILE* f;
  X509* xx;
  f = fopen(filename, "r");
  xx = PEM_read_X509(f, x, cb, u);
  fclose(f);
  return xx;
}

long my_SSL_CTX_add_extra_chain_cert (SSL_CTX* ctx, X509* x509) {
  return SSL_CTX_add_extra_chain_cert(ctx, x509);
}
