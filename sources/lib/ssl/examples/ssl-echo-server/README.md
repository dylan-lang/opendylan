`certificate.pem` and `key.pem` were generated like this:

`openssl req -x509 -sha256 -nodes -days 5000 -newkey rsa:2048 -keyout key.pem -out certificate.pem`

Use `openssl x509 -in certificate.pem -text -noout` to view the certificate.
