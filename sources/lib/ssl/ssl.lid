library: ssl-network
target-type: dll
Files:	 library
	 c-wrapper
	 openssl-wrapper
C-libraries: -lssl -lcrypto
C-source-files: support.c
Platforms: arm-linux
           x86-freebsd
           x86-linux
           x86-netbsd
           x86_64-darwin
           x86_64-freebsd
           x86_64-linux
           x86_64-netbsd
