Module:    network-test
Synopsis:  Automated test of the network library
Copyright: (c) 2018 Open Dylan
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define test ipv4-address-test ()
  let addr = make(<internet-address>, name: "www.google.com");
  assert-instance?(<ipv4-address>, addr);
  assert-equal("www.google.com", host-name(addr));
  let numaddr = make(<internet-address>, address: "192.168.0.1");
  assert-instance?(<ipv4-address>, numaddr);
  assert-equal("192.168.0.1", host-address(numaddr));
  assert-instance?(<numeric-address>, numeric-host-address(numaddr));
  assert-signals(<socket-condition>, make(<internet-address>, address: "1111.2222.3333.4444"));
  assert-signals(<socket-condition>, make(<internet-address>, address: "192.167.0.1.1"));
  assert-no-errors(make(<internet-address>, address: "255.255.255.255"));
  assert-instance?(<numeric-address>, numeric-host-address(numaddr));
  // RFC6761 this is guaranteed not to exist
  assert-signals(<host-not-found>, make(<internet-address>, name: "address.invalid"));
end test;

define test ipv4-numeric-address-test ()
  let raw = as(<machine-word>, #x7f000001);
  let num1 = make(<ipv4-numeric-address>, value: raw, order: #"host-order");
  let num2 = make(<ipv4-numeric-address>, value: raw, order: #"network-order");
  assert-instance?(<numeric-address>, num1);
  assert-instance?(<numeric-address>, num2);
  assert-equal(raw, host-order(num1));
  assert-equal(raw, network-order(num2));
  assert-equal("127.0.0.1", as(<string>, num1))
end test;

define test ipv6-address-test (expected-failure?: #t)
  let address = make(<internet-address>, address: "::1");
  assert-equal("::1", host-address(address));
end test;

define function tcp-server-one-shot ()
  with-socket-thread(server?: #t)
    with-server-socket (srv :: <server-socket>,
                        port: 8888,
                        protocol: "tcp")
      let conn :: <socket> = accept(srv);
      block (return)
        while (#t)
          let data :: <sequence> = read(conn, 16);
          if (first(data) == ' ')
            return();
          else
            write(conn, data);
          end if;
        end while;
      cleanup
        close(conn);
      exception (xep :: <incomplete-read-error>)
      end block;
    end;
  end;
end function;

define test tcp-test ()
  let server :: <thread> = make (<thread>, function: tcp-server-one-shot);
  sleep(0.5);
  let conn :: <socket> = make(<socket>, host: "localhost", port: 8888);
  block ()
    for (i from 1 to 10)
      let sent = make(<string>,
                      size: 16,
                      fill: as(<character>, 96+i));
      write(conn, sent);
      let received = read(conn, 16);
      assert-equal(sent, received);
    end for;
    write(conn, make(<string>, size: 16));
  cleanup
    close(conn);
    join-thread(server);
  end block;
end test;

define function udp-server-one-shot()
  with-socket-thread(server?: #t)
    with-server-socket (srv :: <server-socket>,
                        port: 8889,
                        protocol: "udp")
      let conn :: <socket> = accept(srv);
      block (return)
        while (#t)
          let data :: <sequence> = read(conn, 1);
          if (first(data) == ' ')
            return ();
          end if;
          write(conn, data);
        end while;
      cleanup
        close(conn);
      end block;
    end;
  end;
end function;

define test udp-test ()
  let server :: <thread> = make (<thread>, function: udp-server-one-shot);
  sleep(0.5);
  let conn :: <socket> = make(<socket>,
                              host: "localhost",
                              port: 8889,
                              protocol: #"udp");
  block ()
    for (i from 1 to 10)
      let sent = make(<string>,
                      size: 16,
                      fill: as(<character>, 64+i));
      write(conn, sent);
      let received = read(conn, 16);
      assert-equal(sent, received);
    end for;
    // Special value to cause server to quit
    write(conn, " ");
  cleanup
    close(conn);
    join-thread(server);
  end block;
end test;

define test server-socket-test ()
  let socket = #f;
  // This service because it has port >= 1024
  assert-no-errors(socket := make(<server-socket>,
                                  service: "socks",
                                  protocol: "tcp"));
  if (socket)
    assert-equal(1080, local-port(socket));
    assert-instance?(<internet-address>, local-host(socket));
    close(socket);
  end if;

  assert-no-errors(socket := make(<server-socket>,
                                  port: 8888,
                                  protocol: "tcp"));
  if (socket)
    assert-equal(8888, local-port(socket));
    assert-instance?(<internet-address>, local-host(socket));
    close(socket);
  end if;

end test;

define suite address-test-suite ()
  test ipv4-address-test;
  test ipv4-numeric-address-test;
  test ipv6-address-test;
end suite;

define suite sockets-test-suite ()
  test server-socket-test;
  test tcp-test;
  test udp-test;
end suite;

define suite network-test-suite (setup-function: start-sockets)
  suite address-test-suite;
  suite sockets-test-suite;
end suite;

define method main () => ()
  run-test-application(network-test-suite);
end method main;

begin
  main();
end;
