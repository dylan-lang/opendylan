module: cgi-pizza
synopsis: A sample cgi program that uses the cgi-lib
author: Peter Hinely <phinely@hawaii.edu>
copyright: 

define constant $http-header = "Content-type: text/html\r\n\r\n"; 
define constant $html-header = 
  "<html><head><title>Scrumpy's Pizza</title></head><body BGCOLOR=#EEEEFF>"; 
define constant $html-footer = "</body></html>"; 

define sideways method cgi-main ()
  format-out($http-header);
  format-out($html-header);
  dispatch-cgi-function(carry-out, eat-in, print-environment-variables); 
  format-out($html-footer);
end method;

define function carry-out (#key name, toppings-list, spices-list, requests, #all-keys)
  format-out("<H3>Thank you for ordering from Scrumpy's.</H3>");
  if(name)
    format-out("Please use the name \"%s\" to claim your pizza.", name);
  end if;
  format-out("Our patented Hotstuff box is specially designed to keep your pizza piping hot.<BR>");
  format-out("Your pizza has %d toppings: %s<BR>", toppings-list.size, as-lowercase(list-to-string(toppings-list)));
  if(spices-list)
    format-out("Your pizza has %d spices: %s<BR>", spices-list.size, list-to-string(spices-list));
  end if;
  if(requests)
    format-out("You made the following special request: %s<BR>", requests);
  end if;
end;

define function eat-in (#key, #all-keys)
  format-out("<H3>Thank you for eating at Scrumpy's.</H3>");
  format-out("Come again soon!");
end;

define function print-environment-variables (#key, #all-keys)
  format-out("<pre><code>\n"); 
  format-out("Environment variables:\n"); 

  let var-list = list( 
                   pair("http-user-agent" , $http-user-agent), 
                   pair("remote-address" , $remote-address), 
                   pair("remote-host" , $remote-host), 
                   pair("http-accept" , $http-accept), 
                   pair("http-referer" , $http-referer), 
                   pair("content-length" , $content-length), 
                   pair("query-string" , $query-string), 
                   pair("server-name" , $server-name), 
                   pair("server-port" , $server-port), 
                   pair("server-protocol" , $server-protocol), 
                   pair("gateway-interface" , $gateway-interface)
                  );  
  for(var in var-list)
    let var-name = var.head;
    let var-value = var.tail;
    if(var-value)
      format-out("%25s: %s\n", var-name, var-value); 
    end if;
  end for;
  format-out("</code></pre>"); 
end;

main();

