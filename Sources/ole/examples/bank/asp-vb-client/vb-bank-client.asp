<% @language=vbscript %>
<html>
<head>
<title>Functional Developer OLE Bank</title>

<%
  dim bank, accounts, q, selectedAcct
  if IsEmpty(Session("accounts")) then
    set Session("bank") = Server.CreateObject("HQN.SimpleBankDemo.1")
    set Session("accounts") = Server.CreateObject("Scripting.Dictionary")
  end if
  set selectedAcct = Nothing
  set bank = Session("bank")
  set accounts = Session("accounts")
  q = chr(34) 'double quote
%>

<% SUB find_account
    dim reqname, acct
    reqname = Request("findName")
    if reqname = "" then
      Response.Write "Error: Name must be specified for Lookup"
      exit sub
    end if
    if accounts.Exists(reqname) then
      set selectedAcct = accounts(reqname)
      Response.Write "Account " & reqname & " already listed!"
      exit sub
    end if
    on error resume next
    set acct = bank.retrieveAccount(reqname)
    if IsEmpty(acct) then
      Response.Write "Error: There is no open account named " & reqname & " in the bank."
      exit sub
    end if
    accounts.Add reqname, acct
    set selectedAcct = acct
END SUB %>

<% SUB open_account
    dim reqname, acct
    reqname = Request("openName")
    if reqname = "" then
      Response.Write "Error: Name must be specified for Open"
      exit sub
    end if
    if accounts.Exists(reqname) then
      set selectedAcct = accounts(reqname)
      Response.Write "Error: Account " & reqname & " already exists"
      exit sub
    end if
    on error resume next
    set acct = bank.retrieveAccount(openName)
    if Not IsEmpty(acct) then
      accounts.Add reqname, acct
      set selectedAcct = acct
      Response.Write "Error: Account " & reqname & "already exists"
      exit sub
    end if
    set acct = bank.openAccount(reqname)
    if IsEmpty(acct) then
      Response.Write "Error: Open Account " & reqname & " failed"
      exit sub
    end if
    accounts.Add reqname, acct
    set selectedAcct = acct
END SUB %>

<% SUB debit_account
     dim reqname, acct, amt
     reqname = Request("Account")
     set acct = accounts(reqname)
     set selectedAcct = acct
     amt = Request("Amount")
     if amt = "" then
       Response.Write "Error: Amount to debit must be specified"
       exit sub
     end if
     if Not IsNumeric(amt) OR amt < 0 then
       Response.Write "Error: Invalid amount: " & amt
       exit sub
     end if
     on error resume next
     acct.debit(cInt(amt))
     if err.Number <> 0 then
       Response.Write "Error #" & err.Number & " in " & err.Source
       Response.Write "Description: " & err.Description
       err.Clear
     end if
END SUB %>

<% SUB credit_account
     dim reqname, acct, amt
     reqname = Request("Account")
     set acct = accounts(reqname)
     set selectedAcct = acct
     amt = Request("Amount")
     if amt = "" then
       Response.Write "Error: Amount to credit must be specified"
       exit sub
     end if
     if Not IsNumeric(amt) OR amt < 0 then
       Response.Write "Error: Invalid amount: " & amt
       exit sub
     end if
     on error resume next
     acct.credit(cInt(amt))
     if err.Number <> 0 then
       Response.Write "Error #" & err.Number & " in " & err.Source
       Response.Write "Description: " & err.Description
       err.Clear
     end if
END SUB %>

</head>

<body>
<h1>Functional Developer OLE Bank</h1>
<P><strong>Connected to &quot;<%= bank.name %>&quot;</strong></P>

<%
  select case Request("Action")
    case "Credit"
      call credit_account
    case "Debit"
      call debit_account
    case "Lookup"
      call find_account
    case "Open"
      call open_account
    case ""
      'Response.Write "No action requested"
    case else
      Response.Write("Unknown command <em>" & Request("Action") & "</em>")
  end select
%>


<H2>Select a Transaction</H2>

<FORM method="post">
<% IF accounts.Count > 0 THEN %>
 <INPUT TYPE="RADIO" NAME="Action" VALUE="Credit">Credit Selected Account
 <INPUT TYPE="RADIO" NAME="Action" VALUE="Debit">Debit Selected Account
 <BLOCKQUOTE>
   <SELECT NAME="Account">
   <% FOR EACH key IN accounts
       set account = accounts(key)
       response.write "<OPTION "
       if account Is selectedAcct then
         response.write "SELECTED "
       end if
       aname = account.name
       response.write "Value=" & q & aname & q & ">"
       response.write aname & " (balance = $" & account.balance & ")"
      NEXT
   %>
   </SELECT> Amount: <INPUT TYPE="TEXT" NAME="Amount">
 </BLOCKQUOTE>
<HR>
<% END IF %>

<INPUT CHECKED TYPE="RADIO" NAME="Action" VALUE="Lookup">Lookup Account
  Name:  <INPUT TYPE="TEXT" NAME="findName">
<BR>
<INPUT TYPE="RADIO" NAME="Action" VALUE="Open">Open a New Account
  Name:  <INPUT TYPE="TEXT" NAME="openName">
<HR>
<BR>
<INPUT TYPE="SUBMIT" VALUE="Submit"> <INPUT TYPE="RESET">
<BR>
</FORM>

</body>
</html>
