<% @language=vbscript %>
<html>
<head>
<title>Functional Developer ASP Viewer</title>
<%
  set viewer = Server.CreateObject("HQN.ASPView")
%>
</head>

<body>
<h1>Functional Developer ASP Viewer</h1>

<menu>
<li><A HREF="asp-view.asp?command=Session">  Show Session Info</A>
<li><A HREF="asp-view.asp?command=Variables">Show Server Variables</A>
<li><A HREF="asp-view.asp?command=Cookies">  Show Cookies</A>
<li><A HREF="asp-view.asp?command=Query">    Show Query</A>
<li><A HREF="asp-view.asp?command=Form">     Show Form</A>
<li><A HREF="asp-view.asp?command=All">      Show All</A>
</menu>

<p>--- Start of ASPView Output --- </p>
<%
  select case Request("command")
    case "Session"
	viewer.ShowSession
    case "Variables"
	viewer.ShowVariables
    case "Cookies"
	viewer.ShowCookies
    case "Query"
	viewer.ShowQuery
    case "Form"
	viewer.ShowForm
    case "All"
	viewer.ShowAll
    case ""
	' No command, do nothing
    case else
	Response.Write("Unknown command <em>" & Request("command") & "</em>")
  end select
%>
<p>--- End of ASPView Output --- </p>

</body>
</html>
