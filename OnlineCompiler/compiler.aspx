<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="compiler.aspx.cs" Inherits="OnlineCompiler.compiler" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Simplified C to ACL Online Compiler</title>

</head>
<body>
    <form id="form1" runat="server" action="compiler.aspx" style="height:100%;width:100%;">
    <div>
    
    &nbsp;
        <asp:TextBox ID="CCodeTB" runat="server" Height="90%" TextMode="MultiLine" 
            Width="49%" Rows="60" Wrap="False">//Insert Your Code Here</asp:TextBox>
        <asp:TextBox ID="ACLCodeTB" runat="server" Height="90%" ReadOnly="True" 
            TextMode="MultiLine" Width="49%" Rows="60" Wrap="False">The Compiled Code will be here
</asp:TextBox>
    </div>
                <p>
                <input id="Submit1" type="submit" value="Compile" /></p>
    </form>


</body>
</html>
