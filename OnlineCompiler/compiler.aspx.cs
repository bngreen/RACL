using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace OnlineCompiler
{
    public partial class compiler : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            var c_code = Request.Params["CCodeTB"];
            if (c_code != null)
            {
                CCodeTB.Text = c_code;
                try
                {
                    var compiler = new Compiler.Compiler();
                    var code_list = compiler.Compile(c_code);
                    var cd = String.Concat(from x in code_list select String.Format("// program: {0}\n\n{1}\n//end program: {0}\n\n\n", x.Name, x.Body));
                    ACLCodeTB.Text = cd;
                }
                catch (Exception es)
                {
                    ACLCodeTB.Text = String.Format("Compilation Error: {0}", es.Message);
                }
            }
        }
    }
}