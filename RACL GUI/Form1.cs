using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace RACL_GUI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                var compiler = new Compiler.Compiler();
                var compiledFunctions = compiler.Compile(richTextBox1.Text);
                listBox1.Items.Clear();
                listBox1.Items.AddRange(compiledFunctions);
            }
            catch (Exception es)
            {
                MessageBox.Show("Compilation Failed\n" + es.Message);
                listBox1.Items.Clear();
                richTextBox2.Text = "";
            }
        }

        private void listBox1_SelectedValueChanged(object sender, EventArgs e)
        {
            if (listBox1.SelectedItem != null)
            {
                var func = (CompilerTypes.FunctionDeclaration)listBox1.SelectedItem;
                richTextBox2.Text = func.Body;
            }
        }
    }
}
