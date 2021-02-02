using System;
using System.Windows.Forms;
using System.Collections.Generic;

namespace iu
{
    public partial class Calculadora : Form
    {

        private InterfazVentanas intVen;
        public Calculadora(InterfazVentanas i)
        {
            this.intVen = i;
            InitializeComponent();
		}
        private void button1_Click(object sender, EventArgs e)
        {
			/*Enviamos los datos de la provincia y lo annadimos al mapa*/
			this.intVen.x1 = Convert.ToInt32(this.textBox1.Text);
			this.intVen.y1 = Convert.ToInt32(this.textBox2.Text);

			this.intVen.x2 = Convert.ToInt32(this.textBox3.Text);
			this.intVen.y2 = Convert.ToInt32(this.textBox4.Text);

			if (this.intVen.mapa == null)
			{
				this.intVen.mapa = new Mapa(new List<Provincia>());
			}
			this.intVen.AddProvincia();
			MessageBox.Show("Acabamos de annadir la provincia :)", "Informacion", MessageBoxButtons.OK);
		}
        private void button3_Click(object sender, EventArgs e)
            => this.DialogResult = DialogResult.No;
		private void Calculadora_Load(object sender, EventArgs e)
		{

		}

		private void label1_Click(object sender, EventArgs e)
		{

		}

		private void label2_Click(object sender, EventArgs e)
		{

		}
	}
}
