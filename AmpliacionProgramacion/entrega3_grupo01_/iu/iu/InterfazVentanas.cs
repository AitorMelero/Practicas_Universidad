using System.Windows.Forms;
using System;
using static System.Console;
using System.Collections.Generic;
using System.Drawing;


namespace iu
{
    public class InterfazVentanas : Interfaz
    {

		Calculadora form;
		Form newForm;

		public override void MostrarMosaico()
		{
			Dictionary<Provincia, Color> diccionarioColores;
			if (mapa != null)
			{
				diccionarioColores = mapa.coloreado();
			}
			else
			{
				diccionarioColores = new Dictionary<Provincia, Color>();
			}
			
			
			try
			{
				foreach (Provincia provincia in mapa.provincias)
				{
					mosaico = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas, provincia, diccionarioColores[provincia]);
				}
			}
			catch
			{

			}
			
			var	bitmap = new Bitmap(mosaico.filas, mosaico.columnas);

			String mosaicoString = mosaico.ToString();

			for (var x = 0; x < bitmap.Width; x++)
			{
				for (var y = 0; y < bitmap.Height; y++)
				{
					if (mosaico.mosaico[x][y] == 'R')
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Red);
					}
					else if (mosaico.mosaico[x][y] == 'A')
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Blue);
					}
					else if (mosaico.mosaico[x][y] == 'V')
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Green);
					}
					else if (mosaico.mosaico[x][y] == 'N')
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Orange);
					}
					else if (mosaico.mosaico[x][y] == 'M')
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Purple);
					}
					else
					{
						bitmap.SetPixel(x, y, System.Drawing.Color.Gray);
					}
				}
			}

			if (newForm == null)
			{
				newForm = new Form
				{
					Name = "Mapa",
					Size = new System.Drawing.Size(210, 210),
					Location = new System.Drawing.Point(140, 170),
					Visible = true,
				};
			}
			
			Bitmap result = new Bitmap(210, 210);
			using (Graphics g = Graphics.FromImage(result))
			{
				g.DrawImage(bitmap, 0, 0, 210, 210);
			}
			
			PictureBox pictureBox1 = new PictureBox();
			pictureBox1.Size = new Size(210, 110);

			pictureBox1.Image = result;
			pictureBox1.Dock = DockStyle.Fill;
			try
			{
				newForm.Controls.Remove(pictureBox1);
			}
			catch { }
			
			newForm.Controls.Add(pictureBox1);


			pictureBox1.Show();
			// this.form.Show(newForm);
			newForm.Show();


			// MessageBox.Show("Resultado", mosaico + "", MessageBoxButtons.OK);
		}
	
        public InterfazVentanas() => this.form = new Calculadora(this);
        public override void IntroducirCoordenadas() => this.form.ShowDialog();
		/*public override void RealizarOperacion() 
        {
            // sin código pues la operación se realiza al pulsar el botón "Operar"
        }*/
		public override bool ConfirmarContinuacion()
        {
            DialogResult resultado = MessageBox.Show("¿Repetir?", "Confirmar continuación", MessageBoxButtons.YesNo);
			if (resultado == DialogResult.Yes)
			{
				newForm.Hide();
				newForm = null;
			}
			else
				newForm.Hide();
			return (resultado == DialogResult.Yes);
        }
		
	}
}
