using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainMosaico
	{
		public static void MainMos()
		{
			Mosaico mosaico = new Mosaico(11, 24);
			Provincia hu = new Provincia(new Coordenada(4, 6), new Coordenada(5, 8));

			Console.WriteLine("Mosaico vacio");
			Console.WriteLine(mosaico);
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Mosaico mosaicoAux = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas ,hu, Color.Rojo);
			Console.WriteLine("Mosaico provincia roja");
			Console.WriteLine(mosaicoAux);
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.ReadLine();

		}

	}
}
