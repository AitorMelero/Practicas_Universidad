using System;
using static System.Console;
using System.Collections.Generic;

namespace iu
{
    public class InterfazConsola : Interfaz
    {
		public override void IntroducirCoordenadas()
		{
			Console.WriteLine("Introduzca la coordenada x1 (x superior izquierda):");
			String x1Aux = Console.ReadLine();
			x1 = Convert.ToInt32(x1Aux);

			Console.WriteLine("Introduzca la coordenada y1 (y superior izquierda):");
			String y1Aux = Console.ReadLine();
			y1 = Convert.ToInt32(y1Aux);

			Console.WriteLine("Introduzca la coordenada x2 (x inferior derecha):");
			String x2Aux = Console.ReadLine();
			x2 = Convert.ToInt32(x2Aux);

			Console.WriteLine("Introduzca la coordenada y1 (y inferior derecha):");
			String y2Aux = Console.ReadLine();
			y2 = Convert.ToInt32(y2Aux);
			Console.WriteLine("Introduzca la coordenada y1 (y inferior derecha):");
			/*Creamos la provincia y la annadimos la mapa*/
			if (mapa == null)
			{
				mapa = new Mapa(new List<Provincia>());
			}
			AddProvincia();
		}
		public override void MostrarMosaico()
		{
			Dictionary<Provincia, Color> diccionarioColores = mapa.coloreado();
			// mosaico = new Mosaico(20, 20);
			foreach (Provincia provincia in mapa.provincias)
			{
				mosaico = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas, provincia, diccionarioColores[provincia]);
			}
			Console.WriteLine(mosaico);
			Console.WriteLine(" ");
			Console.WriteLine(" ");
		}

		public override bool ConfirmarContinuacion()
		{
			Console.WriteLine("¿Desea introducir un nuevo rectangulo?");
			String caracter = Console.ReadLine();
			return caracter.Equals("S");
		}
	}
}
