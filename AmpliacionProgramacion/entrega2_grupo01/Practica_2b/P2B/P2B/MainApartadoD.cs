using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainApartadoD
	{
		public static void MainD()
		{

			/*Creamos el mapa inicial, por suerte no me importan las dimensiones del mosaico*/
			Mapa mapa = new Mapa(new List<Provincia>());
			
			/*Creamos el mosaico. De momento de 20x20*/
			Mosaico mosaico = new Mosaico(20, 20);

			string caracter;
			Console.WriteLine("¿Desea introducir un nuevo rectangulo?");
			caracter = Console.ReadLine();

			while (caracter.Equals("S"))
			{
				/*******************************************************************************/
				/*******************************Pedir rectangulos*******************************/
				/*******************************************************************************/
				/*Obtenemos las coordenadas del rectangulo*/
				Console.WriteLine("Introduzca la coordenada x1 (x superior izquierda):");
				String x1Aux = Console.ReadLine();
				int x1 = Convert.ToInt32(x1Aux);

				Console.WriteLine("Introduzca la coordenada y1 (y superior izquierda):");
				String y1Aux = Console.ReadLine();
				int y1 = Convert.ToInt32(y1Aux);

				Console.WriteLine("Introduzca la coordenada x2 (x inferior derecha):");
				String x2Aux = Console.ReadLine();
				int x2 = Convert.ToInt32(x2Aux);

				Console.WriteLine("Introduzca la coordenada y1 (y inferior derecha):");
				String y2Aux = Console.ReadLine();
				int y2 = Convert.ToInt32(y2Aux);

				/*Lo metemos en el mapa*/
				try
				{
					mapa.addProvincia(new Provincia(new Coordenada(x1, y1), new Coordenada(x2, y2)));

					/*******************************************************************************/
					/*********************************Pedir colores*********************************/
					/*******************************************************************************/

					/*Creamos la lista de colores que vamos a pasa*/
					List<Color> listaColores = new List<Color>();

					/*Peticion de cosas de colores*/
					string comprobacionColor = "S";
					while (comprobacionColor.Equals("S"))
					{

						Console.WriteLine("Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:");
						string color = Console.ReadLine();
						
						/*Comprobamos que color ha elegido*/
						if (color.Equals("Verde") || color.Equals("VERDE") || color.Equals("verde"))
						{
							if (!listaColores.Contains(Color.Verde))
							{
								listaColores.Add(Color.Verde);
							}
						}
						if (color.Equals("Azul") || color.Equals("AZUL") || color.Equals("azul"))
						{
							if (!listaColores.Contains(Color.Azul))
							{
								listaColores.Add(Color.Azul);
							}
						}
						if (color.Equals("Rojo") || color.Equals("ROJO") || color.Equals("rojo"))
						{
							if (!listaColores.Contains(Color.Rojo))
							{
								listaColores.Add(Color.Rojo);
							}
						}
						if (color.Equals("Naranja") || color.Equals("NARANJA") || color.Equals("naranja"))
						{
							if (!listaColores.Contains(Color.Naranja))
							{
								listaColores.Add(Color.Naranja);
							}
						}
						if (color.Equals("Morado") || color.Equals("MORADO") || color.Equals("morado"))
						{
							if (!listaColores.Contains(Color.Morado))
							{
								listaColores.Add(Color.Morado);
							}
						}

						Console.WriteLine("¿Desea introducir un nuevo color?");
						comprobacionColor = Console.ReadLine();
					}

					Dictionary<Provincia, Color> diccionarioColores = mapa.coloreadoRestricted(listaColores);

					foreach (Provincia provincia in mapa.provincias)
					{
						mosaico = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas, provincia, diccionarioColores[provincia]);
					}

					Console.WriteLine(mosaico);
					Console.WriteLine(" ");
					Console.WriteLine(" ");

				} catch (Exception e)
				{
					Console.WriteLine(e.ToString());
				}
				

				/*Para ver si repetimos el bucle o no*/
				Console.WriteLine("¿Desea introducir un nuevo rectangulo?");
				caracter = Console.ReadLine();
			}

			Console.WriteLine("Ejecucion finalizada. Pulse Enter para finalizar");
			Console.ReadLine();

		}

	}
}

/*
¿Desea introducir un nuevo rectangulo?
S
Introduzca la coordenada x1(x superior izquierda):
1
Introduzca la coordenada y1(y superior izquierda):
2
Introduzca la coordenada x2(x inferior derecha):
3
Introduzca la coordenada y1(y inferior derecha):
4
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Verde
¿Desea introducir un nuevo color?
Rojo
....................
VVV.................
VVV.................
VVV.................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................



¿Desea introducir un nuevo rectangulo?
S
Introduzca la coordenada x1 (x superior izquierda):
1
Introduzca la coordenada y1(y superior izquierda):
2
Introduzca la coordenada x2(x inferior derecha):
5
Introduzca la coordenada y1(y inferior derecha):
6
P2B.OverlapException: [ERROR]
Superposicion
   en P2B.Mapa.addProvincia(Provincia provincia) en C:\Users\artii\Desktop\Practica_2b\P2B\P2B\Mapa.cs:línea 45
   en P2B.MainApartadoD.MainD() en C:\Users\artii\Desktop\Practica_2b\P2B\P2B\MainApartadoD.cs:línea 50
¿Desea introducir un nuevo rectangulo?
S
Introduzca la coordenada x1(x superior izquierda):
4
Introduzca la coordenada y1(y superior izquierda):
2
Introduzca la coordenada x2(x inferior derecha):
5
Introduzca la coordenada y1(y inferior derecha):
4
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Rojo
¿Desea introducir un nuevo color?
S
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Verde
¿Desea introducir un nuevo color?

....................
RRRVV...............
RRRVV...............
RRRVV...............
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................



¿Desea introducir un nuevo rectangulo?
S
Introduzca la coordenada x1(x superior izquierda):
3
Introduzca la coordenada y1(y superior izquierda):
5
Introduzca la coordenada x2(x inferior derecha):
5
Introduzca la coordenada y1(y inferior derecha):
7
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Rojo
¿Desea introducir un nuevo color?
S
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Verde
¿Desea introducir un nuevo color?
S
Introduzca un color entre el Rojo, Verde, Azul, Naranja y Morado:
Azul
¿Desea introducir un nuevo color?

....................
RRRVV...............
RRRVV...............
RRRVV...............
..AAA...............
..AAA...............
..AAA...............
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................
....................



¿Desea introducir un nuevo rectangulo?

Ejecucion finalizada.Pulse Enter para finalizar 
	*/