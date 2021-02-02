using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainExcepciones
	{
		public static void MainExcep()
		{
			Provincia hu = new Provincia(new Coordenada(4, 6), new Coordenada(5, 8));
			Provincia ca = new Provincia(new Coordenada(6, 8), new Coordenada(8, 9));
			Provincia se = new Provincia(new Coordenada(6, 5), new Coordenada(10, 7));
			Provincia ma = new Provincia(new Coordenada(9, 8), new Coordenada(16, 9));
			Provincia co = new Provincia(new Coordenada(11, 6), new Coordenada(16, 7));
			Provincia ja = new Provincia(new Coordenada(13, 3), new Coordenada(16, 5));
			Provincia gr = new Provincia(new Coordenada(17, 4), new Coordenada(19, 8));
			Provincia al = new Provincia(new Coordenada(20, 6), new Coordenada(22, 8));

			Console.WriteLine("Comprobamos la excepcion de provincia incorrecta:");
			try
			{
				Provincia provinciaIncorrecta = new Provincia(new Coordenada(22, 8), new Coordenada(20, 6));
			} catch (IncorrectProvincia e)
			{
				Console.WriteLine("Excepcion de provincia incorrecta");
				Console.WriteLine(" ");
				Console.WriteLine(" ");
			}

			Console.WriteLine("Creamos el mapa de andalucia");
			Mapa mapa = new Mapa(new List<Provincia>());
			mapa.addProvincia(hu);
			mapa.addProvincia(ca);
			mapa.addProvincia(se);
			mapa.addProvincia(ma);
			mapa.addProvincia(co);
			mapa.addProvincia(ja);
			mapa.addProvincia(gr);
			mapa.addProvincia(al);

			Console.WriteLine(mapa);

			Console.WriteLine("Comprobamos la excepcion de las provincias que se solapan");
			Provincia provinciaSuperposicion1 = new Provincia(new Coordenada(2, 8), new Coordenada(16, 9));
			Provincia provinciaSuperposicion2 = new Provincia(new Coordenada(3, 8), new Coordenada(7, 9));
			Console.WriteLine(provinciaSuperposicion1);
			Console.WriteLine(provinciaSuperposicion2);
			Console.WriteLine("Anyadimos la primera");
			try
			{
				mapa.addProvincia(provinciaSuperposicion1);
			}
			catch (OverlapException e)
			{
				Console.WriteLine("Excepcion de superposicion para la primera provincia");
			}
			Console.WriteLine("Anyadimos la segunda");
			try
			{
				mapa.addProvincia(provinciaSuperposicion2);
			}
			catch (OverlapException e)
			{
				Console.WriteLine("Excepcion de superposicion para la segunda provincia");
			}
			Console.WriteLine(" ");

			Console.WriteLine("Comprobamos la excepcion de las provincias que se estan fuera del mosaico");
			Console.WriteLine("Creamos un mosaico de tamanyo insuficiente");
			Mosaico mosaico = new Mosaico(2, 3);

			try
			{
				Mosaico mosaicoAux = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas, hu, Color.Rojo);
			} catch (OutOfLimitsException e)
			{
				Console.WriteLine("Error fuera de los limites");
			}


			Console.WriteLine(" ");


			Console.ReadLine();

		}

	}
}
/*
Comprobamos la excepcion de provincia incorrecta:
Excepcion de provincia incorrecta


Creamos el mapa de andalucia
MAPA:
        [(4, 6) , (5, 8) ]
        [(6, 8) , (8, 9) ]
        [(6, 5) , (10, 7) ]
        [(9, 8) , (16, 9) ]
        [(11, 6) , (16, 7) ]
        [(13, 3) , (16, 5) ]
        [(17, 4) , (19, 8) ]
        [(20, 6) , (22, 8) ]

Comprobamos la excepcion de las provincias que se solapan
[(2, 8) , (16, 9) ]
[(3, 8) , (7, 9) ]
Anyadimos la primera
Excepcion de superposicion para la primera provincia
Anyadimos la segunda
Excepcion de superposicion para la segunda provincia

Comprobamos la excepcion de las provincias que se estan fuera del mosaico
Creamos un mosaico de tamanyo insuficiente
Error fuera de los limites
 */