using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainApartadoE
	{
		public static void MainE()
		{
			/*Partimos con Andalucia por comodidad*/
			Provincia hu = new Provincia(new Coordenada(4, 6), new Coordenada(5, 8));
			Provincia ca = new Provincia(new Coordenada(6, 8), new Coordenada(8, 9));
			Provincia se = new Provincia(new Coordenada(6, 5), new Coordenada(10, 7));
			Provincia ma = new Provincia(new Coordenada(9, 8), new Coordenada(16, 9));
			Provincia co = new Provincia(new Coordenada(11, 6), new Coordenada(16, 7));
			Provincia ja = new Provincia(new Coordenada(13, 3), new Coordenada(16, 5));
			Provincia gr = new Provincia(new Coordenada(17, 4), new Coordenada(19, 8));
			Provincia al = new Provincia(new Coordenada(20, 6), new Coordenada(22, 8));

			/*Provincia para la region de una sola provincnia*/
			Provincia pAux = new Provincia(new Coordenada(1,10), new Coordenada(10,10));

			/*Todo en tres regiones*/
			List<Provincia> lista1 = new List<Provincia>();
			lista1.Add(al);
			lista1.Add(gr);
			lista1.Add(ma);
			Region region1 = new Region(lista1);
			// Console.WriteLine(region1);

			List<Provincia> lista2 = new List<Provincia>();
			lista2.Add(ja);
			lista2.Add(co);
			lista2.Add(se);
			Region region2 = new Region(lista2);
			// Console.WriteLine(region2);

			List<Provincia> lista3 = new List<Provincia>();
			lista3.Add(hu);
			lista3.Add(ca);
			Region region3 = new Region(lista3);
			// Console.WriteLine(region3);

			List<Provincia> lista4 = new List<Provincia>();
			lista4.Add(pAux);
			Region region4 = new Region(lista4);
			// Console.WriteLine(region4);

			/*Lista de regiones*/
			List<Region> regiones = new List<Region>();
			regiones.Add(region1);
			regiones.Add(region2);
			regiones.Add(region3);
			regiones.Add(region4);


			MapaRegion mapa = new MapaRegion(regiones);

			Dictionary<Region,Color> diccionarioColores = mapa.coloreado();

			/*Creamos el mosaico. De momento de 20x20*/
			Mosaico mosaico = new Mosaico(20, 30);

			foreach(Region r in regiones)
			{
				foreach(Provincia p in r.provincias)
				{
					mosaico = new Mosaico(mosaico.mosaico, mosaico.filas, mosaico.columnas, p, diccionarioColores[r]);
				}
			}

			Console.WriteLine(mosaico);
			Console.ReadLine();

		}

	}
}

/*
..............................
..............................
............AAAA..............
............AAAARRR...........
.....AAAAA..AAAARRR...........
...VVAAAAAAAAAAARRRRRR........
...VVAAAAAAAAAAARRRRRR........
...VVVVVRRRRRRRRRRRRRR........
.....VVVRRRRRRRR..............
AAAAAAAAAA....................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
..............................
.............................. 
 */