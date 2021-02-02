using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainMapa
	{
		public static void MainMap()
		{
			Provincia hu = new Provincia(new Coordenada(4, 6), new Coordenada(5, 8));
			Provincia ca = new Provincia(new Coordenada(6, 8), new Coordenada(8, 9));
			Provincia se = new Provincia(new Coordenada(6, 5), new Coordenada(10, 7));
			Provincia ma = new Provincia(new Coordenada(9, 8), new Coordenada(16, 9));
			Provincia co = new Provincia(new Coordenada(11, 6), new Coordenada(16, 7));
			Provincia ja = new Provincia(new Coordenada(13, 3), new Coordenada(16, 5));
			Provincia gr = new Provincia(new Coordenada(17, 4), new Coordenada(19, 8));
			Provincia al = new Provincia(new Coordenada(20, 6), new Coordenada(22, 8));

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

			Console.WriteLine("");
			Console.WriteLine("");
			Console.WriteLine("Fronteras de almeria: [(17, 4) , (19, 8) ]");
			Console.WriteLine(mapa.getFronteras(al)[0]);

			Console.WriteLine("");
			Console.WriteLine("");
			Console.WriteLine("Fronteras de cordoba:");
			Console.WriteLine(mapa.getFronteras(co)[0]);
			Console.WriteLine(mapa.getFronteras(co)[1]);
			Console.WriteLine(mapa.getFronteras(co)[2]);
			Console.WriteLine(mapa.getFronteras(co)[3]);

			Console.ReadLine();

		}

	}
}
