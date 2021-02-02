using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	class MainProvincia
	{
		public static void MainProv()
		{
			Provincia hu = new Provincia(new Coordenada(4, 6), new Coordenada(5, 8));
			Provincia ca = new Provincia(new Coordenada(6, 8), new Coordenada(8, 9));
			Provincia se = new Provincia(new Coordenada(6, 5), new Coordenada(10, 7));
			Provincia ma = new Provincia(new Coordenada(9, 8), new Coordenada(16, 9));
			Provincia co = new Provincia(new Coordenada(11, 6), new Coordenada(16, 7));
			Provincia ja = new Provincia(new Coordenada(13, 3), new Coordenada(16, 5));
			Provincia gr = new Provincia(new Coordenada(17, 4), new Coordenada(19, 8));
			Provincia al = new Provincia(new Coordenada(20, 6), new Coordenada(22, 8));

			Console.WriteLine(ca);
			Console.WriteLine(se);
			Console.WriteLine("Almeria");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(al.sonVecinos(gr));
			Console.WriteLine(al.sonVecinos(hu));
			Console.WriteLine(al.sonVecinos(ca));
			Console.WriteLine(al.sonVecinos(se));
			Console.WriteLine(al.sonVecinos(ma));
			Console.WriteLine(al.sonVecinos(co));
			Console.WriteLine(al.sonVecinos(ja));
			Console.WriteLine(al.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("Cadiz");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ca.sonVecinos(hu));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ca.sonVecinos(se));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ca.sonVecinos(ma));
			Console.WriteLine(ca.sonVecinos(ca));
			Console.WriteLine(ca.sonVecinos(co));
			Console.WriteLine(ca.sonVecinos(ja));
			Console.WriteLine(ca.sonVecinos(gr));
			Console.WriteLine(ca.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("Cordoba");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(co.sonVecinos(se));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(co.sonVecinos(ma));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(co.sonVecinos(ja));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(co.sonVecinos(gr));
			Console.WriteLine(co.sonVecinos(hu));
			Console.WriteLine(co.sonVecinos(ca));
			Console.WriteLine(co.sonVecinos(co));
			Console.WriteLine(co.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("Grana");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(gr.sonVecinos(ma));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(gr.sonVecinos(co));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(gr.sonVecinos(ja));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(gr.sonVecinos(al));
			Console.WriteLine(gr.sonVecinos(hu));
			Console.WriteLine(gr.sonVecinos(ca));
			Console.WriteLine(gr.sonVecinos(se));
			Console.WriteLine(gr.sonVecinos(gr));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("jaen");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ja.sonVecinos(co));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ja.sonVecinos(gr));
			Console.WriteLine(ja.sonVecinos(hu));
			Console.WriteLine(ja.sonVecinos(ca));
			Console.WriteLine(ja.sonVecinos(se));
			Console.WriteLine(ja.sonVecinos(ma));
			Console.WriteLine(ja.sonVecinos(ja));
			Console.WriteLine(ja.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("huelva");
			Console.WriteLine(hu.sonVecinos(hu));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(hu.sonVecinos(ca));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(hu.sonVecinos(se));
			Console.WriteLine(hu.sonVecinos(ma));
			Console.WriteLine(hu.sonVecinos(co));
			Console.WriteLine(hu.sonVecinos(ja));
			Console.WriteLine(hu.sonVecinos(gr));
			Console.WriteLine(hu.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("malaga");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ma.sonVecinos(ca));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ma.sonVecinos(se));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ma.sonVecinos(co));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(ma.sonVecinos(gr));

			Console.WriteLine(ma.sonVecinos(hu));
			Console.WriteLine(ma.sonVecinos(ma));
			Console.WriteLine(ma.sonVecinos(ja));
			Console.WriteLine(ma.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.WriteLine("se");
			Console.WriteLine("El siguiente True");
			Console.WriteLine(se.sonVecinos(hu));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(se.sonVecinos(ca));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(se.sonVecinos(ma));
			Console.WriteLine("El siguiente True");
			Console.WriteLine(se.sonVecinos(co));

			Console.WriteLine(se.sonVecinos(se));
			Console.WriteLine(se.sonVecinos(ja));
			Console.WriteLine(se.sonVecinos(gr));
			Console.WriteLine(se.sonVecinos(al));
			Console.WriteLine(" ");
			Console.WriteLine(" ");

			Console.ReadLine();

		}

	}
}
