using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	public class Mapa
	{
		public Mapa(List<Provincia> provincias)
		{
			/*bucle 1*/
			foreach (Provincia p in provincias)
			{
				/*Bucle 2*/
				foreach (Provincia pAux in provincias)
				{
					if (!(p == pAux))
					{
						if (p.overlap(pAux))
						{
							throw new OverlapException("[ERROR] Superposicion");
						}
					}
				}
			}

			this.provincias = provincias;
		}

		public List<Provincia> provincias
		{
			get;
			private set;
		}

		public void addProvincia(Provincia provincia)
		{
			/*Comprobacion superposicion*/
			foreach (Provincia pAux in this.provincias)
			{
				if (pAux.overlap(provincia))
				{	
					throw new OverlapException("[ERROR] Superposicion");	
				}
			}

			if (!this.provincias.Contains(provincia))
			{
				this.provincias.Add(provincia);
			}
		}

		public List<Provincia> getFronteras(Provincia provincia)
		{
			List<Provincia> listaProvincias = new List<Provincia>();
			foreach (Provincia provinciaAux in this.provincias)
			{
				if (provincia.sonVecinos(provinciaAux))
				{
					listaProvincias.Add(provinciaAux);
				}
			}

			return listaProvincias;
		}

		public Dictionary<Provincia, Color> coloreado()
		{
			Dictionary<Provincia, Color> diccionario = new Dictionary<Provincia, Color>();
			List<Provincia> vecinos;

			/*Recorremos todas las provincias en el mapa*/
			foreach (Provincia provincia in this.provincias)
			{
				/*Lista para los colores que no puede contener*/
				List<Color> colores = new List<Color>();

				/*Recorremos todos los vecinos de la provincia*/
				vecinos = this.getFronteras(provincia);
				foreach (Provincia vecino in vecinos)
				{
					/*Compruebo si ya la he anyadido*/
					if (diccionario.ContainsKey(vecino))
					{
						/*Si la he anyadido antes lo anyado a la lista de colores*/
						Color color = diccionario[vecino];
						if (!colores.Contains(color))
						{
							colores.Add(color);
						}
					}
				}
				/*El color de la provincia es el que no tengan sus vecinos*/
				/*Iteramos la enumeracion Color y buscamos el primero que no está en colores*/
				var coloresPosibles = Enum.GetValues(typeof(Color));
				foreach (Color c in coloresPosibles)
				{
					if (!colores.Contains(c))
					{
						diccionario.Add(provincia, c);
						break;
					}
				}				
			}

			return diccionario;
		}

		public Dictionary<Provincia, Color> coloreadoRestricted(List<Color> coloresPosibles)
		{
			Dictionary<Provincia, Color> diccionario = new Dictionary<Provincia, Color>();
			List<Provincia> vecinos;

			/*Recorremos todas las provincias en el mapa*/
			foreach (Provincia provincia in this.provincias)
			{
				/*Lista para los colores que no puede contener*/
				List<Color> colores = new List<Color>();

				/*Recorremos todos los vecinos de la provincia*/
				vecinos = this.getFronteras(provincia);
				foreach (Provincia vecino in vecinos)
				{
					/*Compruebo si ya la he anyadido*/
					if (diccionario.ContainsKey(vecino))
					{
						/*Si la he anyadido antes lo anyado a la lista de colores*/
						Color color = diccionario[vecino];
						if (!colores.Contains(color))
						{
							colores.Add(color);
						}
					}
				}
				/*El color de la provincia es el que no tengan sus vecinos*/
				/*Iteramos los colores posibles y buscamos el primero que no está en colores*/
				foreach (Color c in coloresPosibles)
				{
					if (!colores.Contains(c))
					{
						diccionario.Add(provincia, c);
						break;
					}
				}

				
			}

			return diccionario;
		}

		public override string ToString()
		{
			String mapa = "";

			mapa += "MAPA:\n";

			foreach (Provincia provincia in this.provincias)
			{
				mapa += "\t" + provincia;
				mapa += "\n";
			}
			return mapa;
		}
	}


}

