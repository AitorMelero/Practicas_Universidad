using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	public class MapaRegion
	{
		public MapaRegion(List<Region> regiones)
		{
			this.regiones = regiones;
		}

		public List<Region> regiones
		{
			get;
			private set;
		}

		public void addRegion(Region region)
		{
			this.regiones.Add(region);
		}

		public List<Region> getFronteras(Region region)
		{
			List<Region> listaRegiones = new List<Region>();
			/*Iteramos todas las regiones del mapa*/
			foreach (Region regionAux in this.regiones)
			{
				if (regionAux != region)
				{
					/*Iteramos todas las provincias en cada region del mapa*/
					foreach (Provincia provinciaAux1 in regionAux.provincias)
					{
						/*Iteramos todas las provincias de la region a comprobar*/
						foreach (Provincia provinciaAux2 in region.provincias)
						{
							/*Si son vecinos*/
							if (provinciaAux1.sonVecinos(provinciaAux2))
							{
								/*Si no lo hemos anyadido antes*/
								if (!listaRegiones.Contains(regionAux))
								{
									listaRegiones.Add(regionAux);
								}
							}
						}
					}
				}
			}

			return listaRegiones;
		}

		public Dictionary<Region, Color> coloreado()
		{
			Dictionary<Region, Color> diccionario = new Dictionary<Region, Color>();
			List<Region> vecinos;

			/*Recorremos todas las regiones en el mapa*/
			foreach (Region region in this.regiones)
			{
				/*Lista para los colores que no puede contener*/
				List<Color> colores = new List<Color>();

				/*Recorremos todos los vecinos de la provincia*/
				vecinos = this.getFronteras(region);
				foreach (Region vecino in vecinos)
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
						diccionario.Add(region, c);
						break;
					}
				}
			}

			return diccionario;
		}



		public override string ToString()
		{
			String mapa = "";

			mapa += "MAPA REGIOMES:\n";

			foreach (Region region in this.regiones)
			{
				mapa += "\t" + region.ToString();
				mapa += "\n";
			}
			return mapa;
		}
	}


}

