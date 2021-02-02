using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	public class Region
	{
		public Region(List<Provincia> provincias)
		{
			/*bucle 1*/
			foreach (Provincia p in provincias)
			{
				bool vecino = false;
				/*Bucle 2*/
				foreach (Provincia pAux in provincias)
				{
					if (!(p == pAux))
					{
						if (p.overlap(pAux))
						{
							throw new OverlapException("[ERROR] Superposicion");
						}
						if (p.sonVecinos(pAux))
						{
							vecino = true;
						}
					}
				}

				if (!vecino && provincias.Count > 1)
				{
					throw new RegionException("[ERROR] Una provincia no tiene vecino en la region");
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
			bool vecino = false;
			/*Comprobacion superposicion*/
			foreach (Provincia pAux in this.provincias)
			{
				if (pAux.overlap(provincia))
				{
					throw new OverlapException("[ERROR] Superposicion");
				}
				if (!vecino)
				{
					throw new RegionException("[ERROR] Una provincia no tiene vecino en la region");
				}
			}

			if (!this.provincias.Contains(provincia))
			{
				this.provincias.Add(provincia);
			}
		}

		public override string ToString()
		{
			String mapa = "";

			mapa += "REGION:\n";

			foreach (Provincia provincia in this.provincias)
			{
				mapa += "\t" + provincia;
				mapa += "\n";
			}
			return mapa;
		}
	}


}

