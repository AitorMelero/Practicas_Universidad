using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static System.Console;


namespace iu
{

	public enum Color { Rojo, Azul, Verde, Naranja, Morado };

	public class Mosaico
	{
		public Mosaico(int filas, int columnas)
		{
			this.mosaico = new List<string>();
			this.filas = filas;
			this.columnas = columnas;

			/*Asignamos al tablero por defecto todas las casillas como vacias*/
			for (int i = 0; i < filas; i++)
			{
				this.mosaico.Add(new String('.', columnas));
			}
		}

		/*Construtores guapos para a�adir cositas*/
		public Mosaico(List<string> oldMosaico, int fila, int columna, Color color)
		{
			if (fila < 1 || columna < 1 || fila < this.filas || columna < this.columnas)
			{
				throw new OutOfLimitsException("[ERROR] Nos salimos de los limites");
			}
			List<string> newMosaico = new List<string>();
			/*Clonacion del tablero*/
			foreach (string aux in oldMosaico)
			{
				newMosaico.Add((string)aux.Clone()); // As� es una copia y no la misma referencia
			}

			/*Cambiamos la nueva casilla*/
			var newMosaicoAux = new StringBuilder(newMosaico[fila - 1]);
			newMosaicoAux[columna - 1] = getCaracter(color);
			newMosaico[fila - 1] = newMosaicoAux.ToString();
			this.mosaico = newMosaico;
		}

		public Mosaico(List<string> oldMosaico, int filas, int columnas, Provincia provincia, Color color)
		{
			/*Comprobacion por si la provincia se sale del mosaico*/
			if (provincia.arribaIzquierda.X < 1 || provincia.arribaIzquierda.Y < 1
				|| provincia.arribaIzquierda.X > columnas || provincia.arribaIzquierda.Y > filas)
			{
				throw new OutOfLimitsException("[ERROR] Nos salimos de los limites");
			}

			if (provincia.abajoDerecha.X < 1 || provincia.abajoDerecha.Y < 1 
				|| provincia.abajoDerecha.X > columnas || provincia.abajoDerecha.Y > filas)
			{
				throw new OutOfLimitsException("[ERROR] Nos salimos de los limites");
			}

			List<string> newMosaico = new List<string>();
			/*Clonacion del tablero*/
			foreach (string aux in oldMosaico)
			{
				newMosaico.Add((string)aux.Clone()); // As� es una copia y no la misma referencia
			}

			/*Cambiamos la provincia*/
			for (int i = provincia.arribaIzquierda.Y; i <= provincia.abajoDerecha.Y; i++)
			{
				var newMosaicoAux = new StringBuilder(newMosaico[i - 1]);
				for (int j = provincia.arribaIzquierda.X; j <= provincia.abajoDerecha.X; j++)
				{
					newMosaicoAux[j - 1] = getCaracter(color);
				}
				newMosaico[i - 1] = newMosaicoAux.ToString();
			}
			
			this.mosaico = newMosaico;
			this.filas = filas;
			this.columnas = columnas;
		}

		public Mosaico(List<string> oldMosaico)
		{
			List<string> newMosaico = new List<string>();
			/*Clonacion del tablero*/
			foreach (string aux in oldMosaico)
			{
				newMosaico.Add((string)aux.Clone()); // As� es una copia y no la misma referencia
			}

			this.mosaico = newMosaico;
		}

		/*getters y setters*/
		public List<string> mosaico
		{
			get;
			private set;
		}

		public int filas
		{
			get;
			private set;
		}

		public int columnas
		{
			get;
			private set;
		}

		public char getCaracter (Color color)
		{
			if (color == Color.Rojo)
			{
				return 'R';
			}
			if (color == Color.Verde)
			{
				return 'V';
			}
			if (color == Color.Azul)
			{
				return 'A';
			}
			if (color == Color.Morado)
			{
				return 'M';
			}
			if (color == Color.Naranja)
			{
				return 'N';
			}
			return '.';
		}

		public override string ToString()
		{
			string mosaico = "";
			for (int i = 0; i < this.filas; i++)
			{
				for (int j = 0; j < this.columnas; j++)
				{
					mosaico += this.mosaico[i][j];
				}
				mosaico += "\n";
			}
			return mosaico;
		}

	}	
}