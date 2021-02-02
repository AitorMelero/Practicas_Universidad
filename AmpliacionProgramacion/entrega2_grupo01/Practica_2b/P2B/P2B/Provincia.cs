using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P2B
{
	public class Provincia
	{
		public Provincia(Coordenada arribaIzquierda, Coordenada abajoDerecha)
		{
			if (arribaIzquierda.X > abajoDerecha.X)
			{
				throw new IncorrectProvincia("[ERROR] provincia incorrecta"); 
			}

			if (arribaIzquierda.Y > abajoDerecha.Y)
			{
				throw new IncorrectProvincia("[ERROR] provincia incorrecta"); 
			}

			this.arribaIzquierda = arribaIzquierda;
			this.abajoDerecha = abajoDerecha;
		}

		/*getters y setters*/
		public Coordenada arribaIzquierda
		{
			get;
			private set;
		}

		public Coordenada abajoDerecha
		{
			get;
			private set;
		}

		/*Funcion que comprueba si una provincia es frontera/vecina de otra*/
		/*TODO testear esto que está jodido*/
		public bool sonVecinos (Provincia p)
		{
			/* Codigo de haskell para no perderme je
				sonVecinos (Provincia (x11, y11) (x12, y12)) (Provincia (x21, y21) (x22, y22))
				| x11 == x21 && x12 == x22 && y11 == y21 && y12 == y22 = False
				| (x11 == x22 + 1 || x12 == x21 - 1 || x11 == x22 - 1 || x12 == x21 + 1) && length (intersect [y12 .. y11] [y22 .. y21]) >= 1 = 
					True
				| (y11 == y22 + 1 || y12 == y21 - 1 || y11 == y22 - 1 || y12 == y21 + 1) && length (intersect [x11 .. x12] [x21 .. x22]) >= 1 =
					True
				| otherwise = False

				Las provincias estan abajo_izquierda, arriba_derecha
			*/
			Coordenada c11 = this.arribaIzquierda;
			Coordenada c12 = this.abajoDerecha;

			Coordenada c21 = p.arribaIzquierda;
			Coordenada c22 = p.abajoDerecha;

			/*Ahora escribimos las comparaciones*/
			/*Caso es la misma provincia*/
			if (this == p)
			{
				return false;
			}

			/*Caso primero*/
			if (c11.X == c22.X + 1 || c12.X == c21.X -1 || c11.X == c22.X -1 || c12.X == c21.X + 1)
			{
				/*Intersecciones*/
				if (c12.Y >= c21.Y && c12.Y <= c22.Y) // Si la de abajo es menor que la de arriba y mayor que la de abajo
				{
					return true;
				}
				if (c11.Y <= c22.Y && c11.Y >= c21.Y) // Si la de arriba es mayor que la de abajo y menor que la de arriba
				{
					return true;
				}

				if (c22.Y >= c11.Y && c22.Y <= c12.Y) // Si la de abajo es menor que la de arriba y mayor que la de abajo
				{
					return true;
				}
				if (c21.Y <= c12.Y && c21.Y >= c11.Y) // Si la de arriba es mayor que la de abajo y menor que la de arriba
				{
					return true;
				}
			}
			if (c11.Y == c22.Y + 1 || c12.Y == c21.Y - 1 || c11.Y == c22.Y - 1 || c12.Y == c21.Y + 1)
			{
				
				/*Intersecciones 2.0*/
				if (c12.X <= c22.X && c12.X >= c21.X) // Si la de derecha es menor que la derecha y mayor que la izquierda
				{
					return true;
				}
				if (c11.X <= c22.X && c11.X >= c21.X)
				{
					return true;
				}
				if (c21.X <= c12.X && c21.X >= c11.X)
				{
					return true;
				}
				if (c22.X <= c12.X && c22.X >= c11.X)
				{
					return true;
				}
			}

			return false;
		}

		public bool overlap(Provincia provincia)
		{
			/*A lo bruto, si coincide una coordenada es que se superponen*/
			for (int i1 = this.arribaIzquierda.X; i1 < this.abajoDerecha.X ; i1++)
			{
				for (int i2 = provincia.arribaIzquierda.X; i2 < provincia.abajoDerecha.X ; i2++)
				{
					for (int j1 = this.arribaIzquierda.Y; j1 < this.abajoDerecha.Y ; j1++)
					{
						for (int j2 = provincia.arribaIzquierda.Y; j2 < provincia.abajoDerecha.Y ; j2++)
						{
							if (i1 == i2 && j1 == j2)
							{
								return true;
							}
						}
					}
				}
			}
			return false;
		}

		public override string ToString()
		{
			return "[" + this.arribaIzquierda + ", " + this.abajoDerecha + "]";
		}
	}
}
 