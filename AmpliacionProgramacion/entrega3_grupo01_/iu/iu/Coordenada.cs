using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace iu
{
	public class Coordenada
	{
		/*Definicion*/
		public Coordenada(int x, int y)
		{
			this.X = x;
			this.Y = y;
		}

		/*getters y setters*/
		public int X
		{
			get;
			private set;
		}

		public int Y
		{
			get;
			private set;
		}

		/*sobreescribimos metodo de comparacion*/
		public static bool operator ==(Coordenada a, Coordenada b)
		{
			return (a.X == b.X && a.Y == b.Y);
		}

		public static bool operator !=(Coordenada a, Coordenada b)
		{
			return !(a == b);
		}

		public override bool Equals(object obj)
		{
			return this == (Coordenada)obj; /*No se si es casteo es necesario*/
		}

		public override string ToString()
		{
			return "(" + this.X + ", " + this.Y + ") ";
		}

		public override int GetHashCode()
		{
			return base.GetHashCode() + this.X + this.Y;
		}


	}
}