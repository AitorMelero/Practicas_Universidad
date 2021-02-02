
using System.Collections.Generic;

namespace iu
{
    public abstract class Interfaz
    {
		int filas { get; set; }
		int columnas { get; set; }
		public int x1 { get; set; }
		public int y1 { get; set; }
		public int x2 { get; set; }
		public int y2 { get; set; }
		public Mapa mapa { get; set; }
		public Mosaico mosaico { get; set; }

        public void Interaccionar() 
        {
            do {
				/*Introducir coordenadas*/
				IntroducirCoordenadas();				

				/*Dibujamos el mapa en el mosaico*/
				MostrarMosaico();
            } while (ConfirmarContinuacion());
        }

		/*Funcion predefinida*/
		public void AddProvincia() => mapa.addProvincia(new Provincia(new Coordenada(x1, y1), new Coordenada(x2, y2)));

		/*Funciones abstractas*/
		public abstract void IntroducirCoordenadas();
		public abstract void MostrarMosaico();
        public abstract bool ConfirmarContinuacion();
    }
}
