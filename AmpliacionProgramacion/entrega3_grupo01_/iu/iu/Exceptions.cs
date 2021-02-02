using System;
using System.Collections.Generic;
using System.Reflection;
//using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace iu
{
	public class OutOfLimitsException : Exception
	{
		public OutOfLimitsException() : base() { }
		public OutOfLimitsException(string message) : base(message) { }
	}

	public class OverlapException : Exception
	{
		public OverlapException() : base() { }
		public OverlapException(string message) : base(message) { }
	}

	public class IncorrectProvincia : Exception
	{
		public IncorrectProvincia() : base() { }
		public IncorrectProvincia(string message) : base(message) { }
	}

	public class RegionException : Exception 
	{
		public RegionException() : base() { }
		public RegionException(string message) : base(message) { }
	}
}