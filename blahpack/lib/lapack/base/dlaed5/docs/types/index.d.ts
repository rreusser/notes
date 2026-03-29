

// TypeScript declarations for @stdlib/lapack/base/dlaed5

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve the 2-by-2 secular equation.
	*/
	(
		i: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		delta: Float64Array,
		strideDELTA: number,
		offsetDELTA: number,
		rho: number,
		dlam: number
	): Float64Array;
}

/**
* Solve the 2-by-2 secular equation.
*/
declare var dlaed5: Routine;

export = dlaed5;
