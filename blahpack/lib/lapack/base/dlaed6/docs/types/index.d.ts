

// TypeScript declarations for @stdlib/lapack/base/dlaed6

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute one Newton step in the solution of the secular equation.
	*/
	(
		kniter: number,
		orgati: boolean,
		rho: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		finit: number,
		tau: number
	): Float64Array;
}

/**
* Compute one Newton step in the solution of the secular equation.
*/
declare var dlaed6: Routine;

export = dlaed6;
