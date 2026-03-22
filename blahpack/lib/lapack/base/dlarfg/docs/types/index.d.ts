

// TypeScript declarations for @stdlib/lapack/base/dlarfg

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate a real Householder reflector.
	*/
	(
		N: number,
		alpha: number,
		x: Float64Array,
		stride: number,
		offset: number,
		tau: number
	): Float64Array;
}

/**
* Generate a real Householder reflector.
*/
declare var dlarfg: Routine;

export = dlarfg;
