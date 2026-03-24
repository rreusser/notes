

// TypeScript declarations for @stdlib/lapack/base/zdrscl

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Scale a complex vector by the reciprocal of a real scalar with overflow protection
	*/
	(
		N: number,
		sa: number,
		x: Float64Array,
		stride: number,
		offset: number,
		incx: number
	): Float64Array;
}

/**
* Scale a complex vector by the reciprocal of a real scalar with overflow protection
*/
declare var zdrscl: Routine;

export = zdrscl;
