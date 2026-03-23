

// TypeScript declarations for @stdlib/lapack/base/drscl

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Scale a vector by the reciprocal of a scalar
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
* Scale a vector by the reciprocal of a scalar
*/
declare var drscl: Routine;

export = drscl;
