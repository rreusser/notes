

// TypeScript declarations for @stdlib/lapack/base/dzsum1

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Sum of absolute values of a complex vector
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number,
		incx: number
	): Float64Array;
}

/**
* Sum of absolute values of a complex vector
*/
declare var dzsum1: Routine;

export = dzsum1;
