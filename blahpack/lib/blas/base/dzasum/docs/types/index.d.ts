

// TypeScript declarations for @stdlib/blas/base/dzasum

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the sum of absolute values of a complex vector
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
* Compute the sum of absolute values of a complex vector
*/
declare var dzasum: Routine;

export = dzasum;
