

// TypeScript declarations for @stdlib/blas/base/dasum

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the sum of absolute values of a vector
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Compute the sum of absolute values of a vector
*/
declare var dasum: Routine;

export = dasum;
