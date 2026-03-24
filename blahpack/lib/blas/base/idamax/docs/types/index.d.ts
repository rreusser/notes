

// TypeScript declarations for @stdlib/blas/base/idamax

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Find the index of element with maximum absolute value
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Find the index of element with maximum absolute value
*/
declare var idamax: Routine;

export = idamax;
