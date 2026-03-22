

// TypeScript declarations for @stdlib/lapack/base/dlaset

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Initialize a matrix to given diagonal and off-diagonal values
	*/
	(
		uplo: string,
		M: number,
		N: number,
		alpha: number,
		beta: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Initialize a matrix to given diagonal and off-diagonal values
*/
declare var dlaset: Routine;

export = dlaset;
