

// TypeScript declarations for @stdlib/lapack/base/zlacpy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy all or part of a complex matrix
	*/
	(
		uplo: string,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Copy all or part of a complex matrix
*/
declare var zlacpy: Routine;

export = zlacpy;
