

// TypeScript declarations for @stdlib/lapack/base/zlacp2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy all or part of a real matrix to a complex matrix
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
* Copy all or part of a real matrix to a complex matrix
*/
declare var zlacp2: Routine;

export = zlacp2;
