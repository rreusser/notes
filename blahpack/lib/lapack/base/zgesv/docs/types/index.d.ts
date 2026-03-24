

// TypeScript declarations for @stdlib/lapack/base/zgesv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the solution to a complex system of linear equations A * X = B
	*/
	(
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Compute the solution to a complex system of linear equations A * X = B
*/
declare var zgesv: Routine;

export = zgesv;
