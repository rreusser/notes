

// TypeScript declarations for @stdlib/lapack/base/zposv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the solution to a complex system of linear equations A * X = B where A is Hermitian positive definite
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
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
* Compute the solution to a complex system of linear equations A * X = B where A is Hermitian positive definite
*/
declare var zposv: Routine;

export = zposv;
