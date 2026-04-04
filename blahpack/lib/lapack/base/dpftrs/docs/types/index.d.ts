

// TypeScript declarations for @stdlib/lapack/base/dpftrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real system A * X = B where A is symmetric positive definite in Rectangular Full Packed format.
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		nrhs: number,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a real system A * X = B where A is symmetric positive definite in Rectangular Full Packed format.
*/
declare var dpftrs: Routine;

export = dpftrs;
