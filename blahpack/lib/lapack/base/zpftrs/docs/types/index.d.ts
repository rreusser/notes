

// TypeScript declarations for @stdlib/lapack/base/zpftrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex system A * X = B where A is Hermitian positive definite in Rectangular Full Packed format.
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
* Solves a complex system A * X = B where A is Hermitian positive definite in Rectangular Full Packed format.
*/
declare var zpftrs: Routine;

export = zpftrs;
