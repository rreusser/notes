

// TypeScript declarations for @stdlib/lapack/base/zpttrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex Hermitian positive definite tridiagonal system using LDL^H factorization
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a complex Hermitian positive definite tridiagonal system using LDL^H factorization
*/
declare var zpttrs: Routine;

export = zpttrs;
