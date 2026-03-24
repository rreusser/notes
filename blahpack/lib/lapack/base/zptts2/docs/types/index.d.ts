

// TypeScript declarations for @stdlib/lapack/base/zptts2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex Hermitian tridiagonal system using LDL^H factorization
	*/
	(
		iuplo: number,
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
* Solves a complex Hermitian tridiagonal system using LDL^H factorization
*/
declare var zptts2: Routine;

export = zptts2;
