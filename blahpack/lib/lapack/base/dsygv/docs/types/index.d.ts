

// TypeScript declarations for @stdlib/lapack/base/dsygv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem
	*/
	(
		itype: number,
		jobz: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem
*/
declare var dsygv: Routine;

export = dsygv;
