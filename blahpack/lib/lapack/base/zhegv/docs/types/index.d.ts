

// TypeScript declarations for @stdlib/lapack/base/zhegv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a complex generalized Hermitian-definite eigenproblem
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
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Computes all eigenvalues and optionally eigenvectors of a complex generalized Hermitian-definite eigenproblem
*/
declare var zhegv: Routine;

export = zhegv;
