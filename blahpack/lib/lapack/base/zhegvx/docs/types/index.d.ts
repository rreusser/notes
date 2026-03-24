

// TypeScript declarations for @stdlib/lapack/base/zhegvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues and eigenvectors of a complex generalized Hermitian-definite eigenproblem
	*/
	(
		itype: number,
		jobz: string,
		range: string,
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
		vl: number,
		vu: number,
		il: number,
		iu: number,
		abstol: number,
		M: number,
		w: Float64Array,
		strideW: number,
		offsetW: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number,
		IFAIL: Int32Array,
		strideIFAIL: number,
		offsetIFAIL: number
	): Float64Array;
}

/**
* Computes selected eigenvalues and eigenvectors of a complex generalized Hermitian-definite eigenproblem
*/
declare var zhegvx: Routine;

export = zhegvx;
