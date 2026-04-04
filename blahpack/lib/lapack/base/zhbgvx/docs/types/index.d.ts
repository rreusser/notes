

// TypeScript declarations for @stdlib/lapack/base/zhbgvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.
	*/
	(
		jobz: string,
		range: string,
		uplo: string,
		N: number,
		ka: number,
		kb: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		BB: Float64Array,
		strideBB1: number,
		strideBB2: number,
		offsetBB: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
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
* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.
*/
declare var zhbgvx: Routine;

export = zhbgvx;
