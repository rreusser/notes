

// TypeScript declarations for @stdlib/lapack/base/zhpgvx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
	*/
	(
		itype: number,
		jobz: string,
		range: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		BP: Float64Array,
		strideBP: number,
		offsetBP: number,
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
* Computes selected eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
*/
declare var zhpgvx: Routine;

export = zhpgvx;
