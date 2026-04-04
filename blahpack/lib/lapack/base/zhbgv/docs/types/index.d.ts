

// TypeScript declarations for @stdlib/lapack/base/zhbgv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.
	*/
	(
		jobz: string,
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
		offsetRWORK: number
	): Float64Array;
}

/**
* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite banded generalized eigenproblem.
*/
declare var zhbgv: Routine;

export = zhbgv;
