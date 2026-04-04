

// TypeScript declarations for @stdlib/lapack/base/zhpgv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
	*/
	(
		itype: number,
		jobz: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		BP: Float64Array,
		strideBP: number,
		offsetBP: number,
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
* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian-definite generalized eigenproblem in packed storage.
*/
declare var zhpgv: Routine;

export = zhpgv;
