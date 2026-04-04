

// TypeScript declarations for @stdlib/lapack/base/dsbgv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.
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
		offsetWORK: number
	): Float64Array;
}

/**
* Computes all eigenvalues and optionally eigenvectors of a real symmetric-definite banded generalized eigenproblem.
*/
declare var dsbgv: Routine;

export = dsbgv;
