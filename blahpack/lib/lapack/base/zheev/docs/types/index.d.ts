

// TypeScript declarations for @stdlib/lapack/base/zheev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute eigenvalues and optionally eigenvectors of a complex Hermitian matrix
	*/
	(
		jobz: string,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
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
* Compute eigenvalues and optionally eigenvectors of a complex Hermitian matrix
*/
declare var zheev: Routine;

export = zheev;
