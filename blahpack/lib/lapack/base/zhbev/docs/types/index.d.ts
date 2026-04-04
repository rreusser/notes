

// TypeScript declarations for @stdlib/lapack/base/zhbev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.
	*/
	(
		jobz: string,
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
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
* Computes all eigenvalues and optionally eigenvectors of a complex Hermitian band matrix.
*/
declare var zhbev: Routine;

export = zhbev;
