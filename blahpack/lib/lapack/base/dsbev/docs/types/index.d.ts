

// TypeScript declarations for @stdlib/lapack/base/dsbev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a real symmetric band matrix.
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
		offsetWORK: number
	): Float64Array;
}

/**
* Computes all eigenvalues and optionally eigenvectors of a real symmetric band matrix.
*/
declare var dsbev: Routine;

export = dsbev;
