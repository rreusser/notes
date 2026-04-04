

// TypeScript declarations for @stdlib/lapack/base/dspev

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes all eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.
	*/
	(
		jobz: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
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
* Computes all eigenvalues and optionally eigenvectors of a real symmetric matrix in packed storage.
*/
declare var dspev: Routine;

export = dspev;
