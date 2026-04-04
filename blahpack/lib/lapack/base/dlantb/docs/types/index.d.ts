

// TypeScript declarations for @stdlib/lapack/base/dlantb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a real triangular band matrix.
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		K: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Returns the norm of a real triangular band matrix.
*/
declare var dlantb: Routine;

export = dlantb;
