

// TypeScript declarations for @stdlib/lapack/base/zlansb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex symmetric band matrix.
	*/
	(
		norm: string,
		uplo: string,
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
* Returns the norm of a complex symmetric band matrix.
*/
declare var zlansb: Routine;

export = zlansb;
