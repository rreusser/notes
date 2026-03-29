

// TypeScript declarations for @stdlib/lapack/base/dla_gbrpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a general banded matrix.
	*/
	(
		N: number,
		kl: number,
		ku: number,
		ncols: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		AFB: Float64Array,
		strideAFB1: number,
		strideAFB2: number,
		offsetAFB: number
	): Float64Array;
}

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a general banded matrix.
*/
declare var dla_gbrpvgrw: Routine;

export = dla_gbrpvgrw;
