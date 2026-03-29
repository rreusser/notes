

// TypeScript declarations for @stdlib/lapack/base/zla_gbrpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor for a complex general banded matrix.
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
* Compute the reciprocal pivot growth factor for a complex general banded matrix.
*/
declare var zla_gbrpvgrw: Routine;

export = zla_gbrpvgrw;
